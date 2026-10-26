#' Client for the Posit Connect Cloud API
#'
#' Deploying a bundle is done by rsconnect (`deployApp(appId =)`), maintained by
#' Posit for Connect Cloud. This file covers what rsconnect does not provide
#' yet: creating an item with a custom name on a PHS domain, listing domains,
#' finding and reading items, and changing access outside a deploy. Those calls
#' go to the API directly with httr2. Deleting uses rsconnect's own
#' `deleteContent()`.
#'
#' Posit has not published a reference for this API. The request shapes follow
#' rsconnect's internal Cloud client, so must be checked against rsconnect's
#' source for each new minimum version. When rsconnect gains a function for any
#' call defined here, it should be used instead, removing the direct API call,
#' and keeping this file as small as possible.
#'
#' Secret handling: the client secret is sent only in the token request's form
#' body, and the access token only in Authorization headers. No condition raised
#' here carries a request, response, body, header or URL query - failures are
#' raised as new `phsdeploy_*` conditions with a status and a message scrubbed by
#' `scrub_text()`. Records returned to callers have secret-like fields removed by
#' `strip_record()`.
#'
#' Caveat: httr2 keeps the last request in memory (`httr2::last_request()`).
#' After a failed token exchange that is the token request, client secret
#' included. httr2 has no public way to clear it but presents a minimal risk as
#' the secret is in the session's environment variables anyway.
#'
#' @name connect_cloud_api
#' @noRd
NULL

# Constants --------------------------------------------------------------------

#' Base URL of the Connect Cloud API
#'
#' @format A single string.
#' @noRd
cloud_api_base <- "https://api.connect.posit.cloud/v1"

#' URL of the Connect Cloud token exchange
#'
#' @format A single string.
#' @noRd
cloud_token_url <- "https://login.posit.cloud/oauth/token"

#' Scope requested with every access token
#'
#' The scope Connect Cloud's API expects for client-credential tokens.
#'
#' @format A single string.
#' @noRd
cloud_scope <- "vivid"

#' Server name rsconnect uses for Connect Cloud accounts
#'
#' Needed to find the registered account when calling rsconnect functions such
#' as `deleteContent()`.
#'
#' @format A single string.
#' @noRd
cloud_server <- "connect.posit.cloud"

#' User agent sent with every request
#'
#' Identifies this package's traffic in Connect Cloud's logs.
#'
#' @format A single string.
#' @noRd
cloud_user_agent <- "phsdeploy"

#' Records requested per page of a listing
#'
#' @format A single integer.
#' @noRd
cloud_page_limit <- 100L

#' Access values Connect Cloud accepts
#'
#' From most to least restricted. New content is always `"private"`.
#'
#' @format A character vector.
#' @noRd
cloud_access_values <- c(
  "private", "view_team_edit_private", "view_team_edit_team",
  "view_public_edit_team", "public"
)

#' Connect Cloud content type for each app mode
#'
#' Mirrors rsconnect's internal `cloudContentTypeFromAppMode()`.
#'
#' @format A named character vector: app mode = content type.
#' @noRd
cloud_content_types <- c(
  "jupyter-notebook" = "jupyter",
  "python-bokeh" = "bokeh",
  "python-dash" = "dash",
  "python-shiny" = "shiny",
  "shiny" = "shiny",
  "python-streamlit" = "streamlit",
  "quarto" = "quarto",
  "quarto-static" = "quarto",
  "quarto-shiny" = "quarto",
  "rmd-static" = "rmarkdown",
  "rmd-shiny" = "rmarkdown",
  "static" = "static"
)

#' Pattern for error text that means a name is taken
#'
#' Connect Cloud doesn't document its response to a duplicate name. A 409, or
#' a 422 whose message matches this pattern, is treated as "name taken" -
#' anything else stays a plain HTTP error.
#'
#' @format A single regular expression string.
#' @noRd
cloud_name_taken_pattern <- paste(
  "already (been )?(taken|in use|exists|used|registered|assigned)",
  "is taken", "not available", "unavailable", "duplicate",
  sep = "|"
)

#' Fields removed from every record returned by the API
#'
#' A private link token gives access to private content, and `secrets` can
#' hold environment variable values, so neither is kept or shown.
#'
#' @format A character vector.
#' @noRd
cloud_strip_fields <- c("private_link_token", "private_link_token_hash", "secrets")

#' Cache of access tokens
#'
#' Keyed by client id. Held in memory only, never written to disk, so a token
#' is reused until shortly before it expires.
#'
#' @format An environment.
#' @noRd
cloud_tokens <- new.env(parent = emptyenv())

# Token ------------------------------------------------------------------------

#' Get an access token for a Cloud connection
#'
#' Exchanges the connection's client credentials for an access token, and
#' reuses a cached one until 30 seconds before it expires.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param call The environment of the calling function, shown in errors.
#' @return The access token (a string). Never print it. Otherwise an error of
#'   class `phsdeploy_cloud_token_error`, with `status`.
#' @noRd
cloud_token <- function(cld, call = rlang::caller_env()) {
  cached <- cloud_tokens[[cld$client_id]]
  if (!is.null(cached) && !cloud_token_expired(cached)) {
    return(cached$access_token)
  }

  req <- httr2::request(cloud_token_url)
  req <- httr2::req_body_form(
    req,
    grant_type = "client_credentials",
    client_id = cld$client_id,
    client_secret = cld$client_secret,
    scope = cloud_scope
  )
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_user_agent(req, cloud_user_agent)
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  resp <- tryCatch(cloud_perform(req), error = function(e) e)
  status <- NA_integer_
  token <- NULL
  if (!inherits(resp, "error")) {
    status <- httr2::resp_status(resp)
    body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
    if (status < 300L && is.list(body)) token <- body
  }
  if (is.null(token) || !rlang::is_string(token$access_token)) {
    detail <- if (inherits(resp, "error")) {
      cloud_failure_detail(resp, cloud_secrets(cld))
    } else {
      cloud_error_detail(resp, cloud_secrets(cld))
    }
    cli::cli_abort(
      c("Could not get a Posit Connect Cloud access token.", "x" = "{detail}"),
      class = c("phsdeploy_cloud_token_error", "phsdeploy_error"),
      status = if (!is.na(status) && status < 300L) NA_integer_ else status,
      call = call
    )
  }

  expires_in <- token$expires_in
  entry <- list(
    access_token = token$access_token,
    expires_at = if (is.numeric(expires_in) && length(expires_in) == 1L) Sys.time() + expires_in
  )
  assign(cld$client_id, entry, envir = cloud_tokens)
  entry$access_token
}

#' Expired cache token check
#'
#' Treats a token as expired 30 seconds early, so it doesn't run out part-way
#' through a request. A token without an expiry time never expires here - a 401
#' still replaces it (see `cloud_call()`).
#'
#' @param entry A cache entry with `access_token` and `expires_at`.
#' @return `TRUE` or `FALSE`.
#' @noRd
cloud_token_expired <- function(entry) {
  !is.null(entry$expires_at) && Sys.time() > entry$expires_at - 30
}

#' Forget cached access tokens
#'
#' @param cld A `phsdeploy_cld` to forget the token of, or `NULL` for all.
#' @return `NULL`, invisibly.
#' @noRd
cloud_token_cache_clear <- function(cld = NULL) {
  cached <- ls(cloud_tokens, all.names = TRUE)
  keys <- if (is.null(cld)) cached else cld$client_id
  rm(list = intersect(keys, cached), envir = cloud_tokens)
  invisible()
}

#' Secrets to redact from server text
#'
#' @param cld A `phsdeploy_cld` connection.
#' @return The client secret and, if one is cached, the access token.
#' @noRd
cloud_secrets <- function(cld) {
  c(cld$client_secret, cloud_tokens[[cld$client_id]]$access_token)
}

# Requests and errors ----------------------------------------------------------

#' Perform a request quietly
#'
#' Verbosity is forced to 0, so a user's `httr2_verbosity` option can never
#' print the token request's body (the client secret) or a response body (the
#' access token).
#'
#' @param req An httr2 request.
#' @return The httr2 response.
#' @noRd
cloud_perform <- function(req) {
  httr2::req_perform(req, verbosity = 0L)
}

#' Build an authenticated Connect Cloud API request
#'
#' Gets (or reuses) the access token, so building a request may make the token
#' request. Retries up to 3 times on a 429 or 5xx response, and on a network
#' failure with no response at all (such as a dropped or stale connection).
#' A `POST` is retried only on 429, so a create that may have reached the
#' server is never repeated. HTTP errors are not raised by httr2, because its
#' errors carry the request - `cloud_call()` raises its own instead.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param path API path below `cloud_api_base`, starting with `/`, without a
#'   query.
#' @param method HTTP method.
#' @param call The environment of the calling function, shown in errors.
#' @return An httr2 request.
#' @noRd
cloud_request <- function(cld, path, method = "GET", call = rlang::caller_env()) {
  check_cld(cld, call = call)
  token <- cloud_token(cld, call = call)
  req <- httr2::request(paste0(cloud_api_base, path))
  req <- httr2::req_method(req, method)
  req <- httr2::req_auth_bearer_token(req, token)
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_user_agent(req, cloud_user_agent)
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_retry(
    req,
    max_tries = 3,
    retry_on_failure = toupper(method) != "POST",
    is_transient = cloud_is_transient(method)
  )
  httr2::req_error(req, is_error = function(resp) FALSE)
}

#' Which responses are worth retrying
#'
#' @param method The request's HTTP method.
#' @return A function of an httr2 response, returning `TRUE` for a 429, or for
#'   a 5xx unless `method` is `POST`.
#' @noRd
cloud_is_transient <- function(method) {
  force(method)
  function(resp) {
    status <- httr2::resp_status(resp)
    status == 429L || (status >= 500L && toupper(method) != "POST")
  }
}

#' Perform a Connect Cloud API call
#'
#' A 401 forgets the cached token and tries once more with a new one, in case
#' the token was revoked before its expiry time.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param method,path As for `cloud_request()`.
#' @param query A named list of query parameters (URL-encoded by httr2).
#' @param body A list sent as JSON, `NULL` fields are dropped.
#' @param allow Statuses of 300 or above to return instead of raising an error.
#' @param error_class `NULL`, or a function of `status` and `detail` returning
#'   extra classes for the error.
#' @param error_fields Extra fields for the error condition.
#' @param unreached_hint Extra cli bullets for an error raised when no response
#'   came back at all (a timeout or network failure), since the server may
#'   still have acted on the request.
#' @param call The environment of the calling function, shown in errors.
#' @return The httr2 response. Otherwise an error of class
#'   `phsdeploy_cloud_http_error`, with `status` (`NA` if the server wasn't
#'   reached).
#' @noRd
cloud_call <- function(cld, method, path, query = list(), body = NULL,
                       allow = integer(), error_class = NULL,
                       error_fields = list(), unreached_hint = NULL,
                       call = rlang::caller_env()) {
  for (attempt in 1:2) {
    req <- cloud_request(cld, path, method = method, call = call)
    if (length(query)) req <- httr2::req_url_query(req, !!!query)
    if (!is.null(body)) req <- httr2::req_body_json(req, drop_null(body), auto_unbox = TRUE)
    resp <- tryCatch(cloud_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      detail <- cloud_failure_detail(resp, cloud_secrets(cld))
      abort_cloud_http(NA_integer_, detail, method, path,
        fields = error_fields, hint = unreached_hint, call = call
      )
    }
    status <- httr2::resp_status(resp)
    if (status != 401L || attempt == 2L) break
    cloud_token_cache_clear(cld)
  }
  if (status < 300L || status %in% allow) {
    return(resp)
  }
  detail <- cloud_error_detail(resp, cloud_secrets(cld))
  extra <- if (is.function(error_class)) error_class(status, detail)
  abort_cloud_http(status, detail, method, path,
    class = extra,
    fields = error_fields, call = call
  )
}

#' Raise an error for a failed Connect Cloud API call
#'
#' @param status The HTTP status, or `NA` if the server wasn't reached.
#' @param detail Scrubbed error text, from `cloud_error_detail()` or
#'   `cloud_failure_detail()`.
#' @param method,path The request's method and path, shown in the message.
#' @param class Extra classes, such as `"phsdeploy_name_taken"`.
#' @param fields Extra fields for the condition. Also available to `hint` as
#'   `fields`.
#' @param hint Extra cli bullets.
#' @param call The environment of the calling function, shown in the error.
#' @return Does not return, raises an error of class
#'   `phsdeploy_cloud_http_error`.
#' @noRd
abort_cloud_http <- function(status, detail, method, path, class = NULL,
                             fields = list(), hint = NULL,
                             call = rlang::caller_env()) {
  what <- paste(method, scrub_text(path))
  head <- if (is.na(status)) {
    "Could not reach Posit Connect Cloud ({.code {what}})."
  } else {
    "Posit Connect Cloud request {.code {what}} failed with HTTP {status}."
  }
  msg <- c(head, "x" = "{detail}")
  if ("phsdeploy_name_taken" %in% class) {
    msg <- c(msg, "i" = "The name {.val {fields$name}} is already in use on that domain.")
  }
  msg <- c(msg, hint)
  rlang::exec(
    cli::cli_abort,
    msg,
    class = c(class, "phsdeploy_cloud_http_error", "phsdeploy_error"),
    status = as.integer(status),
    !!!fields,
    call = call
  )
}

#' The API's own error text
#'
#' Takes the first non-empty `error_description`, `error`, `message` or
#' `detail` field, never the whole body, and scrubs it.
#'
#' @param resp An httr2 response.
#' @param secrets Strings to redact.
#' @return A single string.
#' @noRd
cloud_error_detail <- function(resp, secrets) {
  body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(body)) {
    for (field in c("error_description", "error", "message", "detail")) {
      if (rlang::is_string(body[[field]]) && nzchar(body[[field]])) {
        return(squish_scrub(body[[field]], secrets))
      }
    }
  }
  "The server gave no error text."
}

#' Text of a request that couldn't be performed at all
#'
#' For a network failure or timeout, where there is no response to read.
#'
#' @param e The condition raised by httr2.
#' @param secrets Strings to redact.
#' @return A single string.
#' @noRd
cloud_failure_detail <- function(e, secrets) {
  text <- tryCatch(rlang::cnd_message(e), error = function(.) class(e)[[1]])
  squish_scrub(text, secrets)
}

#' Tidy and scrub error text
#'
#' Removes colour codes and runs of whitespace, then scrubs, truncates to 500
#' characters and scrubs again, so truncation can't expose part of a secret.
#'
#' @param text A character vector.
#' @param secrets Strings to redact.
#' @return A single string.
#' @noRd
squish_scrub <- function(text, secrets) {
  text <- gsub("\033\\[[0-9;]*m", "", paste(text, collapse = " "))
  text <- trimws(gsub("[[:space:]]+", " ", text))
  scrub_text(substr(scrub_text(text, secrets), 1, 500), secrets)
}

#' Parse a JSON response body
#'
#' @param resp An httr2 response.
#' @return The body as a list, with JSON arrays kept as lists.
#' @noRd
cloud_json <- function(resp) {
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Get every page of a listing
#'
#' Sends `include_total=true` and `limit` with each request (a caller's own
#' values win) and pages by `offset`. Stops when `offset` reaches a reported
#' `total`, when a page is empty, or when a page adds no new `id`, so it can't
#' loop if the server ignores `offset`. Records are de-duplicated by `id`.
#'
#' Reaching `max_pages` first is an error, never a partial result, because
#' callers such as `cloud_name_in_use()` must not miss a record on a page that
#' wasn't read.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param path API path, as for `cloud_request()`.
#' @param query A named list of query parameters.
#' @param max_pages The most pages to read.
#' @param call The environment of the calling function, shown in errors.
#' @return A list of records. Otherwise an error of class
#'   `phsdeploy_cloud_paging_error`.
#' @noRd
cloud_get_pages <- function(cld, path, query = list(), max_pages = 1000L,
                            call = rlang::caller_env()) {
  defaults <- list(include_total = "true", limit = cloud_page_limit)
  query <- c(query, defaults[setdiff(names(defaults), names(query))])
  records <- list()
  seen <- character()
  offset <- 0L
  for (page in seq_len(max_pages)) {
    resp <- cloud_call(cld, "GET", path, query = c(query, list(offset = offset)), call = call)
    body <- cloud_json(resp)
    data <- if (is.list(body$data)) body$data else list()
    ids <- pluck_chr(data, "id")
    new <- is.na(ids) | !(ids %in% seen) & !duplicated(ids)
    records <- c(records, data[new])
    seen <- c(seen, ids[new & !is.na(ids)])
    offset <- offset + length(data)
    total <- body$total
    known_total <- is.numeric(total) && length(total) == 1L && !is.na(total)
    if (!length(data) || !any(new & !is.na(ids)) || (known_total && offset >= total)) {
      return(records)
    }
  }
  cli::cli_abort(
    c(
      "Posit Connect Cloud listing {.code {path}} did not end after {max_pages} pages.",
      "x" = "Nothing was decided on a partial listing."
    ),
    class = c("phsdeploy_cloud_paging_error", "phsdeploy_error"),
    call = call
  )
}

# Account ----------------------------------------------------------------------

#' Look up the Cloud account id
#'
#' Lists the accounts the credentials can see and returns the id of the one
#' whose `name` is exactly `cld$account`.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @return The account id (a string). Otherwise an error of class
#'   `phsdeploy_cloud_account_not_found`.
#' @noRd
cloud_account_id <- function(cld) {
  check_cld(cld)
  records <- cloud_get_pages(cld, "/accounts", query = list(has_user_role = "true"))
  ids <- unique(pluck_chr(records, "id")[pluck_chr(records, "name") %in% cld$account])
  ids <- ids[!is.na(ids)]
  if (length(ids) != 1L) {
    cli::cli_abort(
      c(
        "Can't find exactly one Posit Connect Cloud account named {.val {cld$account}}.",
        "i" = "Accounts found with that name: {length(ids)}."
      ),
      class = c("phsdeploy_cloud_account_not_found", "phsdeploy_error")
    )
  }
  ids
}

#' The connection's account id
#'
#' @param cld A `phsdeploy_cld` connection.
#' @return `cld$account_id` if set, otherwise the id looked up with
#'   `cloud_account_id()`.
#' @noRd
cloud_account_of <- function(cld) {
  if (rlang::is_string(cld$account_id) && nzchar(cld$account_id)) {
    cld$account_id
  } else {
    cloud_account_id(cld)
  }
}

# Content ----------------------------------------------------------------------

#' Get one content item
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param id Content id.
#' @return The item as a list, with secret-like fields removed, or `NULL` if
#'   Cloud answers 404 or the item is pending deletion (`state` `"deleted"`).
#' @noRd
cloud_get_content <- function(cld, id) {
  check_cld(cld)
  check_content_id(id)
  resp <- cloud_call(cld, "GET", paste0("/contents/", id), allow = 404L)
  if (httr2::resp_status(resp) == 404L) {
    return(NULL)
  }
  item <- strip_record(cloud_json(resp))
  if (identical(item$state, "deleted")) NULL else item
}

#' Find content by exact title
#'
#' Cloud content has no separate name field, an item is identified by its
#' `title`. The API's `search` matches loosely, so results are filtered to an
#' exact match.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param title The title to match.
#' @return A data frame as from `content_frame()`, with zero rows if none
#'   match.
#' @noRd
cloud_find_content <- function(cld, title) {
  check_cld(cld)
  check_string_arg(title, "title")
  records <- cloud_get_pages(cld, "/contents", query = list(
    account_id = cloud_account_of(cld), search = title
  ))
  out <- content_frame(records)
  out <- out[!is.na(out$title) & out$title == title, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Is a custom name already used on a domain?
#'
#' Names belong to a domain, so this lists all of the account's content and
#' looks for an item with this name (ignoring case) on this domain. The same
#' name on another domain doesn't count, and nor does an item pending
#' deletion.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param name The custom name.
#' @param domain_id The domain's id.
#' @return `TRUE` or `FALSE`.
#' @noRd
cloud_name_in_use <- function(cld, name, domain_id) {
  check_cld(cld)
  check_string_arg(name, "name")
  check_domain_id(domain_id)
  found <- cloud_account_contents(cld)
  any(!is.na(found$vanity_name) & tolower(found$vanity_name) == tolower(name) &
    !is.na(found$domain_id) & found$domain_id == domain_id)
}

#' Ids of the content on a domain
#'
#' Used to check a fixed domain (which holds only one item) is free.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param domain_id The domain's id.
#' @return A character vector of content ids, possibly empty.
#' @noRd
cloud_contents_on_domain <- function(cld, domain_id) {
  check_cld(cld)
  check_domain_id(domain_id)
  found <- cloud_account_contents(cld)
  found$id[!is.na(found$domain_id) & found$domain_id == domain_id & !is.na(found$id)]
}

#' All of the account's live content
#'
#' @param cld A `phsdeploy_cld` connection.
#' @return A data frame as from `content_frame()`.
#' @noRd
cloud_account_contents <- function(cld) {
  records <- cloud_get_pages(cld, "/contents", query = list(account_id = cloud_account_of(cld)))
  content_frame(records)
}

#' Content records as a data frame
#'
#' One row per record, with items pending deletion dropped.
#'
#' @param records A list of content records from the API.
#' @return A data frame with `id`, `title`, `access`, `vanity_name`,
#'   `domain_id` and `state`, all character, with `NA` for missing values.
#' @noRd
content_frame <- function(records) {
  records <- lapply(records, strip_record)
  out <- data.frame(
    id = pluck_chr(records, "id"),
    title = pluck_chr(records, "title"),
    access = pluck_chr(records, "access"),
    vanity_name = pluck_chr(records, "vanity_name"),
    domain_id = pluck_chr(records, "domain_id"),
    state = pluck_chr(records, "state"),
    stringsAsFactors = FALSE
  )
  out <- out[is.na(out$state) | out$state != "deleted", , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Create a content item
#'
#' Creates an empty item ready for `rsconnect::deployApp(appId =)`, optionally
#' with a custom name on a domain. rsconnect can't set a name or domain, which
#' is why this is a direct call. Cloud only accepts a name together with a
#' domain, so a name without a domain is refused before any request. New
#' content is private.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param title The item's title.
#' @param app_mode An rsconnect app mode, such as `"shiny"` or
#'   `"quarto-static"`.
#' @param primary_file The bundle's entry point, such as `"app.R"`.
#' @param domain_id,vanity_name Optional domain id and custom name.
#' @return The created item as a list, with secret-like fields removed.
#'   Otherwise an error of class `phsdeploy_bad_name`, `phsdeploy_bad_app_mode`,
#'   `phsdeploy_name_taken` (with `name` and `domain_id`) or
#'   `phsdeploy_cloud_http_error`.
#' @noRd
cloud_create_content <- function(cld, title, app_mode, primary_file,
                                 domain_id = NULL, vanity_name = NULL) {
  check_cld(cld)
  check_string_arg(title, "title")
  check_string_arg(primary_file, "primary_file")
  if (!is.null(vanity_name)) check_vanity_name(vanity_name, "vanity_name")
  if (!is.null(domain_id)) check_domain_id(domain_id)
  if (!is.null(vanity_name) && is.null(domain_id)) {
    cli::cli_abort(
      c(
        "A custom name needs a domain.",
        "x" = "Posit Connect Cloud only accepts {.field vanity_name} with {.field domain_id}."
      ),
      class = c("phsdeploy_bad_name", "phsdeploy_error"),
      name = vanity_name
    )
  }
  if (!rlang::is_string(app_mode) || !app_mode %in% names(cloud_content_types)) {
    cli::cli_abort(
      c(
        "Posit Connect Cloud does not support app mode {.val {app_mode}}.",
        "i" = "Supported: {.val {names(cloud_content_types)}}."
      ),
      class = c("phsdeploy_bad_app_mode", "phsdeploy_error")
    )
  }

  body <- list(
    account_id = cloud_account_of(cld),
    title = title,
    next_revision = list(
      source_type = "bundle",
      content_type = unname(cloud_content_types[[app_mode]]),
      app_mode = app_mode,
      primary_file = primary_file
    ),
    secrets = list(),
    vanity_name = vanity_name,
    domain_id = domain_id
  )
  resp <- cloud_call(
    cld, "POST", "/contents",
    body = body,
    error_class = if (!is.null(vanity_name)) name_taken_class,
    error_fields = list(name = vanity_name, domain_id = domain_id, title = title),
    unreached_hint = c(
      "!" = "No response came back, so the item may have been created anyway.",
      "i" = paste(
        "Before retrying, check the Connect Cloud dashboard for an item titled",
        "{.val {fields$title}}. If it's there, use it (or delete it) rather than",
        "creating another."
      )
    )
  )
  strip_record(cloud_json(resp))
}

#' Extra error class for a taken name
#'
#' @param status The HTTP status.
#' @param detail The scrubbed error text.
#' @return `"phsdeploy_name_taken"` for a 409, or a 422 whose text matches
#'   `cloud_name_taken_pattern`, otherwise `NULL`.
#' @noRd
name_taken_class <- function(status, detail) {
  taken <- status == 409L ||
    (status == 422L && grepl(cloud_name_taken_pattern, detail, ignore.case = TRUE))
  if (taken) "phsdeploy_name_taken"
}

#' Set who can see a content item
#'
#' Content deployed to Connect Cloud defaults to private, and a deployment doesn't
#' change this. The rsconnect `deployApp(appVisibility =)` only applies during a
#' deploy, which is why this direct call is required.This means setting a product
#' to be public is its own step, separate from the deployment process. A product
#' that needs to be public at release time must invoke this method immediately after
#' deployment, otherwise changing from private to public can be handled on its own
#' schedule.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param id Content id.
#' @param access One of `cloud_access_values`.
#' @return `TRUE`, invisibly. Otherwise an error of class `phsdeploy_bad_access`
#'   (before any request) or `phsdeploy_cloud_http_error`.
#' @noRd
cloud_set_access <- function(cld, id, access) {
  check_cld(cld)
  check_content_id(id)
  if (!rlang::is_string(access) || !access %in% cloud_access_values) {
    cli::cli_abort(
      "{.arg access} must be one of {.val {cloud_access_values}}.",
      class = c("phsdeploy_bad_access", "phsdeploy_error")
    )
  }
  cloud_call(cld, "PATCH", paste0("/contents/", id), body = list(access = access))
  invisible(TRUE)
}

#' Delete a content item
#'
#' Uses `rsconnect::deleteContent()`, with `force = TRUE` so it doesn't ask for
#' confirmation. It signs in with the account registered by the Cloud
#' connection, so it must run inside the same `local_rsconnect_config()` scope.
#' An item that is already gone counts as success and is reported with a
#' message of class `phsdeploy_already_deleted`.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param id Content id.
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_cloud_delete_error`.
#' @noRd
cloud_delete_content <- function(cld, id) {
  check_cld(cld)
  check_content_id(id)
  local_rsconnect_http()
  result <- tryCatch(
    {
      suppressMessages(rsconnect::deleteContent(
        contentId = id, account = cld$account, server = cloud_server, force = TRUE
      ))
      "deleted"
    },
    error = function(e) if (rlang::cnd_inherits(e, "rsconnect_http_404")) "gone" else e
  )
  if (inherits(result, "error")) {
    detail <- squish_scrub(conditionMessage(result), cloud_secrets(cld))
    cli::cli_abort(
      c("Could not delete content {.val {id}} from Posit Connect Cloud.", "x" = "{detail}"),
      class = c("phsdeploy_cloud_delete_error", "phsdeploy_error")
    )
  }
  if (identical(result, "gone")) {
    cli::cli_inform(
      "Content {.val {id}} was already gone from Posit Connect Cloud.",
      class = "phsdeploy_already_deleted"
    )
  }
  invisible(TRUE)
}

# Domains ----------------------------------------------------------------------

#' List the account's custom domains
#'
#' `name` is the hostname, without any `*.` prefix. A wildcard domain and a
#' fixed domain can share a `name`, so always identify a domain by `id`, or by
#' `name` together with `wildcard`.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @return A data frame with `id`, `name`, `status` (character), `wildcard`
#'   (logical, `NA` if missing) and `content_count` (integer).
#' @noRd
cloud_list_domains <- function(cld) {
  check_cld(cld)
  records <- cloud_get_pages(cld, "/domains", query = list(account_id = cloud_account_of(cld)))
  data.frame(
    id = pluck_chr(records, "id"),
    name = pluck_chr(records, "name"),
    wildcard = vapply(records, function(r) {
      v <- r[["wildcard"]]
      if (rlang::is_bool(v)) v else NA
    }, logical(1)),
    status = pluck_chr(records, "status"),
    content_count = vapply(records, function(r) {
      v <- r[["content_count"]]
      if (is.numeric(v) && length(v) == 1L) as.integer(v) else NA_integer_
    }, integer(1)),
    stringsAsFactors = FALSE
  )
}

#' Public address of an item on a custom domain
#'
#' On a wildcard domain the address is `https://<name>.<hostname>`, on a fixed
#' domain it is `https://<hostname>` and `name` isn't used.
#'
#' @param domain One domain, with `name` and `wildcard`: a one-row data frame
#'   from `cloud_list_domains()`, or a list.
#' @param name The item's custom name, for a wildcard domain.
#' @return The address (a string). Otherwise an error of class
#'   `phsdeploy_bad_domain` or `phsdeploy_bad_name`.
#' @noRd
cloud_public_url <- function(domain, name = NULL) {
  if (is.data.frame(domain)) {
    if (nrow(domain) != 1L) {
      cli::cli_abort(
        "{.arg domain} must be exactly one domain, not {nrow(domain)}.",
        class = c("phsdeploy_bad_domain", "phsdeploy_error")
      )
    }
    domain <- as.list(domain)
  }
  if (!rlang::is_string(domain$name) || !nzchar(domain$name) || !rlang::is_bool(domain$wildcard)) {
    cli::cli_abort(
      "{.arg domain} needs a hostname {.field name} and a TRUE/FALSE {.field wildcard}.",
      class = c("phsdeploy_bad_domain", "phsdeploy_error")
    )
  }
  host <- tolower(sub("\\.$", "", sub("^\\*\\.", "", trimws(domain$name))))
  if (!domain$wildcard) {
    return(paste0("https://", host))
  }
  check_vanity_name(name, "name")
  paste0("https://", name, ".", host)
}

# Checks and helpers -----------------------------------------------------------

#' Pattern for a lowercase DNS label
#'
#' Lowercase letters, digits and hyphens, not starting or ending with a hyphen.
#' Length is checked separately.
#'
#' @format A single regular expression string.
#' @noRd
dns_label_pattern <- "^[a-z0-9]([a-z0-9-]*[a-z0-9])?$"

#' Check a custom name is a lowercase DNS label
#'
#' A name becomes part of a web address, so it must be a valid DNS label: at
#' most 63 characters, from `dns_label_pattern`.
#'
#' @param name The value to check.
#' @param arg The argument name used in the error message.
#' @param call The environment of the calling function, shown in the error.
#' @return `name`, invisibly. Otherwise an error of class `phsdeploy_bad_name`.
#' @noRd
check_vanity_name <- function(name, arg = "name", call = rlang::caller_env()) {
  if (!rlang::is_string(name) || nchar(name) > 63L || !grepl(dns_label_pattern, name)) {
    cli::cli_abort(
      "{.arg {arg}} must be a lowercase DNS label (letters, digits and hyphens, at most 63).",
      class = c("phsdeploy_bad_name", "phsdeploy_error"),
      name = if (rlang::is_string(name)) name,
      call = call
    )
  }
  invisible(name)
}

#' Check a Cloud connection
#'
#' @param cld The value to check.
#' @param call The environment of the calling function, shown in the error.
#' @return `cld`, invisibly, otherwise an error of class `phsdeploy_bad_cld`.
#' @noRd
check_cld <- function(cld, call = rlang::caller_env()) {
  if (!inherits(cld, "phsdeploy_cld") || !rlang::is_string(cld$client_id) ||
    !rlang::is_string(cld$client_secret)) {
    cli::cli_abort(
      "{.arg cld} must be a Posit Connect Cloud connection.",
      class = c("phsdeploy_bad_cld", "phsdeploy_error"),
      call = call
    )
  }
  invisible(cld)
}

#' Check a Cloud content id
#'
#' The id is pasted into API paths, so anything other than one id made of
#' letters, digits, `-` and `_` is refused before a request is made.
#'
#' @param id The value to check.
#' @param call The environment of the calling function, shown in the error.
#' @return `id`, invisibly. Otherwise an error of class
#'   `phsdeploy_bad_content_id`.
#' @noRd
check_content_id <- function(id, call = rlang::caller_env()) {
  if (!rlang::is_string(id) || !grepl("^[A-Za-z0-9_-]+$", id)) {
    cli::cli_abort(
      "{.arg id} must be one Connect Cloud content id (letters, digits, {.val -} and {.val _}).",
      class = c("phsdeploy_bad_content_id", "phsdeploy_error"),
      call = call
    )
  }
  invisible(id)
}

#' Check a Cloud domain id
#'
#' @param domain_id The value to check.
#' @param call The environment of the calling function, shown in the error.
#' @return `domain_id`, invisibly, otherwise an error of class
#'   `phsdeploy_bad_domain`.
#' @noRd
check_domain_id <- function(domain_id, call = rlang::caller_env()) {
  if (!rlang::is_string(domain_id) || !nzchar(domain_id)) {
    cli::cli_abort(
      "{.arg domain_id} must be one Connect Cloud domain id.",
      class = c("phsdeploy_bad_domain", "phsdeploy_error"),
      call = call
    )
  }
  invisible(domain_id)
}

#' Check an argument is one non-empty string
#'
#' @param x The value to check.
#' @param arg The argument name used in the error message.
#' @param call The environment of the calling function, shown in the error.
#' @return `x`, invisibly, otherwise an error of class
#'   `phsdeploy_bad_argument`.
#' @noRd
check_string_arg <- function(x, arg, call = rlang::caller_env()) {
  if (!rlang::is_string(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single non-empty string.",
      class = c("phsdeploy_bad_argument", "phsdeploy_error"),
      call = call
    )
  }
  invisible(x)
}

#' Remove secret-like fields from an API record
#'
#' Drops `cloud_strip_fields` at every level of a parsed record.
#'
#' @param x A parsed record. Anything other than a list is returned unchanged.
#' @return `x` without those fields.
#' @noRd
strip_record <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  if (!is.null(names(x))) x <- x[!names(x) %in% cloud_strip_fields]
  x[] <- lapply(x, strip_record)
  x
}

#' Drop `NULL` fields from a list
#'
#' So optional fields are left out of a JSON body rather than sent as `null`.
#'
#' @param x A list.
#' @return `x` without its `NULL` elements.
#' @noRd
drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' One text field from each record
#'
#' @param records A list of records.
#' @param field The field name.
#' @return A character vector, one value per record, `NA` where the field is
#'   missing or not one string.
#' @noRd
pluck_chr <- function(records, field) {
  vapply(records, function(r) {
    v <- r[[field]]
    if (rlang::is_string(v)) v else NA_character_
  }, character(1))
}
