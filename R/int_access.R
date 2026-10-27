#' Internal Connect: content listing, access, groups and tags
#'
#' These go to the internal Connect API directly with httr2, using the server
#' and key of a connectapi client. Any client works, including one made from a
#' visitor's session token in a Shiny app (`connectapi::connect(token = ...)`),
#' so each call acts with that person's permissions.
#'
#' Connect calls a collaborator `owner` in its access API. These functions say
#' `collaborator` to their callers and translate.
#'
#' @name int_access
#' @noRd
NULL

# Content ----------------------------------------------------------------------

#' List internal content
#'
#' Lists every item the client's key can see on internal Connect. A
#' publisher's key sees what they own, collaborate on or can view. An
#' administrator's key also lists items they have no access to.
#'
#' @param client A connectapi client for internal Connect, by default
#'   `phs_connect_int()`.
#'
#' @return A data frame with one row per item, all columns character:
#'   `guid`, `name`, `title`, `owner_guid`, `app_mode`, `app_role` (your
#'   relationship to the item: `owner`, `editor`, `viewer` or `none`),
#'   `access_type`, `bundle_id` (`NA` if never deployed),
#'   `last_deployed_time`, `content_url` and `dashboard_url`.
#'
#'   Otherwise an error of class `phsdeploy_int_http_error`.
#' @export
#' @examples
#' \dontrun{
#' items <- list_int_content()
#' items[items$app_role == "owner", c("title", "guid")]
#' }
list_int_content <- function(client = phs_connect_int()) {
  records <- int_call(client, "GET", "v1/content")
  fields <- c(
    "guid", "name", "title", "owner_guid", "app_mode", "app_role", "access_type",
    "bundle_id", "last_deployed_time", "content_url", "dashboard_url"
  )
  int_frame(records, fields)
}

# Access -----------------------------------------------------------------------

#' Who has access to an internal item
#'
#' @inheritParams list_int_content
#' @param guid The internal content's GUID.
#'
#' @return A data frame with one row per user or group on the item's access
#'   list: `permission_id`, `principal_guid`, `principal_type` (`user` or
#'   `group`) and `role` (`collaborator` or `viewer`). The item's owner isn't
#'   listed.
#'
#'   Otherwise an error of class `phsdeploy_int_http_error`.
#' @export
int_list_access <- function(client = phs_connect_int(), guid) {
  check_guid(guid)
  records <- int_call(client, "GET", paste0("v1/content/", guid, "/permissions"))
  out <- int_frame(records, c("id", "principal_guid", "principal_type", "role"))
  names(out)[names(out) == "id"] <- "permission_id"
  out$role <- int_role_out(out$role)
  out
}

#' Give a group access to an internal item
#'
#' Adds the group to the item's access list, or changes its role if it is
#' already there. A group must be a collaborator for its members to download
#' the item's bundles, which deploying to Connect Cloud needs: a viewer can't,
#' even an administrator.
#'
#' @inheritParams int_list_access
#' @param group_guid The group's GUID, from [int_find_group()].
#' @param role `"collaborator"` or `"viewer"`.
#'
#' @return A list with `permission_id` and `changed` (`FALSE` if the group
#'   already had that role), invisibly. Otherwise an error of class
#'   `phsdeploy_int_http_error`.
#' @export
#' @examples
#' \dontrun{
#' group <- int_find_group(name = "PHS-PositConnect-PROD-Admin")
#' int_add_group_access(guid = guid, group_guid = group$guid)
#' }
int_add_group_access <- function(client = phs_connect_int(), guid, group_guid,
                                 role = c("collaborator", "viewer")) {
  check_guid(guid)
  check_guid(group_guid)
  role <- rlang::arg_match(role)
  current <- int_list_access(client, guid)
  mine <- current[current$principal_guid %in% group_guid, , drop = FALSE]
  body <- list(principal_guid = group_guid, principal_type = "group", role = int_role_in(role))
  if (nrow(mine) && identical(mine$role[[1]], role)) {
    return(invisible(list(permission_id = mine$permission_id[[1]], changed = FALSE)))
  }
  path <- paste0("v1/content/", guid, "/permissions")
  record <- if (nrow(mine)) {
    int_call(client, "PUT", paste0(path, "/", mine$permission_id[[1]]), body = body)
  } else {
    int_call(client, "POST", path, body = body)
  }
  invisible(list(permission_id = flat_chr(record$id), changed = TRUE))
}

#' Remove a user or group from an internal item's access list
#'
#' An entry that is already gone counts as success.
#'
#' @inheritParams int_list_access
#' @param permission_id The entry's `permission_id`, from [int_list_access()].
#'
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_int_http_error`.
#' @export
int_remove_access <- function(client = phs_connect_int(), guid, permission_id) {
  check_guid(guid)
  permission_id <- check_int_id(permission_id, "permission_id")
  int_call(client, "DELETE", paste0("v1/content/", guid, "/permissions/", permission_id), allow = 404L)
  invisible(TRUE)
}

# Groups -----------------------------------------------------------------------

#' Find an internal Connect group by name
#'
#' Groups on PHS Connect come from SAML, so their members are managed there,
#' not through Connect.
#'
#' @inheritParams list_int_content
#' @param name The group's exact name.
#'
#' @return A list with `guid` and `name`. Otherwise an error of class
#'   `phsdeploy_group_not_found` or `phsdeploy_int_http_error`.
#' @export
int_find_group <- function(client = phs_connect_int(), name) {
  check_string_arg(name, "name")
  records <- int_call(client, "GET", "v1/groups", query = list(prefix = name, page_size = 500))
  groups <- int_frame(records, c("guid", "name"))
  found <- groups[groups$name %in% name, , drop = FALSE]
  if (nrow(found) != 1L) {
    cli::cli_abort(
      "Can't find exactly one internal Connect group named {.val {name}}.",
      class = c("phsdeploy_group_not_found", "phsdeploy_error"),
      name = name
    )
  }
  as.list(found)
}

#' Members of an internal Connect group
#'
#' @inheritParams list_int_content
#' @param group_guid The group's GUID, from [int_find_group()].
#'
#' @return A data frame with one row per member, all columns character:
#'   `guid`, `username`, `email`, `first_name`, `last_name` and `user_role`.
#'   Otherwise an error of class `phsdeploy_int_http_error`.
#' @export
int_group_members <- function(client = phs_connect_int(), group_guid) {
  check_guid(group_guid)
  records <- int_call(client, "GET", paste0("v1/groups/", group_guid, "/members"))
  int_frame(records, c("guid", "username", "email", "first_name", "last_name", "user_role"))
}

# Tags -------------------------------------------------------------------------

#' Internal Connect tags
#'
#' Tags form a tree: top-level tags are categories, and content is tagged with
#' the tags inside them. Creating, renaming and deleting tags is left to
#' Connect administrators, in Connect. These functions read the tree and
#' change which tags an item has, which an item's owner or collaborators can
#' do.
#'
#' * `int_list_tags()` lists every tag.
#' * `int_content_tags()` lists one item's tags.
#' * `int_add_tag()` and `int_remove_tag()` change one item's tags. Adding a
#'   tag the item already has, or removing one it doesn't, changes nothing.
#'
#' @inheritParams int_list_access
#' @param tag_id A tag's id, from `int_list_tags()`.
#'
#' @return `int_list_tags()` and `int_content_tags()` return a data frame with
#'   `id`, `name` and `parent_id` (`NA` for a category), all character. The
#'   others return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_int_http_error`.
#' @export
#' @examples
#' \dontrun{
#' tags <- int_list_tags()
#' int_add_tag(guid = guid, tag_id = tags$id[tags$name == "Health"])
#' int_content_tags(guid = guid)
#' }
int_list_tags <- function(client = phs_connect_int()) {
  int_frame(int_call(client, "GET", "v1/tags"), c("id", "name", "parent_id"))
}

#' @rdname int_list_tags
#' @export
int_content_tags <- function(client = phs_connect_int(), guid) {
  check_guid(guid)
  int_frame(int_call(client, "GET", paste0("v1/content/", guid, "/tags")), c("id", "name", "parent_id"))
}

#' @rdname int_list_tags
#' @export
int_add_tag <- function(client = phs_connect_int(), guid, tag_id) {
  check_guid(guid)
  tag_id <- check_int_id(tag_id, "tag_id")
  if (tag_id %in% int_content_tags(client, guid)$id) {
    return(invisible(TRUE))
  }
  int_call(client, "POST", paste0("v1/content/", guid, "/tags"), body = list(tag_id = tag_id))
  invisible(TRUE)
}

#' @rdname int_list_tags
#' @export
int_remove_tag <- function(client = phs_connect_int(), guid, tag_id) {
  check_guid(guid)
  tag_id <- check_int_id(tag_id, "tag_id")
  int_call(client, "DELETE", paste0("v1/content/", guid, "/tags/", tag_id), allow = 404L)
  invisible(TRUE)
}

# Requests and helpers ---------------------------------------------------------

#' Call the internal Connect API
#'
#' Sends the client's key in the Authorization header, which httr2 redacts.
#' Retries a network failure, a 429 or a 5xx response up to 3 times, except
#' for a `POST`, which is retried only on a 429. An error status is raised as
#' a new condition, scrubbed of the key and of URLs, without the request or
#' response.
#'
#' @param client A connectapi client.
#' @param method The HTTP method.
#' @param path The path after `__api__/`, such as `"v1/content"`.
#' @param query A named list of query parameters.
#' @param body A list sent as JSON, or `NULL`.
#' @param allow Statuses of 300 or above to accept, returning `NULL`.
#' @param call The environment of the calling function, shown in errors.
#' @return The parsed body. A listing comes back as its list of records,
#'   whether Connect sent an array or an object with `results`. Otherwise an
#'   error of class `phsdeploy_int_http_error`, with `status` (`NA` if the
#'   server wasn't reached).
#' @noRd
int_call <- function(client, method, path, query = list(), body = NULL,
                     allow = integer(), call = rlang::caller_env()) {
  check_int_client(client, call = call)
  req <- httr2::request(sub("/+$", "", client$server))
  req <- httr2::req_url_path_append(req, "__api__", path)
  req <- httr2::req_method(req, method)
  req <- httr2::req_headers(req, Authorization = paste("Key", client$api_key), .redact = "Authorization")
  req <- httr2::req_user_agent(req, cloud_user_agent)
  req <- httr2::req_timeout(req, 60)
  if (length(query)) req <- httr2::req_url_query(req, !!!query)
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_retry(
    req,
    max_tries = 3,
    retry_on_failure = toupper(method) != "POST",
    is_transient = cloud_is_transient(method)
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req, verbosity = 0L), error = function(e) e)
  secrets <- client$api_key
  if (inherits(resp, "error")) {
    abort_int_http(NA_integer_, squish_scrub(conditionMessage(resp), secrets), method, path, call)
  }
  status <- httr2::resp_status(resp)
  parsed <- if (httr2::resp_has_body(resp)) {
    tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  }
  if (status %in% allow) {
    return(NULL)
  }
  if (status >= 300L) {
    detail <- if (is.list(parsed)) paste(c(parsed$code, parsed$error %||% parsed$message), collapse = ": ") else ""
    abort_int_http(status, squish_scrub(detail, secrets), method, path, call)
  }
  if (is.list(parsed) && !is.null(names(parsed)) && is.list(parsed$results)) parsed$results else parsed
}

#' Raise an error for a failed internal Connect API call
#'
#' @param status The HTTP status, or `NA` if the server wasn't reached.
#' @param detail Scrubbed error text.
#' @param method,path The request's method and path, shown in the message.
#' @param call The environment of the calling function, shown in the error.
#' @return Does not return, raises an error of class `phsdeploy_int_http_error`.
#' @noRd
abort_int_http <- function(status, detail, method, path, call) {
  cli::cli_abort(
    c(
      if (is.na(status)) {
        "Couldn't reach internal Connect for {.code {method} {path}}."
      } else {
        "Internal Connect refused {.code {method} {path}} with status {status}."
      },
      "x" = if (nzchar(detail)) "{detail}"
    ),
    class = c("phsdeploy_int_http_error", "phsdeploy_error"),
    status = status,
    call = call
  )
}

#' Records as a data frame of text columns
#'
#' @param records A list of records.
#' @param fields The fields to keep, in order.
#' @return A data frame with one character column per field, `NA` where a
#'   record lacks the field.
#' @noRd
int_frame <- function(records, fields) {
  records <- records %||% list()
  out <- lapply(fields, function(field) {
    vapply(records, function(r) flat_chr(r[[field]]), character(1))
  })
  names(out) <- fields
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Connect's access role for a phsdeploy role
#'
#' @param role `"collaborator"` or `"viewer"`.
#' @return `"owner"` or `"viewer"`.
#' @noRd
int_role_in <- function(role) {
  ifelse(role == "collaborator", "owner", role)
}

#' phsdeploy's role for Connect's access roles
#'
#' @param role A character vector of Connect roles.
#' @return The roles, with `"owner"` as `"collaborator"`.
#' @noRd
int_role_out <- function(role) {
  ifelse(role %in% "owner", "collaborator", role)
}

#' Check a connectapi client
#'
#' @param client The value to check.
#' @param call The environment of the calling function, shown in the error.
#' @return `client`, invisibly. Otherwise an error of class
#'   `phsdeploy_bad_client`.
#' @noRd
check_int_client <- function(client, call = rlang::caller_env()) {
  if (!inherits(client, "Connect") || !rlang::is_string(client$server) ||
    !rlang::is_string(client$api_key)) {
    cli::cli_abort(
      "{.arg client} must be a connectapi client for internal Connect.",
      class = c("phsdeploy_bad_client", "phsdeploy_error"),
      call = call
    )
  }
  invisible(client)
}

#' Check a numeric id from internal Connect
#'
#' Tag and permission ids are numbers, sent as text. Anything else is refused
#' before it is pasted into a path.
#'
#' @param id The value, a whole number or digits as text.
#' @param arg The argument name used in the error message.
#' @param call The environment of the calling function, shown in the error.
#' @return The id as text. Otherwise an error of class
#'   `phsdeploy_bad_argument`.
#' @noRd
check_int_id <- function(id, arg, call = rlang::caller_env()) {
  text <- if (is.numeric(id) && length(id) == 1L && !is.na(id)) format(id, scientific = FALSE) else id
  if (!rlang::is_string(text) || !grepl("^[0-9]+$", text)) {
    cli::cli_abort(
      "{.arg {arg}} must be one id made of digits.",
      class = c("phsdeploy_bad_argument", "phsdeploy_error"),
      call = call
    )
  }
  text
}
