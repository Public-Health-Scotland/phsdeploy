#' Connect to the PHS internal Connect server
#'
#' Creates a connectapi client object.
#'
#' @param config A `phsdeploy_config` with an `int` part, by default read from
#'   the environment with `phsdeploy_config("int")`.
#'
#' @return A connectapi `Connect` object.
#' @export
phs_connect_int <- function(config = phsdeploy_config("int")) {
  connectapi::connect(
    server = config$int$url,
    api_key = config$int$key
  )
}

#' Connect to Posit Connect Cloud
#'
#' Registers the configured Connect Cloud account with rsconnect, which
#' deploying and deleting need, and looks up the account's id through the
#' Connect Cloud API.
#'
#' rsconnect stores the account, credential included, in its config folder.
#' **Call `local_rsconnect_config()` (internal) first, in the calling
#' function's frame,** so that folder is temporary and removed when the caller
#' exits. Without it this aborts before anything is contacted.
#'
#' @param config A `phsdeploy_config` with a `cld` part, by default read from
#'   the environment with `phsdeploy_config("cld")`.
#'
#' @return An object of class `phsdeploy_cld` with `server`, `account`,
#'   `account_id`, `client_id` and `client_secret`. Printing never shows the
#'   client secret.
#'
#'   Otherwise an error of class `phsdeploy_rsconnect_config_not_temporary`
#'   (rsconnect's folders are not inside `tempdir()`), `phsdeploy_cloud_auth_error`
#'   (rsconnect could not register the account), `phsdeploy_cloud_token_error`
#'   or `phsdeploy_cloud_http_error` (the account id lookup failed), or
#'   `phsdeploy_cloud_account_not_found`.
#' @export
phs_connect_cld <- function(config = phsdeploy_config("cld")) {
  check_rsconnect_config_temporary()
  settings <- config$cld

  registered <- tryCatch(
    {
      rsconnect::connectCloudClientCredentials(
        clientId = settings$client_id,
        clientSecret = settings$client_secret,
        accountName = settings$account,
        quiet = TRUE
      )
      TRUE
    },
    error = function(e) e
  )
  if (inherits(registered, "error")) {
    detail <- squish_scrub(conditionMessage(registered), settings$client_secret)
    cli::cli_abort(
      c("Could not sign in to Posit Connect Cloud.", "x" = "{detail}"),
      class = c("phsdeploy_cloud_auth_error", "phsdeploy_error")
    )
  }

  cld <- new_phsdeploy_cld(
    account = settings$account,
    client_id = settings$client_id,
    client_secret = settings$client_secret
  )
  cld$account_id <- cloud_account_id(cld)
  cld
}

#' Internal constructor for `phsdeploy_cld` objects
#'
#' @param account The Connect Cloud account name.
#' @param client_id,client_secret The credential for that account.
#' @param account_id The account's id, or `NULL` until it is looked up.
#' @return An object of class `phsdeploy_cld`.
#' @noRd
new_phsdeploy_cld <- function(account, client_id, client_secret, account_id = NULL) {
  structure(
    list(
      server = cloud_server,
      account = account,
      account_id = account_id,
      client_id = client_id,
      client_secret = client_secret
    ),
    class = "phsdeploy_cld"
  )
}

#' Format a Connect Cloud connection for printing
#'
#' Never shows the client secret.
#'
#' @param x A `phsdeploy_cld`.
#' @param ... Not used.
#' @return A character vector, one element per line.
#' @noRd
#' @export
format.phsdeploy_cld <- function(x, ...) {
  c(
    "<phsdeploy_cld>",
    paste0("  server: ", x$server),
    paste0("  account: ", x$account),
    paste0("  account_id: ", x$account_id %||% "<unknown>"),
    paste0("  client_id: ", x$client_id),
    "  client_secret: <hidden>"
  )
}

#' Print a Connect Cloud connection
#'
#' @param x A `phsdeploy_cld`.
#' @param ... Passed to `format()`.
#' @return `x`, invisibly.
#' @noRd
#' @export
print.phsdeploy_cld <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Point rsconnect's config and data folders at a temporary directory
#'
#' rsconnect stores account and deployment records under the user's config and
#' data folders. This redirects both into a new temporary directory, so no
#' credential or record outlives the calling function. Everything is undone
#' when `.local_envir` exits.
#'
#' @param .local_envir The environment whose exit undoes the change.
#' @return The path of the new temporary directory, invisibly.
#' @noRd
local_rsconnect_config <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(pattern = "phsdeploy-rsconnect-", .local_envir = .local_envir)
  config_dir <- file.path(dir, "config")
  data_dir <- file.path(dir, "data")
  dir.create(config_dir)
  dir.create(data_dir)
  withr::local_envvar(
    R_USER_CONFIG_DIR = config_dir,
    R_USER_DATA_DIR = data_dir,
    .local_envir = .local_envir
  )
  invisible(dir)
}

#' Check rsconnect's folders are temporary
#'
#' Guards every function that registers an account with rsconnect, so the
#' credential is never written to the user's real config folder.
#'
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_rsconnect_config_not_temporary`.
#' @noRd
check_rsconnect_config_temporary <- function() {
  vars <- c("R_USER_CONFIG_DIR", "R_USER_DATA_DIR")
  temporary <- vapply(vars, function(var) {
    dir <- Sys.getenv(var, unset = "")
    nzchar(dir) && path_is_within(dir, tempdir())
  }, logical(1))
  if (!all(temporary)) {
    cli::cli_abort(
      c(
        "rsconnect's config and data folders must be temporary directories.",
        "x" = "Not set, or not inside {.code tempdir()}: {.envvar {vars[!temporary]}}.",
        "i" = "Call {.code local_rsconnect_config()} first, so nothing lasting is written."
      ),
      class = c("phsdeploy_rsconnect_config_not_temporary", "phsdeploy_error")
    )
  }
  invisible(TRUE)
}

#' Is a path inside another?
#'
#' Both paths are normalised first, so `..` parts can't make an outside path
#' look inside. Symbolic links are resolved for paths that exist, such as
#' macOS's `/var`, which is a link to `/private/var`.
#'
#' @param path The path to check.
#' @param parent The folder it should be inside.
#' @return `TRUE` if `path` is `parent` or inside it, otherwise `FALSE`.
#' @noRd
path_is_within <- function(path, parent) {
  path <- fs::path_norm(normalizePath(path, mustWork = FALSE))
  parent <- fs::path_norm(normalizePath(parent, mustWork = FALSE))
  path == parent || startsWith(path, paste0(parent, "/"))
}
