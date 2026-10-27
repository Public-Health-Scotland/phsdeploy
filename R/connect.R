#' Connect to the PHS internal Connect server
#'
#' Creates a connectapi client object.
#'
#' @param config A `phsdeploy_config` with an `int` part, by default read from
#'   the environment with `phsdeploy_config("int")`, or built with
#'   [phsdeploy_int_config()].
#'
#' @return A connectapi `Connect` object.
#' @export
phs_connect_int <- function(config = phsdeploy_config("int")) {
  check_config_part(config, "int")
  connectapi::connect(
    server = config$int$url,
    api_key = config$int$key
  )
}

#' Connect to Posit Connect Cloud
#'
#' Checks the credential by getting an access token, and looks up the
#' account's id through the Connect Cloud API. Nothing is written to disk: the
#' token is held in memory only, so a connection is safe to use in a Shiny app
#' that serves several people from one R process. A network failure is retried
#' by each API call. A refused credential isn't retried.
#'
#' Deploying also needs the account registered with rsconnect, which
#' [deploy_bundle()] does for itself, in a temporary folder of its own.
#'
#' @param config A `phsdeploy_config` with a `cld` part, by default read from
#'   the environment with `phsdeploy_config("cld")`. Use
#'   [phsdeploy_cld_config()] to connect with a credential from elsewhere.
#'
#' @return An object of class `phsdeploy_cld` with `server`, `account`,
#'   `account_id`, `client_id` and `client_secret`. Printing never shows the
#'   client secret.
#'
#'   Otherwise an error of class `phsdeploy_cloud_token_error` (the credential
#'   was refused, or no token came back), `phsdeploy_cloud_http_error` (the
#'   account lookup failed) or `phsdeploy_cloud_account_not_found`.
#' @export
#' @examples
#' \dontrun{
#' cld <- phs_connect_cld()
#' cld
#' }
phs_connect_cld <- function(config = phsdeploy_config("cld")) {
  check_config_part(config, "cld")
  settings <- config$cld
  cld <- new_phsdeploy_cld(
    account = settings$account,
    client_id = settings$client_id,
    client_secret = settings$client_secret
  )
  cld$account_id <- cloud_account_id(cld)
  cld
}

#' Register a Cloud connection's account with rsconnect, temporarily
#'
#' rsconnect needs the account registered before `deployApp()`. It stores the
#' account, credential included, in its config folder, so this first points
#' rsconnect's folders at a new temporary directory (`local_rsconnect_config()`),
#' removed when `.local_envir` exits. That changes environment variables for
#' the whole R process, so two deploys must not run at once in one process.
#' Run each in its own background process instead.
#'
#' A network failure is retried once, since it often clears by itself. A
#' refused credential isn't retried.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param .local_envir The environment whose exit removes the registration.
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_cloud_auth_error`, also of class `phsdeploy_cloud_unreachable`
#'   when the network failed twice rather than the credential being refused.
#' @noRd
local_cloud_registration <- function(cld, .local_envir = parent.frame()) {
  local_rsconnect_config(.local_envir = .local_envir)
  registered <- register_cloud_account(cld)
  if (is_network_failure(registered)) {
    Sys.sleep(2)
    registered <- register_cloud_account(cld)
  }
  if (inherits(registered, "error")) {
    detail <- squish_scrub(conditionMessage(registered), cloud_secrets(cld))
    network <- is_network_failure(registered)
    cli::cli_abort(
      c(
        if (network) {
          "Could not reach Posit Connect Cloud to sign in for the deploy, even after a retry."
        } else {
          "Posit Connect Cloud refused the sign-in for the deploy."
        },
        "x" = "{detail}",
        "i" = if (network) {
          "This is a network problem, not the credential. If it persists, restart R."
        } else {
          "Check the Connect Cloud client ID and secret."
        }
      ),
      class = c(
        if (network) "phsdeploy_cloud_unreachable",
        "phsdeploy_cloud_auth_error", "phsdeploy_error"
      )
    )
  }
  invisible(TRUE)
}

#' Register the Connect Cloud account with rsconnect
#'
#' @param cld A `phsdeploy_cld` connection.
#' @return `TRUE`, or the error rsconnect raised.
#' @noRd
register_cloud_account <- function(cld) {
  check_rsconnect_config_temporary()
  local_rsconnect_http()
  tryCatch(
    {
      rsconnect::connectCloudClientCredentials(
        clientId = cld$client_id,
        clientSecret = cld$client_secret,
        accountName = cld$account,
        quiet = TRUE
      )
      TRUE
    },
    error = function(e) e
  )
}

#' Did a request fail without reaching the server?
#'
#' A network failure, such as a timeout or a dropped or stale connection, is
#' worth retrying because it often clears by itself. httr2, which rsconnect
#' uses, gives these the class `httr2_failure`. The curl error text is also
#' matched, in case a caller re-raises the error without its class. An HTTP
#' error (the server answered, for example to refuse a credential) or a failed
#' build is not a network failure.
#'
#' @param x A condition, or any other value.
#' @return `TRUE` for a network failure, otherwise `FALSE`.
#' @noRd
is_network_failure <- function(x) {
  inherits(x, "error") && (
    rlang::cnd_inherits(x, "httr2_failure") ||
      grepl(network_failure_pattern, conditionMessage(x), ignore.case = TRUE)
  )
}

#' Pattern for curl's network failure messages
#'
#' @format A single regular expression string.
#' @noRd
network_failure_pattern <- paste(
  "timeout was reached", "timed out", "could not resolve", "failed to connect",
  "connection reset", "recv failure", "send failure", "ssl connect error",
  sep = "|"
)

#' Give rsconnect's requests a longer connection timeout
#'
#' rsconnect sets no timeout of its own, so the curl package's default limit
#' of 10 seconds to open a connection applies. A slow network can exceed that
#' without anything being wrong. This allows 60 seconds, through rsconnect's
#' `rsconnect.libcurl.options` option, unless that option already sets one.
#' Undone when `.local_envir` exits.
#'
#' @param .local_envir The environment whose exit undoes the change.
#' @return `NULL`, invisibly.
#' @noRd
local_rsconnect_http <- function(.local_envir = parent.frame()) {
  options <- getOption("rsconnect.libcurl.options", list())
  if (is.null(options$connecttimeout)) options$connecttimeout <- 60L
  withr::local_options(rsconnect.libcurl.options = options, .local_envir = .local_envir)
  invisible()
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
