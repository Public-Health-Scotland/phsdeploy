#' Read the configuration from environment variables
#'
#' Reads the settings needed from environment variables (normally set in
#' `.Renviron`) and returns them as one object. Every missing or invalid
#' variable is reported in one error, so they can all be fixed at once.
#'
#' Variables read:
#' * The PHS Connect internal server (`needs = "int"`):
#'   * `PHSDEPLOY_INT_URL`: the server URL
#'   * `PHSDEPLOY_INT_KEY`: an API key
#' * Posit Connect Cloud (`needs = "cld"`):
#'   * `PHSDEPLOY_CLD_ACCOUNT`: the Connect Cloud account name
#'   * `PHSDEPLOY_CLD_CLIENT_ID`: the client ID of a credential with access to
#'     that account
#'   * `PHSDEPLOY_CLD_CLIENT_SECRET`: that credential's client secret
#'
#' Printing the result never shows the key or the client secret.
#'
#' @param needs Which settings to read: `"int"` (internal Connect), `"cld"`
#'   (Connect Cloud), or both.
#'
#' @return An object of class `phsdeploy_config`: a list with `int` (`url`,
#'   `key`) and/or `cld` (`account`, `client_id`, `client_secret`).
#'
#'   If anything is missing or invalid, an error of class
#'   `phsdeploy_config_error` is raised, with `missing` and `invalid` fields
#'   naming the variables.
#'
#' @export
#' @examples
#' \dontrun{
#' server_config <- phsdeploy_config("int")
#' server_config
#' }
phsdeploy_config <- function(needs = c("int", "cld")) {
  needs <- rlang::arg_match(needs, c("int", "cld"), multiple = TRUE)

  missing <- character()
  invalid <- character()
  int <- NULL
  cld <- NULL

  if ("int" %in% needs) {
    url <- config_value("PHSDEPLOY_INT_URL", url = TRUE)
    key <- config_value("PHSDEPLOY_INT_KEY")
    if (is.null(url)) missing <- c(missing, "PHSDEPLOY_INT_URL")
    if (is.null(key)) missing <- c(missing, "PHSDEPLOY_INT_KEY")
    if (!is.null(url) && !grepl("^https?://", url)) {
      invalid <- c(invalid, "PHSDEPLOY_INT_URL")
    }
    int <- list(url = url, key = key)
  }

  if ("cld" %in% needs) {
    account <- config_value("PHSDEPLOY_CLD_ACCOUNT")
    client_id <- config_value("PHSDEPLOY_CLD_CLIENT_ID")
    client_secret <- config_value("PHSDEPLOY_CLD_CLIENT_SECRET")
    if (is.null(account)) missing <- c(missing, "PHSDEPLOY_CLD_ACCOUNT")
    if (is.null(client_id)) missing <- c(missing, "PHSDEPLOY_CLD_CLIENT_ID")
    if (is.null(client_secret)) missing <- c(missing, "PHSDEPLOY_CLD_CLIENT_SECRET")
    cld <- list(account = account, client_id = client_id, client_secret = client_secret)
  }

  if (length(missing) || length(invalid)) {
    abort_config(missing, invalid)
  }

  new_phsdeploy_config(int = int, cld = cld)
}

#' Internal constructor for `phsdeploy_config` objects
#'
#' A `phsdeploy_config` is a plain list with a class attribute. Only the parts
#' that are supplied are present.
#'
#' @param int `NULL` or `list(url, key)`.
#' @param cld `NULL` or `list(account, client_id, client_secret)`.
#' @return An object of class `phsdeploy_config`.
#' @noRd
new_phsdeploy_config <- function(int = NULL, cld = NULL) {
  structure(
    c(
      if (!is.null(int)) list(int = int),
      if (!is.null(cld)) list(cld = cld)
    ),
    class = "phsdeploy_config"
  )
}

#' Check a configuration has the part a connection needs
#'
#' @param config The value to check.
#' @param part `"int"` or `"cld"`.
#' @param call The environment of the calling function, shown in the error.
#' @return `config`, invisibly. Otherwise an error of class
#'   `phsdeploy_config_error`.
#' @noRd
check_config_part <- function(config, part, call = rlang::caller_env()) {
  if (!inherits(config, "phsdeploy_config") || is.null(config[[part]])) {
    cli::cli_abort(
      "{.arg config} must be a {.cls phsdeploy_config} with a {.field {part}} part.",
      class = c("phsdeploy_config_error", "phsdeploy_error"),
      call = call
    )
  }
  invisible(config)
}

#' Read an environment variable as a config value
#'
#' @param name The environment variable's name.
#' @param url `TRUE` if the value is a URL, so a trailing `/` is removed.
#' @return The cleaned value, or `NULL` if the variable is unset or empty.
#' @noRd
config_value <- function(name, url = FALSE) {
  value <- clean_value(Sys.getenv(name, unset = ""), url)
  if (nzchar(value)) value
}

#' Clean a config value
#'
#' Trims whitespace and strips one pair of surrounding quotes, which are easy
#' to add by mistake in `.Renviron`. A trailing `/` is only stripped from URLs
#' (`url = TRUE`), never from keys or secrets, where it could be part of the
#' value.
#'
#' @param x The text to clean.
#' @param url `TRUE` if the value is a URL.
#' @return The cleaned text.
#' @noRd
clean_value <- function(x, url = FALSE) {
  x <- trimws(x)
  x <- sub("^([\"'])(.*)\\1$", "\\2", x)
  x <- trimws(x)
  if (url) x <- sub("/+$", "", x)
  x
}

#' Raise a config error
#'
#' Lists every missing and invalid variable in one message.
#'
#' @param missing Names of the missing variables.
#' @param invalid Names of the URL variables that don't start with `http://` or
#'   `https://`.
#' @return Does not return, raises an error of class `phsdeploy_config_error`.
#' @noRd
abort_config <- function(missing, invalid) {
  bullets <- c(
    if (length(missing)) {
      c("x" = "Missing environment variable{?s}: {.envvar {missing}}.")
    },
    if (length(invalid)) {
      c("x" = "{.envvar {invalid}} must start with {.code http://} or {.code https://}.")
    },
    "i" = "Set them in your {.file .Renviron} and restart R."
  )
  cli::cli_abort(
    c("The phsdeploy configuration is incomplete.", bullets),
    class = c("phsdeploy_config_error", "phsdeploy_error"),
    missing = missing,
    invalid = invalid
  )
}

#' Format a configuration for printing
#'
#' Shows which settings are present, redacting any key and client secret,
#' ensuring a printed configuration is safe to share.
#'
#' @param x A `phsdeploy_config`.
#' @param ... Not used.
#' @return A character vector, one element per line.
#' @noRd
#' @export
format.phsdeploy_config <- function(x, ...) {
  c(
    "<phsdeploy_config>",
    if (!is.null(x$int)) {
      c("  int:", paste0("    url: ", x$int$url), "    key: <hidden>")
    },
    if (!is.null(x$cld)) {
      c(
        "  cld:",
        paste0("    account: ", x$cld$account),
        paste0("    client_id: ", x$cld$client_id),
        "    client_secret: <hidden>"
      )
    }
  )
}

#' Print a configuration
#'
#' @param x A `phsdeploy_config`.
#' @param ... Passed to `format()`.
#' @return `x`, invisibly.
#' @noRd
#' @export
print.phsdeploy_config <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
