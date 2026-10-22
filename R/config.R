#' Read a configuration from environment variables
#'
#' Reads the settings required from environment variables (normally set in `.Renviron`)
#' and returns as one object.
#'
#' Variables read:
#' * The PHS Connect internal server (`needs = "int"`):
#'   * `PHSDEPLOY_INT_URL`: the server URL
#'   * `PHSDEPLOY_INT_KEY`: an API key
#'
#' @param needs Which settings to read: `"int"` (internal Connect)
#'
#' @return An object of class `phsdeploy_config`: a list with `int`
#'   (`url`, `key`).
#'
#'   If anything is missing, an error of class `phsdeploy_config_error` is
#'   raised, with a `missing` field naming every missing variable.
#'
#' @export
#' @examples
#' \dontrun{
#' server_config <- phsdeploy_config("int")
#' server_config
#' }
phsdeploy_config <- function(needs = c("int")) {
  needs <- rlang::arg_match(needs, c("int"), multiple = TRUE)

  missing <- character()
  invalid <- character()
  int <- NULL

  if ("int" %in% needs) {
    url <- config_value("PHSDEPLOY_INT_URL", url = TRUE)
    key <- config_value("PHSDEPLOY_INT_KEY")
    if (is.null(url)) missing <- c(missing, "PHSDEPLOY_INT_URL")
    if (is.null(key)) missing <- c(missing, "PHSDEPLOY_INT_KEY")
    if (!is.null(url) && !grepl("^https?://", url)) {
      invalid <- c(invalid, "PHSDEPLOY_INT_URL")
    }
    if (!length(c(missing, invalid))) {
      int <- list(url = url, key = key)
    }
  }

  if (length(missing) || length(invalid)) {
    abort_config(missing, invalid)
  }

  new_phsdeploy_config(int = int)
}

#' Internal constructor for `phsdeploy_config` objects
#'
#' A `phsdeploy_config` is a plain list with a class attribute. Only the parts
#' that are supplied are present.
#'
#' @param int `NULL` or `list(url, key)`
#' @noRd
new_phsdeploy_config <- function(int = NULL) {
  structure(
    c(
      if (!is.null(int)) list(int = int)
    ),
    class = "phsdeploy_config"
  )
}

#' Read an environment variable as a config value
#' 
#' Identify values from environment variables. Returns NULL when no value can be found.
#' 
#' @param name environment variable name
#' @noRd
config_value <- function(name, url = FALSE) {
  value <- clean_value(Sys.getenv(name, unset = ""), url)
  if (nzchar(value)) value
}

#' Clean value utility
#' 
#' Trim whitespace and strip one pair of surrounding quotes. A trailing "/" is only
#' stripped from URLs (`url = TRUE`), never from keys or secrets.
#' 
#' @param x the text to be cleaned
#' @param url a flag to note if the value is supposed to be a URL
#' @noRd
clean_value <- function(x, url = FALSE) {
  x <- trimws(x)
  x <- sub("^([\"'])(.*)\\1$", "\\2", x)
  x <- trimws(x)
  if (url) x <- sub("/+$", "", x)
  x
}

#' Abort config utility
#' 
#' Gracefully encounter issues with defining config values, collect errors, and abort processes.
#' 
#' @param missing missing value name(s)
#' @param invalid invalid value name(s)
#' @noRd
abort_config <- function(missing, invalid) {
  bullets <- c(
    if (length(missing)) {
      c("Missing environment variable{?s}: {.envvar {missing}}.")
    },
    if (length(invalid)) {
      c("x" = "{.envvar {invalid}} must start with {.code http://} or {.code https://}.")
    },
    "i" = "Set them in your {.file .Renviron} and restart R."
  )
  cli::cli_abort(
    bullets,
    class = c("phsdeploy_config_error", "phsdeploy_error"),
    missing = missing,
    invalid = invalid
  )
}
