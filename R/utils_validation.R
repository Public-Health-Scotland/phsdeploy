#' Pattern for a Connect content GUID
#'
#' A whole-string match for one UUID in its usual 8-4-4-4-12 hex form.
#'
#' @format A single regular expression string.
#' @noRd
guid_pattern <- "^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$"

#' GUID validator
#' 
#' The guid is pasted into API paths, so anything other than one UUID is refused
#' before a request is made. This will stop things like `"../"` or a vector.
#' 
#' @param guid The GUID to validate.
#' @param arg The argument name used in the error message.
#' @param call the calling function environment to return.
#' @return `guid`, invisibly. Otherwise error of class `phsdeploy_bad_guid`.
#' @noRd
check_guid <- function(guid, arg = rlang::caller_arg(guid), call = rlang::caller_env()) {
  if (!rlang::is_string(guid) || !grepl(guid_pattern, guid)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single content GUID.",
      class = c("phsdeploy_bad_guid", "phsdeploy_error"),
      call = call
    )
  }
  invisible(guid)
}
