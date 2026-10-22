#' Pattern for a UUID
#'
#' The usual 8-4-4-4-12 hex form, unanchored, so it can find UUIDs inside
#' longer text (used by `scrub_text()` to keep ids in messages).
#'
#' @format A single regular expression string.
#' @noRd
uuid_pattern <- "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"

#' Pattern for a Connect content GUID
#'
#' `uuid_pattern` anchored at both ends, so the whole string must be one UUID.
#'
#' @format A single regular expression string.
#' @noRd
guid_pattern <- paste0("^", uuid_pattern, "$")

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

#' Pattern for a run of token-like characters
#'
#' 24 or more characters that could make up a key, token or secret. Shorter
#' runs are left alone, so ordinary words and ids stay readable.
#'
#' @format A single regular expression string.
#' @noRd
token_run_pattern <- "[A-Za-z0-9_.~+/=-]{24,}"

#' Scrub free text before showing it
#'
#' Every http(s) URL becomes `"<url withheld>"`, and every run of 24 or more
#' token-like characters becomes `"<withheld>"`. A run made only of UUIDs plus
#' short glue (such as `id=`) is kept, because ids help diagnosis. Any of
#' `secrets` found in the text is replaced by `"<redacted>"` first, so short
#' secrets are caught too.
#'
#' Used so keys and tokens don't reach errors and logs.
#'
#' @param text A character vector. Anything else is returned unchanged.
#' @param secrets Character vector of exact strings to redact (empty strings
#'   are ignored).
#' @return `text`, scrubbed. `NA` stays `NA`.
#' @noRd
scrub_text <- function(text, secrets = character()) {
  if (!is.character(text)) {
    return(text)
  }
  na <- is.na(text)
  for (secret in unique(secrets[!is.na(secrets) & nzchar(secrets)])) {
    text <- gsub(secret, "<redacted>", text, fixed = TRUE)
  }
  text <- gsub("https?://[^[:space:]\"'<>]*", "<url withheld>", text, ignore.case = TRUE)
  run_match <- gregexpr(token_run_pattern, text)
  regmatches(text, run_match) <- lapply(regmatches(text, run_match), function(runs) {
    rest <- gsub(uuid_pattern, " ", runs)
    ifelse(grepl(uuid_pattern, runs) & !grepl(token_run_pattern, rest), runs, "<withheld>")
  })
  text[na] <- NA_character_
  text
}
