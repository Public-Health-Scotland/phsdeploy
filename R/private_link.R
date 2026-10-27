#' URL parameter that carries a private link's token
#'
#' A private link is the item's address with its token added as this
#' parameter. Connect Cloud doesn't document the link's form, so check 11
#' (`checks/11-private-link.R`) tries the likely parameters and reports which
#' one opens the item without signing in. Update this if it reports another.
#'
#' @format A single string.
#' @noRd
cloud_private_link_param <- "private_link_token"

#' Switch private link sharing on or off for a Connect Cloud item
#'
#' Private link sharing lets anyone with the link view an item while it stays
#' private otherwise. It's a single field on the item, `private_link_enabled`,
#' changed with `PATCH /contents/{id}`, which is what the Connect Cloud web
#' interface sends. Switching it off keeps the item's token, but the link no
#' longer works. The item is read back to check the change took.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param id The Cloud item's id.
#' @param enabled `TRUE` to switch sharing on, `FALSE` to switch it off.
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_bad_argument`, `phsdeploy_missing_item` or
#'   `phsdeploy_private_link_unchanged`.
#' @noRd
cloud_set_private_link <- function(cld, id, enabled) {
  check_cld(cld)
  check_content_id(id)
  if (!rlang::is_bool(enabled)) {
    cli::cli_abort(
      "{.arg enabled} must be {.code TRUE} or {.code FALSE}.",
      class = c("phsdeploy_bad_argument", "phsdeploy_error")
    )
  }
  cloud_call(cld, "PATCH", paste0("/contents/", id), body = list(private_link_enabled = enabled))
  after <- cloud_get_content(cld, id)
  if (is.null(after)) abort_missing_item(id)
  if (!identical(isTRUE(after$private_link_enabled), enabled)) {
    cli::cli_abort(
      "Private link sharing for item {.val {id}} is still {.val {!enabled}} after the change.",
      class = c("phsdeploy_private_link_unchanged", "phsdeploy_error"),
      cloud_id = id
    )
  }
  invisible(TRUE)
}

#' Get a Connect Cloud item's private link
#'
#' Switches private link sharing on if it's off (unless `enable = FALSE`),
#' then builds the link from the item's address and its token. The token is
#' read straight from the API, since every other function removes it from
#' the records it returns.
#'
#' The link is effectively a password for the item, so the result never
#' shows it when printed. Use `$url` to pass it on, and never write it to a
#' log.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param id The Cloud item's id.
#' @param enable `TRUE` to switch sharing on first if needed. With `FALSE`,
#'   sharing must already be on.
#' @return An object of class `phsdeploy_private_link`: a list with
#'   `cloud_id`, `url` (the private link) and `base_url` (the item's address
#'   without the token). Otherwise an error of class
#'   `phsdeploy_private_link_off` or `phsdeploy_private_link_missing`.
#' @noRd
cloud_private_link <- function(cld, id, enable = TRUE) {
  check_cld(cld)
  check_content_id(id)
  item <- cloud_get_content(cld, id)
  if (is.null(item)) abort_missing_item(id)
  if (!isTRUE(item$private_link_enabled)) {
    if (!isTRUE(enable)) {
      cli::cli_abort(
        "Private link sharing is off for item {.val {id}}.",
        class = c("phsdeploy_private_link_off", "phsdeploy_error"),
        cloud_id = id
      )
    }
    cloud_set_private_link(cld, id, TRUE)
  }

  record <- cloud_json(cloud_call(cld, "GET", paste0("/contents/", id)))
  token <- repo_value(record$private_link_token)
  base_url <- repo_value(record$current_revision$url)
  if (is.na(token) || is.na(base_url)) {
    cli::cli_abort(
      "Connect Cloud didn't return a private link for item {.val {id}}.",
      class = c("phsdeploy_private_link_missing", "phsdeploy_error"),
      cloud_id = id
    )
  }
  structure(
    list(cloud_id = id, url = private_link_url(base_url, token), base_url = base_url),
    class = "phsdeploy_private_link"
  )
}

#' Build a private link from an address and a token
#'
#' @param base_url The item's address, such as
#'   `https://<id>.share.connect.posit.cloud`.
#' @param token The item's private link token.
#' @param param The URL parameter for the token.
#' @return The private link.
#' @noRd
private_link_url <- function(base_url, token, param = cloud_private_link_param) {
  paste0(sub("/+$", "", base_url), "/?", param, "=", utils::URLencode(token, reserved = TRUE))
}

#' Format a private link for printing
#'
#' Shows the item's address and that a token is set, never the token.
#'
#' @param x A `phsdeploy_private_link`.
#' @param ... Not used.
#' @return A character vector, one element per line.
#' @noRd
#' @export
format.phsdeploy_private_link <- function(x, ...) {
  c(
    "<phsdeploy_private_link>",
    paste0("  cloud_id: ", x$cloud_id),
    paste0("  url: ", x$base_url, "/?", cloud_private_link_param, "=<hidden>")
  )
}

#' Print a private link
#'
#' @param x A `phsdeploy_private_link`.
#' @param ... Passed to `format()`.
#' @return `x`, invisibly.
#' @noRd
#' @export
print.phsdeploy_private_link <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
