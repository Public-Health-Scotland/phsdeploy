#' Read one item of Connect (internal) content
#'
#' @param guid The content GUID on the internal Connect server.
#' @param client A connectapi client, by default `phs_connect_int()`.
#'
#' @return A list with `guid`, `name`, `title`, `owner_guid`, `app_mode`,
#'   `bundle_id` (character, `NA` if never deployed), `last_deployed_time`,
#'   `content_url` and `dashboard_url` (`NA` when the server gives none).
#' @export
get_int_content <- function(guid, client = phs_connect_int()) {
  item <- connectapi::content_item(client, guid)
  x <- item$content
  list(
    guid = flat_chr(x$guid),
    name = flat_chr(x$name),
    title = flat_chr(x$title),
    owner_guid = flat_chr(x$owner_guid),
    app_mode = flat_chr(x$app_mode),
    bundle_id = flat_chr(x$bundle_id),
    last_deployed_time = flat_chr(x$last_deployed_time),
    content_url = flat_chr(x$content_url),
    dashboard_url = flat_chr(x$dashboard_url)
  )
}

#' List the bundles of one item of internal content
#'
#' @inheritParams get_int_content
#'
#' @return A data frame with `id` (character), `active` (logical), `size`
#'   (double) and `created_time` (character), one row per bundle.
#' @export
get_int_content_bundles <- function(guid, client = phs_connect_int()) {
  raw <- client$GET(paste0("v1/content/", guid, "/bundles"))
  data.frame(
    id = vapply(raw, function(b) flat_chr(b$id), character(1)),
    active = vapply(raw, function(b) isTRUE(b$active), logical(1)),
    size = vapply(raw, function(b) as.numeric(b$size %||% NA_real_), numeric(1)),
    created_time = vapply(raw, function(b) flat_chr(b$created_time), character(1)),
    stringsAsFactors = FALSE
  )
}

# NULL / missing becomes NA_character_ and anything else becomes a single string.
flat_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1]])
}
