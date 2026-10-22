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

#' Download a bundle from the Connect (internal) server
#'
#' Quiet internal helper. Checks that the bundle belongs to the content before
#' downloading anything.
#'
#' @param guid Content GUID.
#' @param bundle_id Bundle id, or `NULL` for the active bundle.
#' @param dir Folder to download into.
#' @param client A connectapi client.
#'
#' @return A list with `path`, `guid`, `bundle_id` and `sha256`.
#' @noRd
fetch_bundle <- function(guid, bundle_id = NULL, dir, client = phs_connect_int()) {
  if (is.null(bundle_id)) {
    bundle_id <- get_int_content(guid, client)$bundle_id
    if (is.na(bundle_id)) {
      cli::cli_abort(
        c("Content {.val {guid}} has never been deployed, so it has no bundle."),
        class = c("phsdeploy_no_bundle", "phsdeploy_error")
      )
    }
  } else {
    bundle_id <- format_bundle_id(bundle_id)
  }

  known <- get_int_content_bundles(guid, client)$id
  if (!bundle_id %in% known) {
    cli::cli_abort(
      c(
        "Bundle {.val {bundle_id}} does not belong to content {.val {guid}}.",
        "i" = "Known bundles: {.val {known}}."
      ),
      class = c("phsdeploy_unknown_bundle", "phsdeploy_error")
    )
  }

  fs::dir_create(dir)
  path <- file.path(dir, paste0("bundle-", bundle_id, ".tar.gz"))
  item <- connectapi::content_item(client, guid)
  suppressMessages(
    connectapi::download_bundle(item, filename = path, bundle_id = bundle_id, overwrite = TRUE)
  )

  list(path = path, guid = guid, bundle_id = bundle_id, sha256 = bundle_sha256(path))
}

#' A bundle id as text
#'
#' A number is written out in full (`100000`, never `1e+05`), so it matches
#' the ids Connect lists.
#' 
#' @param x A bundle id: a string or a single number.
#' @return A string.
#' @noRd
format_bundle_id <- function(x) {
  if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
}

#' SHA-256 checksum of a file
#' 
#' @param path File path.
#' @return Lowercase hex string.
#' @noRd
bundle_sha256 <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  as.character(openssl::sha256(con))
}
