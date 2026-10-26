#' Read one item of Connect (internal) content
#'
#' @param guid The content GUID on the internal Connect server.
#' @param client A connectapi client, by default `phs_connect_int()`.
#' @return A list with `guid`, `name`, `title`, `owner_guid`, `app_mode`,
#'   `bundle_id` (character, `NA` if never deployed), `last_deployed_time`,
#'   `content_url` and `dashboard_url` (`NA` when the server gives none).
#' @export
get_int_content <- function(guid, client = phs_connect_int()) {
  check_guid(guid)

  content_summary(connectapi::content_item(client, guid))
}

#' Summarise a connectapi content item
#'
#' Flattens the fields from a connectapi `Content` object for easier use.
#' It is kept separate from `get_int_content()`` so a caller that already
#' holds the item doesn't request it again.
#'
#' @param item A connectapi `Content` object.
#' @return A list with `guid`, `name`, `title`, `owner_guid`, `app_mode`,
#'   `bundle_id` (`NA` if never deployed), `last_deployed_time`,
#'   `content_url` and `dashboard_url` (`NA` when the server gives none).
#' @noRd
content_summary <- function(item) {
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
  check_guid(guid)

  raw <- client$GET(paste0("v1/content/", guid, "/bundles"))
  data.frame(
    id = vapply(raw, function(b) flat_chr(b$id), character(1)),
    active = vapply(raw, function(b) isTRUE(b$active), logical(1)),
    size = vapply(raw, function(b) as.numeric(b$size %||% NA_real_), numeric(1)),
    created_time = vapply(raw, function(b) flat_chr(b$created_time), character(1)),
    stringsAsFactors = FALSE
  )
}

#' Flatten value from API into a single string
#'
#' NULL / missing becomes NA_character_ and anything else becomes a single string.
#' This maintains a single fixed shape for anything returned by the server.
#'
#' @param x A value from a parsed API response.
#' @return First element as a string or `NA_character_` for `NULL` or an empty value.
#' @noRd
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
download_bundle <- function(guid, dir, bundle_id = NULL, client = phs_connect_int()) {
  check_guid(guid)

  item <- connectapi::content_item(client, guid)

  if (is.null(bundle_id)) {
    bundle_id <- content_summary(item)$bundle_id
    if (is.na(bundle_id)) {
      cli::cli_abort(
        c(
          "Content {.val {guid}} has no active bundle to download.",
          "i" = paste(
            "Either it has never been deployed, or every deploy has failed, for example",
            "because the server couldn't install one of its packages."
          )
        ),
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

  suppressMessages(
    connectapi::download_bundle(item, filename = path, bundle_id = bundle_id, overwrite = TRUE)
  )

  list(path = path, guid = guid, bundle_id = bundle_id, sha256 = file_sha256(path))
}

#' Format a bundle id as text
#'
#' A number is written out in full (`100000`, never `1e+05`), so it matches
#' the ids Connect lists, and strings returned as they are.
#'
#' @param x A bundle id: a string or a single number.
#' @param call The environment of the calling function.
#' @return A string. Otherwise error of class `phsdeploy_bad_bundle_id`.
#' @noRd
format_bundle_id <- function(x, call = rlang::caller_env()) {
  if (!(is.character(x) || is.numeric(x)) || length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "{.arg bundle_id} must be a single bundle id.",
      class = c("phsdeploy_bad_bundle_id", "phsdeploy_error"),
      call = call
    )
  }

  if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
}

#' SHA-256 checksum of a file
#'
#' Read from a connection rather than into memory, so large bundles don't need to fit
#' in RAM. The checksum identifies the exact published bundle.
#'
#' @param path File path.
#' @return Checksum as lowercase hex string.
#' @noRd
file_sha256 <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  as.character(openssl::sha256(con))
}

#' Unpack a bundle safely
#'
#' A bundle comes from content created/owned by someone else, so any created
#' archive must not write outside `exdir`. This therefore checks:
#'
#' 1. `exdir` must be empty or missing, so nothing already there is mixed in
#'    or removed by mistake.
#' 2. Every entry type must be a regular file, a directory or a header entry
#'    (`0`, `""`, `5`, `x`, `g`, `L`), links and special files are refused.
#' 3. Every entry path must be relative and contain no `..`.
#' 4. After extraction, any symlink or a missing `manifest.json` removes what
#'    was unpacked.
#'
#' @param path Bundle `.tar.gz`.
#' @param exdir Target folder (created if missing, must be empty if present).
#' @return `exdir`, invisibly, otherwise error of class `phsdeploy_unsafe_bundle`,
#'   `phsdeploy_no_manifest` or `phsdeploy_no_bundle_file`.
#' @noRd
unpack_bundle <- function(path, exdir) {
  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort(
      "Bundle file {.path {path}} does not exist.",
      class = c("phsdeploy_no_bundle_file", "phsdeploy_error")
    )
  }

  if (dir.exists(exdir) && length(list.files(exdir, all.files = TRUE, no.. = TRUE))) {
    abort_unsafe_bundle("Refusing to unpack into {.path {exdir}}: the folder is not empty.")
  }

  # Pre-checks: nothing is extracted until these pass.
  types <- tar_entry_types(path)
  allowed <- c("", "0", "5", "g", "L", "x")
  odd <- types[!types$type %in% allowed, , drop = FALSE]
  if (nrow(odd)) {
    abort_unsafe_bundle(c(
      "The bundle contains links or special files and was not unpacked.",
      "x" = "Entries: {.val {odd$name}}."
    ))
  }

  tar <- system_tar()
  entries <- tryCatch(
    utils::untar(path, list = TRUE, tar = tar),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  if (is.null(entries)) {
    abort_unsafe_bundle("The bundle could not be read as a tar archive.")
  }
  bad <- entries[vapply(entries, is_unsafe_entry, logical(1))]
  if (length(bad)) {
    abort_unsafe_bundle(c(
      "The bundle contains unsafe paths and was not unpacked.",
      "x" = "Unsafe entries: {.val {bad}}."
    ))
  }

  created <- !dir.exists(exdir)
  fs::dir_create(exdir)

  ok <- tryCatch(
    utils::untar(path, exdir = exdir, tar = tar) == 0L,
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  if (!ok) {
    abort_unpack(exdir, created, "The bundle could not be unpacked cleanly and was removed.")
  }

  links <- fs::dir_ls(exdir, recurse = TRUE, all = TRUE, type = "symlink")
  if (length(links)) {
    rel <- as.character(fs::path_rel(links, exdir))
    abort_unpack(exdir, created, c(
      "The bundle contains symbolic links and was not unpacked.",
      "x" = "Links: {.val {rel}}."
    ))
  }

  if (!file.exists(file.path(exdir, "manifest.json"))) {
    abort_unpack(exdir, created, "The bundle has no {.file manifest.json} at its root.",
      class = "phsdeploy_no_manifest"
    )
  }

  invisible(exdir)
}

#' Path to the system tar
#'
#' The system tar is used rather than R's internal one, because the internal
#' tar can't read some pax archives. Listing and extracting both use it, so
#' the path check sees the same names that will be written.
#'
#' @return The path to `tar`. Otherwise an error of class `phsdeploy_no_tar`.
#' @noRd
system_tar <- function() {
  tar <- Sys.which("tar")
  if (!nzchar(tar)) {
    cli::cli_abort("No {.code tar} program was found on this system.",
      class = c("phsdeploy_no_tar", "phsdeploy_error")
    )
  }
  unname(tar)
}

#' Remove a partly unpacked bundle, then raise an error
#'
#' Called when extraction fails or the result is unsafe, so no half-checked
#' files are left for later steps to use. A folder this call created is
#' deleted and an empty folder that already existed is emptied but kept.
#'
#' @param exdir The folder being unpacked into.
#' @param created `TRUE` if `unpack_bundle()` created `exdir`.
#' @param msg A cli message: a string or a named bullet vector.
#' @param class The error class.
#' @param .envir The environment where `msg`'s `{}` values are found,
#'   default the caller.
#' @return None, raises an error of class `class`.
#' @noRd
abort_unpack <- function(exdir, created, msg, class = "phsdeploy_unsafe_bundle", .envir = parent.frame()) {
  if (created) {
    fs::dir_delete(exdir)
  } else {
    unlink(list.files(exdir, all.files = TRUE, no.. = TRUE, full.names = TRUE),
      recursive = TRUE
    )
  }
  cli::cli_abort(msg, class = c(class, "phsdeploy_error"), .envir = .envir)
}

#' Raise an unsafe-bundle error
#'
#' Gives every refusal from `unpack_bundle()` the same class, so callers and
#' tests can catch them as one kind of failure.
#'
#' @param msg A cli message: a string or a named bullet vector.
#' @param .envir The environment where `msg`'s `{}` values are found,
#'   default the caller's.
#' @return None, raises an error of class `phsdeploy_unsafe_bundle`.
#' @noRd
abort_unsafe_bundle <- function(msg, .envir = parent.frame()) {
  cli::cli_abort(msg, class = c("phsdeploy_unsafe_bundle", "phsdeploy_error"), .envir = .envir)
}

#' Read the entry names and type flags of a gzipped tar archive
#'
#' Walks the 512-byte headers directly, so the types are known before any
#' extraction. Data blocks are skipped in 1 MB steps, so large bundles aren't
#' read into memory.
#'
#' @param path Path to the bundle `.tar.gz`.
#' @return A data frame with `name` and `type`, one row per entry. Otherwise,
#' error of class `phsdeploy_unsafe_bundle` if archive truncated or corrupt.
#' @noRd
tar_entry_types <- function(path) {
  corrupt <- function() abort_unsafe_bundle("The bundle is truncated or is not a valid tar archive.")
  con <- tryCatch(gzfile(path, "rb"), error = function(e) corrupt(), warning = function(w) corrupt())
  on.exit(close(con), add = TRUE)

  # Read bytes
  read <- function(n) {
    tryCatch(readBin(con, "raw", n), error = function(e) corrupt(), warning = function(w) corrupt())
  }

  # Skip bytes
  skip <- function(n) {
    while (n > 0) {
      step <- min(n, 1048576)
      if (length(read(step)) < step) corrupt()
      n <- n - step
    }
  }

  names <- character()
  types <- character()
  zero_blocks <- 0L
  repeat {
    block <- read(512L)
    if (length(block) == 0L) break
    if (length(block) < 512L) corrupt()
    if (all(block == as.raw(0))) {
      zero_blocks <- zero_blocks + 1L
      if (zero_blocks >= 2L) break
      next
    }
    zero_blocks <- 0L
    size_txt <- trimws(tar_field(block, 125L, 136L))
    size <- if (nzchar(size_txt)) strtoi(size_txt, base = 8L) else 0L
    if (is.na(size)) corrupt()
    flag <- block[157L]
    type <- if (flag == as.raw(0)) "" else rawToChar(flag)
    names <- c(names, tar_field(block, 1L, 100L))
    types <- c(types, type)
    # Link, device, directory and FIFO entries (1-6) don't have data blocks,
    # every other type is followed by `size` bytes of data.
    if (!type %in% c("1", "2", "3", "4", "5", "6")) {
      skip(ceiling(size / 512) * 512)
    }
  }
  if (zero_blocks == 0L && length(names) == 0L) corrupt()
  data.frame(name = names, type = types, stringsAsFactors = FALSE)
}

#' Read a text field from a tar header block
#'
#' Header fields are fixed-width and end at the first NUL byte, so bytes from
#' the first NUL onwards are dropped.
#'
#' @param block A 512-byte raw tar header.
#' @param from,to The field's first and last byte positions (1-based).
#' @return The field as a string (`""` if it is empty).
#' @noRd
tar_field <- function(block, from, to) {
  b <- block[from:to]
  rawToChar(b[cumsum(b == as.raw(0)) == 0L])
}

#' Check tar entry path for extract safety
#'
#' An entry is unsafe if it is absolute (a leading `/` or `\`, or a drive
#' letter such as `C:`) or contains a `..` part, since either could write
#' outside the target folder. Both separators are checked, because an archive
#' made on Windows can use `\`.
#'
#' @param entry One entry path from the archive listing.
#' @return `TRUE` if the entry is unsafe, otherwise `FALSE`.
#' @noRd
is_unsafe_entry <- function(entry) {
  if (grepl("^(/|\\\\|[A-Za-z]:)", entry)) {
    return(TRUE)
  }
  ".." %in% strsplit(entry, "[/\\\\]")[[1]]
}
