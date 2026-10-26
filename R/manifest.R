#' Default map from internal PHS to public package repositories
#'
#' Each name is an internal repo base, each value is the public repo that
#' serves the same packages under the same paths. The CRAN mirror has the
#' public equivalent, so it is the repo listed, override with
#' `options(phsdeploy.repo_map = )`.
#'
#' @return A named character vector: internal base = public base.
#' @noRd
default_repo_map <- function() {
  c(
    "https://ppm.publichealthscotland.org/cran" = "https://packagemanager.posit.co/cran",
    "https://ppm-prod.publichealthscotland.org/cran" = "https://packagemanager.posit.co/cran"
  )
}

#' Hosts on the PHS internal network
#'
#' Connect Cloud can't reach these, so a manifest that still refers to any of
#' them after rewriting can't be deployed.
#'
#' @return A character vector of host names.
#' @noRd
internal_hosts <- function() {
  c(
    "publichealthscotland.org",
    "phs-git.nhsnss.scot.nhs.uk"
  )
}

#' A pattern matching any internal host in a URL
#'
#' Matches a host only as a whole name (or a subdomain of one), so a public
#' host containing an internal name doesn't match.
#'
#' @param hosts Host names, as from `internal_hosts()`.
#' @return A single regular expression string.
#' @noRd
host_pattern <- function(hosts) {
  escaped <- gsub(".", "\\.", hosts, fixed = TRUE)
  paste0("(^|[/@.])(", paste(escaped, collapse = "|"), ")($|[/:])")
}

#' Rewrite internal package repositories in a manifest
#'
#' A manifest records the repository each package was installed from when the
#' content was first deployed. For PHS content that is usually the internal
#' package manager, which Connect Cloud cannot reach. This rewrites each
#' package's `Repository` and `description$Repository` to the public
#' equivalent from `map`, keeping the snapshot date so the same package
#' versions are installed.
#'
#' Afterwards, every text field of every package entry is checked against
#' `internal`. Any match (for example a package from a non-CRAN
#' repo, or one installed from PHS Gitea) means Connect Cloud could not
#' install it, so the manifest is left unchanged and an error is raised.
#'
#' @param manifest_path Path to `manifest.json`.
#' @param map A named character vector: internal base = public base. See
#'   `default_repo_map()`.
#' @param internal Host names on the internal network.
#' @return Invisibly, a data frame with `package`, `field`, `from` and `to`,
#'   one row per value changed. Otherwise an error of class
#'   `phsdeploy_no_manifest`, `phsdeploy_bad_manifest` or `phsdeploy_unmapped_repo`.
#' @noRd
rewrite_manifest_repos <- function(manifest_path,
                                   map = getOption("phsdeploy.repo_map", default_repo_map()),
                                   internal = getOption(
                                     "phsdeploy.internal_hosts",
                                     internal_hosts()
                                   )) {
  manifest <- read_manifest(manifest_path)
  changes <- data.frame(
    package = character(), field = character(), from = character(), to = character()
  )
  packages <- manifest[["packages"]]
  if (!is.list(packages) || !length(packages)) {
    return(invisible(changes))
  }

  unmapped <- character()
  pattern <- host_pattern(internal)

  for (name in names(packages)) {
    entry <- packages[[name]]
    if (!is.list(entry)) next

    from <- repo_value(entry[["Repository"]])
    to <- map_repo(from, map)
    if (!is.na(to)) {
      entry[["Repository"]] <- to
      changes[nrow(changes) + 1L, ] <- list(name, "Repository", from, to)
    }

    if (is.list(entry[["description"]])) {
      from <- repo_value(entry[["description"]][["Repository"]])
      to <- map_repo(from, map)
      if (!is.na(to)) {
        entry[["description"]][["Repository"]] <- to
        changes[nrow(changes) + 1L, ] <- list(name, "description.Repository", from, to)
      }
    }

    packages[[name]] <- entry
    fields <- internal_fields(entry, pattern)
    if (length(fields)) {
      unmapped <- c(unmapped, paste0(name, " (", paste(fields, collapse = ", "), ")"))
    }
  }

  if (length(unmapped)) {
    cli::cli_abort(
      c(
        "Some packages refer to PHS internal servers, which Connect Cloud cannot reach.",
        "x" = "Packages (fields): {.val {unmapped}}.",
        "i" = "Reinstall them from CRAN or public GitHub and republish the content, or add a public equivalent with {.code options(phsdeploy.repo_map = )}."
      ),
      class = c("phsdeploy_unmapped_repo", "phsdeploy_error"),
      packages = unmapped
    )
  }

  if (nrow(changes)) {
    manifest[["packages"]] <- packages
    write_manifest(manifest, manifest_path)
  }
  invisible(changes)
}

#' Read a manifest file
#'
#' @param path Path to `manifest.json`.
#' @return The manifest as a list, with JSON arrays kept as lists so that
#'   writing it back gives the same structure. Otherwise an error of class
#'   `phsdeploy_no_manifest` or `phsdeploy_bad_manifest`.
#' @noRd
read_manifest <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "Manifest {.file {path}} does not exist.",
      class = c("phsdeploy_no_manifest", "phsdeploy_error")
    )
  }
  manifest <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        "Manifest {.file {path}} is not valid JSON.",
        class = c("phsdeploy_bad_manifest", "phsdeploy_error"),
        parent = e
      )
    }
  )
  if (!is.list(manifest) || is.null(names(manifest))) {
    cli::cli_abort(
      "Manifest {.file {path}} is not a JSON object.",
      class = c("phsdeploy_bad_manifest", "phsdeploy_error")
    )
  }
  manifest
}

#' Map one repository URL to its public equivalent
#'
#' Finds the longest base in `map` that `url` starts with, and swaps that base
#' for its public value, keeping the rest of the path. Any `__linux__/<distro>`
#' part is removed first. That part asks for binaries built for the original
#' server's operating system, removing allows Connect Cloud's package manager
#' to serve the right binaries for Cloud.
#'
#' For example, with the default map:
#' * `.../cran/latest` becomes `https://packagemanager.posit.co/cran/latest`
#' * `.../cran/__linux__/jammy/2024-05-01` becomes
#'   `https://packagemanager.posit.co/cran/2024-05-01`
#'
#' @param url A repository URL, or `NA`.
#' @param map A named character vector: internal base = public base.
#' @return The public URL, or `NA` if `url` is `NA` or matches no base.
#' @noRd
map_repo <- function(url, map) {
  if (is.na(url)) {
    return(NA_character_)
  }
  url <- sub("/+$", "", url)
  url <- sub("/__linux__/[^/]+(/|$)", "\\1", url)
  bases <- sub("/+$", "", names(map))
  hit <- which(url == bases | startsWith(url, paste0(bases, "/")))
  if (!length(hit)) {
    return(NA_character_)
  }
  hit <- hit[which.max(nchar(bases[hit]))]
  paste0(unname(map[[hit]]), substring(url, nchar(bases[hit]) + 1L))
}

#' A repository value as one string
#'
#' Manifest fields can be missing, empty or not text. This keeps only a
#' single string, so later steps compare like with like.
#'
#' @param x A value from a manifest package entry.
#' @return `x` if it is one non-`NA` string, otherwise `NA_character_`.
#' @noRd
repo_value <- function(x) {
  if (rlang::is_string(x)) x else NA_character_
}

#' Fields of a package entry that refer to internal servers
#'
#' Checks every text value in the entry, including its `description`, so
#' internal references outside `Repository` are found too, for example a
#' `RemoteUrl` on the internal git server.
#'
#' @param entry One package entry from a manifest.
#' @param pattern A regular expression matching internal hosts.
#' @return The names of the matching fields, such as
#'   `"description.RemoteUrl"`. Empty if none match.
#' @noRd
internal_fields <- function(entry, pattern) {
  values <- unlist(entry)
  names(values)[grepl(pattern, values, ignore.case = TRUE)]
}

#' Write a manifest file safely
#'
#' Writes to a temporary file in the same folder, then renames it over
#' `path`. The rename is a single step, so a failure part-way through never
#' leaves a half-written manifest.
#'
#' @param manifest The manifest as a list, as from `read_manifest()`.
#' @param path Path to `manifest.json`.
#' @return `path`, invisibly. Otherwise an error of class `phsdeploy_error`.
#' @noRd
write_manifest <- function(manifest, path) {
  tmp <- tempfile(pattern = ".manifest-", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(manifest, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  if (!file.rename(tmp, path)) {
    cli::cli_abort("Could not replace {.file {path}}.", class = "phsdeploy_error")
  }
  invisible(path)
}

#' What a Connect Cloud item needs to know about a bundle
#'
#' Reads the app mode and primary file from the manifest. A new Cloud item is
#' created with both, so they must agree with what `rsconnect::deployApp()`
#' sends on each deploy. rsconnect uses `primary_rmd`, then `primary_html`, so
#' this checks them in the same order before falling back to `entrypoint` and
#' the usual file for the app mode.
#'
#' @param path Path to `manifest.json`.
#' @return A list with `app_mode` and `primary_file`. Otherwise an error of
#'   class `phsdeploy_bad_manifest` or `phsdeploy_unsupported_content`.
#' @noRd
manifest_target <- function(path) {
  meta <- read_manifest(path)[["metadata"]]
  mode <- if (is.list(meta)) meta[["appmode"]]
  if (!rlang::is_string(mode) || !nzchar(mode)) {
    cli::cli_abort(
      "Manifest {.file {path}} doesn't say what kind of content this is ({.field metadata.appmode}).",
      class = c("phsdeploy_bad_manifest", "phsdeploy_error")
    )
  }
  if (!mode %in% names(cloud_content_types)) {
    cli::cli_abort(
      c(
        "Posit Connect Cloud can't host content of type {.val {mode}}.",
        "i" = "Supported: {.val {names(cloud_content_types)}}."
      ),
      class = c("phsdeploy_unsupported_content", "phsdeploy_error"),
      app_mode = mode
    )
  }
  candidates <- list(
    meta[["primary_rmd"]], meta[["primary_html"]], meta[["entrypoint"]],
    switch(mode,
      shiny = "app.R",
      static = "index.html"
    )
  )
  primary <- Find(function(x) rlang::is_string(x) && nzchar(x), candidates)
  if (is.null(primary) || is_unsafe_entry(primary)) {
    cli::cli_abort(
      "Manifest {.file {path}} names no usable primary file for {.val {mode}} content.",
      class = c("phsdeploy_bad_manifest", "phsdeploy_error")
    )
  }
  list(app_mode = mode, primary_file = primary)
}
