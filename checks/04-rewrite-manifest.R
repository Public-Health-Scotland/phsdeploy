# Check 4: rewrite the unpacked bundle's manifest for Connect Cloud.
#
# Shows the package repositories before and after, and which values changed.
# A package from a repository Cloud can't reach stops the rewrite with a
# clear error instead.
#
# Needs: the bundle unpacked by 03-download-bundle.R.
#
# Changes: none. It works on the temporary copy only.

source("checks/_helpers.R")

dir <- check_need("unpacked_dir", "03-download-bundle.R")
manifest <- file.path(dir, "manifest.json")

#' Each package's repository in a manifest
#'
#' @param path Path to `manifest.json`.
#' @return A data frame with `package` and `repository`.
repositories <- function(path) {
  packages <- read_manifest(path)$packages
  data.frame(
    package = names(packages),
    repository = vapply(packages, function(p) p$Repository %||% NA_character_, character(1)),
    row.names = NULL
  )
}

check_step("Repositories before")
before <- repositories(manifest)
print(as.data.frame(table(repository = before$repository, useNA = "ifany")))

check_step("Rewrite")
changes <- tryCatch(
  rewrite_manifest_repos(manifest),
  phsdeploy_unmapped_repo = function(e) {
    cli::cli_alert_danger("The rewrite refused {length(e$packages)} package{?s}:")
    cli::cli_bullets(stats::setNames(e$packages, rep("*", length(e$packages))))
    cli::cli_text("")
    cli::cli_alert_info(paste(
      "If a repository above is the internal CRAN mirror under another path, map it to",
      "public CRAN for this session, then run this check again:"
    ))
    cli::cli_code(paste0(
      'options(phsdeploy.repo_map = c(default_repo_map(), ',
      '"<internal base>" = "https://packagemanager.posit.co/cran"))'
    ))
    cli::cli_alert_info(paste(
      "The internal base is the URL up to and including the repository name, without",
      "the date or {.code latest} part. Please send it on, so it can go in",
      "{.code default_repo_map()}."
    ))
    stop("Stopped: some packages can't be installed by Connect Cloud.", call. = FALSE)
  }
)
print(utils::head(changes, 10))
check_ok("Rewrote {nrow(changes)} value{?s}.")

check_step("Repositories after")
print(as.data.frame(table(repository = repositories(manifest)$repository, useNA = "ifany")))
check_ok("No package refers to an internal server.")

check_done(4)
