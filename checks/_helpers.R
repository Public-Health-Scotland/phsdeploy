# Shared helpers for the phsdeploy checks. Each check script sources this
# file first. It loads the package from source, so internal functions such as
# `download_bundle()` and `cloud_create_content()` can be called directly.
#
# Run the checks from the package root (the folder with DESCRIPTION), in one R
# session, in number order. What each script creates is recorded in a state
# file in `tempdir()`, so later scripts can find it. Restarting R forgets it.

if (!file.exists("DESCRIPTION") || !dir.exists("checks")) {
  stop("Run the checks from the phsdeploy package root.", call. = FALSE)
}
pkgload::load_all(".", quiet = TRUE)

#' Prefix of every title the checks create
#'
#' The delete check only removes items whose title starts with this, so it
#' can never remove anything else.
check_title_prefix <- "phsdeploy check"

#' Path of the file that records what the checks create
check_state_file <- file.path(tempdir(), "phsdeploy-checks.rds")

#' Path of the log of detailed output from the checks
#'
#' Publishing and deploying print a lot. That output goes here instead of the
#' console, so the console shows only the checks' own messages.
check_log_file <- file.path(tempdir(), "phsdeploy-checks.log")

#' Print a step heading
#'
#' @param ... Text to show, pasted together.
#' @return `NULL`, invisibly.
check_step <- function(...) {
  cli::cli_h2(paste0(...))
  invisible()
}

#' Report that something worked
#'
#' @param text A cli message, evaluated in the caller's environment.
#' @return `NULL`, invisibly.
check_ok <- function(text) {
  cli::cli_alert_success(text, .envir = parent.frame())
  invisible()
}

#' Report that a whole check passed
#'
#' @param number The check's number.
#' @return `NULL`, invisibly.
check_done <- function(number) {
  cli::cli_rule(left = paste("Check", number, "passed"))
  invisible()
}

#' Ask before a step that changes a server
#'
#' Skips the question when the environment variable `PHSDEPLOY_CHECK_CONFIRM`
#' is `yes`, which suits sourcing several checks in a row, or `Rscript`.
#' Otherwise it asks with `utils::askYesNo()`. RStudio shows that as a
#' dialog, so lines run after `source()` can't be read as the answer by
#' mistake.
#'
#' Only a clear yes continues. Pressing Enter, answering no, cancelling, or
#' giving an answer that isn't recognised all stop the check, before it
#' changes anything, and say so.
#'
#' @param what What is about to happen.
#' @return `TRUE`, invisibly. Otherwise stops the script.
check_confirm <- function(what) {
  cli::cli_alert_warning("About to {what}.")
  if (identical(tolower(Sys.getenv("PHSDEPLOY_CHECK_CONFIRM")), "yes")) {
    cli::cli_alert_info("Confirmed by {.envvar PHSDEPLOY_CHECK_CONFIRM}.")
    return(invisible(TRUE))
  }
  if (!interactive()) {
    check_stop_unconfirmed(
      "No one could be asked. Set {.envvar PHSDEPLOY_CHECK_CONFIRM} to {.val yes} to run without questions."
    )
  }
  answer <- tryCatch(
    utils::askYesNo("Continue? Only yes continues.", default = FALSE),
    error = function(e) "unrecognised"
  )
  if (identical(answer, "unrecognised")) {
    check_stop_unconfirmed("That answer wasn't recognised, so it was taken as no.")
  }
  if (is.na(answer)) {
    check_stop_unconfirmed("The question was cancelled.")
  }
  if (!isTRUE(answer)) {
    check_stop_unconfirmed("The answer was no.")
  }
  invisible(TRUE)
}

#' Stop a check because a change wasn't confirmed
#'
#' Says why, and that nothing was changed, so it's clear what state the
#' servers are in.
#'
#' @param why Why the check stopped, as cli text.
#' @return Does not return. Stops the script.
check_stop_unconfirmed <- function(why) {
  cli::cli_alert_danger(why, .envir = parent.frame())
  cli::cli_alert_info("Stopped before making this change. Nothing was changed by this step.")
  stop("Not confirmed.", call. = FALSE)
}

#' Run a noisy step with its output sent to the log
#'
#' Printed output, messages and warnings from `expr` go to `check_log_file`,
#' under a heading for the step. If the step fails, the last lines this step
#' logged are shown with the error, so the cause is still visible. Lines from
#' earlier steps are never shown, so they can't be mistaken for this step's.
#'
#' @param label What the step is, for the log heading and any failure.
#' @param expr The code to run.
#' @return The value of `expr`.
check_quietly <- function(label, expr) {
  # Lines already in the log belong to earlier steps.
  start <- if (file.exists(check_log_file)) length(readLines(check_log_file)) else 0L
  log <- file(check_log_file, open = "a")
  sink(log)
  sinking <- TRUE
  on.exit({
    if (sinking) sink()
    close(log)
  }, add = TRUE)
  cat("\n== ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", label, "\n", sep = "")
  result <- tryCatch(
    withCallingHandlers(
      expr,
      message = function(m) {
        cat(conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        cat("Warning:", conditionMessage(w), "\n")
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  sink()
  sinking <- FALSE
  if (inherits(result, "error")) {
    cli::cli_alert_danger("{label} failed. The last lines it logged:")
    step_lines <- readLines(check_log_file)[-seq_len(start)]
    cat(utils::tail(step_lines, 20), sep = "\n")
    cli::cli_alert_info("Full log: {.file {check_log_file}}")
    stop(result)
  }
  result
}

#' Everything the checks have recorded in this session
#'
#' @return A named list, empty if nothing is recorded yet.
check_state <- function() {
  if (file.exists(check_state_file)) readRDS(check_state_file) else list()
}

#' Record values for later checks
#'
#' @param ... Named values to record. Existing names are replaced.
#' @return `NULL`, invisibly.
check_save <- function(...) {
  state <- check_state()
  new <- list(...)
  state[names(new)] <- new
  saveRDS(state, check_state_file)
  invisible()
}

#' Get a recorded value, or stop with which script to run first
#'
#' @param name The recorded value's name.
#' @param script The script that records it.
#' @return The value.
check_need <- function(name, script) {
  value <- check_state()[[name]]
  if (is.null(value)) {
    stop("Nothing recorded for `", name, "`. Run ", script, " first.", call. = FALSE)
  }
  value
}

#' A timestamp for names, unique to the minute
#'
#' @return A string such as `"202610261530"`.
check_stamp <- function() {
  format(Sys.time(), "%Y%m%d%H%M", tz = "UTC")
}

#' Publish a test app to internal Connect
#'
#' Copies the app to a temporary folder, writes `data.csv` with the given data
#' version, writes a manifest from the local library (as a publisher's own
#' deploy would), then deploys the bundle. With `guid`, deploys a new bundle
#' to that existing item instead of creating one.
#'
#' With `save_as`, a new item's GUID is recorded as soon as it is created,
#' before its build finishes, so `12-delete.R` can remove it even if the build
#' fails.
#'
#' @param client A connectapi client from `phs_connect_int()`.
#' @param app The folder name under `checks/apps`.
#' @param title The item's title.
#' @param data_version The data version to show in the app.
#' @param guid `NULL` for a new item, or the GUID of an existing one.
#' @param save_as `NULL`, or the name to record a new item's GUID under.
#' @return The item's GUID.
check_publish_internal <- function(client, app, title, data_version = 1, guid = NULL,
                                   save_as = NULL) {
  # connectapi's upload leaves the bundle file open (see
  # `close_new_connections()`), so close it here. The note goes to the log.
  connections_before <- open_connection_ids()
  on.exit(close_new_connections(connections_before, "while publishing to internal Connect"), add = TRUE)
  dir <- withr::local_tempdir(pattern = "phsdeploy-check-app-")
  file.copy(list.files(file.path("checks", "apps", app), full.names = TRUE), dir)
  write.csv(
    data.frame(version = data_version, written = format(Sys.time(), "%Y-%m-%d %H:%M")),
    file.path(dir, "data.csv"),
    row.names = FALSE
  )
  rsconnect::writeManifest(dir, quiet = TRUE)
  bundle <- connectapi::bundle_dir(dir)
  name <- gsub("[^a-z0-9-]+", "-", tolower(title))
  content <- connectapi::deploy(client, bundle, name = name, title = title, guid = guid)
  if (!is.null(save_as)) do.call(check_save, stats::setNames(list(content$content$guid), save_as))
  connectapi::poll_task(content)
  content$content$guid
}

#' Deploy to Connect Cloud, recording the item for the delete check
#'
#' Calls `deploy_bundle()`, with its output sent to the log. The item's id is
#' recorded under `save_as` whether the deploy succeeds or fails after
#' creating the item, so `12-delete.R` can always remove it.
#'
#' If an earlier run recorded an item under `save_as` and it still exists,
#' this deploys into it as an update instead of creating another. That
#' finishes a first deploy that failed part-way, so running the check again is
#' the way to retry.
#'
#' @param save_as The name to record the Cloud item's id under.
#' @param guid The internal content's GUID.
#' @param client A connectapi client for internal Connect.
#' @param cld A `phsdeploy_cld` connection.
#' @param ... Passed to `deploy_bundle()` for a first deploy.
#' @return The result of `deploy_bundle()`.
check_deploy <- function(save_as, guid, client, cld, ...) {
  record <- function(cloud_id) do.call(check_save, stats::setNames(list(cloud_id), save_as))
  existing <- check_state()[[save_as]]
  if (!is.null(existing) && !is.null(cloud_get_content(cld, existing))) {
    cli::cli_alert_info("Resuming: deploying into item {.val {existing}} from an earlier run.")
    deploy <- function() deploy_bundle(guid, cloud_id = existing, client = client, cld = cld)
  } else {
    deploy <- function() deploy_bundle(guid, client = client, cld = cld, ...)
  }
  result <- check_quietly(
    "Deploy to Connect Cloud",
    withCallingHandlers(
      deploy(),
      phsdeploy_deploy_failed = function(e) record(e$cloud_id)
    )
  )
  record(result$cloud_id)
  result
}

#' Show the main fields of a deploy result
#'
#' @param result The result of `deploy_bundle()`.
#' @return `result`, invisibly.
check_show_result <- function(result) {
  fields <- c(
    "cloud_id", "first_deploy", "title", "access", "private_link_enabled", "bundle_id",
    "url", "default_url"
  )
  for (field in fields) {
    cat(sprintf("  %-14s %s\n", paste0(field, ":"), format(result[[field]])))
  }
  cat(sprintf("  %-14s %d\n", "repo changes:", nrow(result$repo_changes)))
  invisible(result)
}

#' Show the fields of a Cloud item that the checks care about
#'
#' Includes any field whose name contains `url`, since the API doesn't
#' document where an item's addresses are.
#'
#' @param item A Cloud item from `cloud_get_content()`.
#' @return `item`, invisibly.
check_show_item <- function(item) {
  fields <- c("id", "title", "access", "state", "vanity_name", "domain_id")
  fields <- c(fields, grep("url", names(item), value = TRUE))
  for (field in fields) {
    value <- item[[field]]
    if (is.null(value)) value <- "<none>"
    if (is.list(value)) value <- "<list>"
    cat(sprintf("  %-14s %s\n", paste0(field, ":"), value))
  }
  invisible(item)
}

#' HTTP status of a public address, without signing in
#'
#' Doesn't follow redirects, so a private item shows as a redirect (302 or
#' 307) to the sign-in page, and a public one as 200.
#'
#' @param url The address to check.
#' @return The status as a string, or `"no response"`.
check_public_status <- function(url) {
  req <- httr2::request(url)
  req <- httr2::req_options(req, followlocation = 0L)
  req <- httr2::req_timeout(req, 20)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req, verbosity = 0L), error = function(e) NULL)
  if (is.null(resp)) "no response" else as.character(httr2::resp_status(resp))
}

#' Wait for a public address to give an expected status
#'
#' A changed address can take a little while to start answering, so this asks
#' every 5 seconds until the status is one of `expected` or `wait` seconds have
#' passed.
#'
#' @param url The address to check.
#' @param expected The statuses to wait for, as strings, such as `"200"`.
#' @param wait The most seconds to wait.
#' @return The last status seen, as a string.
check_wait_status <- function(url, expected, wait = 60) {
  deadline <- Sys.time() + wait
  repeat {
    status <- check_public_status(url)
    if (status %in% expected || Sys.time() >= deadline) {
      return(status)
    }
    Sys.sleep(5)
  }
}

#' Check a public address answers, redeploying once if it doesn't
#'
#' A new item's address only answers after its first deploy. This finds out
#' whether a renamed address answers straight away, or needs a redeploy too,
#' and reports which.
#'
#' @param url The address, which should answer 200 while the item is public.
#' @param guid The internal content's GUID, for a redeploy.
#' @param cloud_id The Cloud item.
#' @param client A connectapi client for internal Connect.
#' @param cld A `phsdeploy_cld` connection.
#' @return `TRUE`, invisibly. Otherwise stops the check.
check_address_answers <- function(url, guid, cloud_id, client, cld) {
  status <- check_wait_status(url, "200")
  if (identical(status, "200")) {
    check_ok("{.url {url}} answers 200 straight away.")
    return(invisible(TRUE))
  }
  cli::cli_alert_warning("{.url {url}} answers {status}, not 200. Trying a redeploy.")
  check_quietly(
    "Redeploy after a rename",
    deploy_bundle(guid, cloud_id = cloud_id, client = client, cld = cld)
  )
  status <- check_wait_status(url, "200")
  if (!identical(status, "200")) {
    stop("The address ", url, " answers ", status, " even after a redeploy.", call. = FALSE)
  }
  check_ok("{.url {url}} answers 200, but only after a redeploy.")
  invisible(TRUE)
}
