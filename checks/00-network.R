# Check 0: test the network route to each server the deploy uses.
#
# Opens a connection to each host five times and reports how long the
# connection and the whole request took, and any failures. rsconnect gives up
# on a connection that takes over 10 seconds to open (phsdeploy allows 60), so
# slow or failing connections here explain deploys that time out. The results
# are useful evidence for whoever looks after the network.
#
# Needs: PHSDEPLOY_INT_URL, for the internal server.
#
# Changes: none.

source("checks/_helpers.R")

hosts <- c(
  "Connect Cloud API" = "https://api.connect.posit.cloud/v1",
  "Connect Cloud sign-in" = "https://login.posit.cloud",
  "GitHub API" = "https://api.github.com",
  "Public package manager" = "https://packagemanager.posit.co",
  "Internal Connect" = Sys.getenv("PHSDEPLOY_INT_URL")
)
hosts <- hosts[nzchar(hosts)]

check_step("Proxy settings")
proxies <- c("HTTPS_PROXY", "https_proxy", "HTTP_PROXY", "http_proxy", "NO_PROXY", "no_proxy")
set <- proxies[nzchar(Sys.getenv(proxies))]
if (length(set)) {
  cli::cli_alert_info("Set: {.envvar {set}}. Requests go through the proxy.")
} else {
  cli::cli_alert_info("No proxy variables set. Requests go direct.")
}

#' Time one request to a URL, without signing in
#'
#' @param url The address to request.
#' @return A list with `ok`, `status`, `connect` and `total` (seconds), and
#'   `error` (the failure text, or `NA`).
time_request <- function(url) {
  req <- httr2::request(url)
  # req_timeout() resets the connection limit, so it comes first. A fresh
  # connection each time means every try measures opening one.
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_options(
    req,
    connecttimeout = 60, followlocation = 0L, fresh_connect = 1L, forbid_reuse = 1L
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req, verbosity = 0L), error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, status = NA, connect = NA, total = NA, error = conditionMessage(resp)))
  }
  timing <- httr2::resp_timing(resp)
  list(
    ok = TRUE, status = httr2::resp_status(resp),
    connect = unname(timing[["connect"]]), total = unname(timing[["total"]]), error = NA
  )
}

check_step("Connections (5 tries each)")
slow <- FALSE
failed <- FALSE
for (name in names(hosts)) {
  tries <- lapply(1:5, function(i) time_request(hosts[[name]]))
  ok <- vapply(tries, `[[`, logical(1), "ok")
  connect <- vapply(tries[ok], `[[`, numeric(1), "connect")
  total <- vapply(tries[ok], `[[`, numeric(1), "total")
  if (all(ok)) {
    check_ok(
      "{name}: 5 of 5 answered. Connect {round(min(connect), 2)} to {round(max(connect), 2)} s, whole request up to {round(max(total), 2)} s."
    )
  } else {
    failed <- TRUE
    errors <- unique(vapply(tries[!ok], `[[`, character(1), "error"))
    cli::cli_alert_danger("{name}: {sum(ok)} of 5 answered. Failures:")
    cli::cli_bullets(stats::setNames(scrub_text(errors), rep("x", length(errors))))
  }
  if (length(connect) && max(connect) > 5) slow <- TRUE
}

if (slow) {
  cli::cli_alert_warning(paste(
    "Some connections took over 5 seconds to open. That's close to rsconnect's",
    "own 10-second limit, so deploys may time out on a bad day."
  ))
}
if (failed) stop("Some servers couldn't be reached. See the failures above.", call. = FALSE)
check_done(0)
