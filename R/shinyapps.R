#' List the apps on the shinyapps.io account
#'
#' Read only. shinyapps.io is closing, and its apps are moving to Connect
#' Cloud, so phsdeploy only lists them, to help decide what moves and what is
#' archived. Nothing on shinyapps.io is ever changed.
#'
#' shinyapps.io signs each request with the token's secret, which rsconnect
#' does for registered accounts. So, like [deploy_bundle()], this registers
#' the account with rsconnect in a temporary folder that is removed when it
#' returns, and nothing lasting is written. That changes environment
#' variables for the whole R process while it runs.
#'
#' @param config A `phsdeploy_config` with an `sio` part, by default read from
#'   the environment with `phsdeploy_config("sio")`, or built with
#'   [phsdeploy_sio_config()].
#'
#' @return A data frame with one row per app: `id`, `name`, `url`, `status`
#'   (such as `"running"`, `"sleeping"` or `"archived"`), `created` and
#'   `updated` (both `POSIXct`, in UTC).
#'
#'   Otherwise an error of class `phsdeploy_sio_error`, also of class
#'   `phsdeploy_sio_unreachable` when the network failed twice rather than
#'   the token being refused.
#' @export
#' @examples
#' \dontrun{
#' apps <- sio_list_apps()
#' apps[apps$status != "archived", ]
#' }
sio_list_apps <- function(config = phsdeploy_config("sio")) {
  check_config_part(config, "sio")
  sio <- config$sio
  local_rsconnect_config()
  check_rsconnect_config_temporary()
  local_rsconnect_http()

  apps <- sio_attempt(sio)
  if (is_network_failure(apps)) {
    Sys.sleep(2)
    apps <- sio_attempt(sio)
  }
  if (inherits(apps, "error")) {
    network <- is_network_failure(apps)
    cli::cli_abort(
      c(
        if (network) {
          "Could not reach shinyapps.io, even after a retry."
        } else {
          "shinyapps.io refused the request."
        },
        "x" = "{squish_scrub(conditionMessage(apps), c(sio$token, sio$secret))}",
        "i" = if (!network) "Check the shinyapps.io account, token and secret."
      ),
      class = c(if (network) "phsdeploy_sio_unreachable", "phsdeploy_sio_error", "phsdeploy_error")
    )
  }
  sio_apps_frame(apps)
}

#' Register the shinyapps.io account and list its apps, once
#'
#' Expects rsconnect's folders to be temporary already.
#'
#' @param sio The `sio` part of a `phsdeploy_config`.
#' @return The data frame from `rsconnect::applications()`, or the error
#'   rsconnect raised.
#' @noRd
sio_attempt <- function(sio) {
  tryCatch(
    {
      suppressMessages(rsconnect::setAccountInfo(
        name = sio$account, token = sio$token, secret = sio$secret, server = "shinyapps.io"
      ))
      rsconnect::applications(account = sio$account, server = "shinyapps.io")
    },
    error = function(e) e
  )
}

#' Tidy rsconnect's list of shinyapps.io apps
#'
#' @param apps The data frame from `rsconnect::applications()`.
#' @return A data frame with `id`, `name`, `url`, `status`, `created` and
#'   `updated`.
#' @noRd
sio_apps_frame <- function(apps) {
  as_time <- function(x) {
    as.POSIXct(substr(as.character(x), 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  }
  column <- function(name) if (name %in% names(apps)) apps[[name]] else rep(NA, nrow(apps))
  data.frame(
    id = as.character(column("id")),
    name = as.character(column("name")),
    url = as.character(column("url")),
    status = as.character(column("status")),
    created = as_time(column("created_time")),
    updated = as_time(column("updated_time")),
    stringsAsFactors = FALSE
  )
}
