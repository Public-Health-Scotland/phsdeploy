#' Connect to PHS Connect internal
#'
#' Creates a connectapi client object.
#'
#' @param config A `phsdeploy_config` with an `int` part, by default read from
#'   the environment with `phsdeploy_config("int")`.
#'
#' @return A connectapi `Connect` object.
#' @export
phs_connect_int <- function(config = phsdeploy_config("int")) {
  connectapi::connect(
    server = config$int$url, 
    api_key = config$int$key
  )
}
