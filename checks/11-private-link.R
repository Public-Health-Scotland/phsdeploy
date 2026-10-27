# Check 11: share a private product by private link.
#
# Uses the private item from 06-deploy-private.R. It switches private link
# sharing on, then finds which form of link opens the item without signing
# in, since Connect Cloud doesn't document it. It deploys an update while the
# link is live, which must keep sharing on, then switches sharing off and on
# again to see whether the link stays the same.
#
# A link "opens" the item when it ends on the item's own address after any
# redirects, rather than on the sign-in page. The token is never printed.
#
# Needs: 06-deploy-private.R.
#
# Changes: private link sharing is on for the item from 06 for the length of
# this check, and off again at the end, even if a step fails. The item stays
# private throughout.

source("checks/_helpers.R")

client <- phs_connect_int()
guid <- check_need("int_basic_guid", "02-publish-internal.R")
cloud_id <- check_need("cloud_basic_id", "06-deploy-private.R")
candidate_params <- unique(c(
  cloud_private_link_param, "private_link_token", "token", "share_token", "access_token"
))

#' Read an item's private link token straight from the API
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param id The item's id.
#' @return The token, or `NA`.
link_token <- function(cld, id) {
  repo_value(cloud_json(cloud_call(cld, "GET", paste0("/contents/", id)))$private_link_token)
}

#' Does a link open the item without signing in?
#'
#' Follows redirects, keeping cookies, as a browser would. Some links set a
#' cookie and redirect to the plain address, so the first status alone can't
#' tell a working link from a sign-in redirect.
#'
#' @param url The link to try.
#' @param host The item's own host.
#' @return A list with `opens` (`TRUE` if the request ends on `host` with 200),
#'   and `status` and `host` (where it ended).
link_opens <- function(url, host) {
  req <- httr2::request(url)
  req <- httr2::req_cookie_preserve(req, tempfile())
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req, verbosity = 0L), error = function(e) NULL)
  if (is.null(resp)) {
    return(list(opens = FALSE, status = "no response", host = NA_character_))
  }
  final_host <- httr2::url_parse(resp$url)$hostname
  list(
    opens = httr2::resp_status(resp) == 200L && identical(final_host, host),
    status = as.character(httr2::resp_status(resp)), host = final_host
  )
}

local({
  local_rsconnect_config()
  cld <- phs_connect_cld()

  check_confirm(paste("switch on private link sharing for the private item", cloud_id))
  on.exit(try(cloud_set_private_link(cld, cloud_id, FALSE), silent = TRUE), add = TRUE)

  check_step("Switch private link sharing on")
  link <- cloud_private_link(cld, cloud_id)
  host <- httr2::url_parse(link$base_url)$hostname
  access <- cloud_get_content(cld, cloud_id)$access
  if (!identical(access, "private")) stop("The item's access is ", access, ", not private.")
  check_ok("Sharing is on, and the item is still private. Its address is {.url {link$base_url}}.")
  plain <- link_opens(link$base_url, host)
  if (plain$opens) stop("The address opens without a link, so the item isn't private.")
  check_ok("Without the link, the address goes to sign-in (ends {plain$status} on {.val {plain$host}}).")

  check_step("Find the form of link that opens it")
  token <- link_token(cld, cloud_id)
  works <- character()
  for (param in candidate_params) {
    result <- link_opens(private_link_url(link$base_url, token, param), host)
    if (result$opens) works <- c(works, param)
    cli::cli_text("{.field ?{param}=<token>}: {if (result$opens) 'opens the item' else 'goes to sign-in'} (ends {result$status} on {.val {result$host}})")
  }
  if (!length(works)) {
    stop(
      "None of the link forms tried opens the item. Copy the private link from the web ",
      "interface and send its form (host, path and parameter name, not the token).",
      call. = FALSE
    )
  }
  if (cloud_private_link_param %in% works) {
    check_ok("The package's link form ({.field ?{cloud_private_link_param}=}) opens the item.")
  } else {
    cli::cli_alert_warning(paste(
      "The package builds links with {.field ?{cloud_private_link_param}=}, which doesn't open",
      "the item. Set {.code cloud_private_link_param} to {.val {works[[1]]}} in R/private_link.R."
    ))
  }

  check_step("Deploy an update while the link is live (expect sharing to stay on)")
  result <- check_quietly(
    "Deploy an update with private link sharing on",
    deploy_bundle(guid, cloud_id = cloud_id, client = client, cld = cld)
  )
  if (!isTRUE(result$private_link_enabled)) stop("The update switched private link sharing off.")
  after_update <- link_opens(private_link_url(link$base_url, token, works[[1]]), host)
  if (!after_update$opens) stop("The link stopped working after the update.")
  check_ok("Updated it. Sharing is still on, and the link still opens it.")

  check_step("Switch sharing off (expect the link to stop working)")
  cloud_set_private_link(cld, cloud_id, FALSE)
  off <- link_opens(private_link_url(link$base_url, token, works[[1]]), host)
  if (off$opens) stop("The link still opens the item with sharing off.")
  check_ok("Sharing is off, and the link goes to sign-in.")

  check_step("Switch sharing on again (does the link stay the same?)")
  cloud_set_private_link(cld, cloud_id, TRUE)
  same <- identical(link_token(cld, cloud_id), token)
  if (same) {
    check_ok("The token is the same, so links shared before still work after switching back on.")
  } else {
    cli::cli_alert_warning("The token changed, so links shared before stop working after switching off and on.")
  }

  check_step("Switch sharing off again")
  cloud_set_private_link(cld, cloud_id, FALSE)
  check_ok("Sharing is off. The item is private, with no link.")
})

check_done(11)
