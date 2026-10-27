# Check 8: first deploy to a custom address.
#
# Deploys to a custom name on the PHS wildcard domain. The item stays
# private, so its address should redirect to the sign-in page (302 or 307).
#
# Uses the basic app, as a second Cloud item. Set
# PHSDEPLOY_CHECK_CUSTOM_APP=phsshiny to use the phsshiny app instead, which
# also tests that Connect Cloud can install phsshiny from its private GitHub
# repository. That needs the Connect Cloud GitHub App to have access to the
# repository, and that access recorded for the session with
# options(phsdeploy.cloud_github_access = "Public-Health-Scotland").
#
# Needs: 02-publish-internal.R.
#
# Changes: creates one private Cloud item (removed by 14-delete.R).

source("checks/_helpers.R")

client <- phs_connect_int()
app <- Sys.getenv("PHSDEPLOY_CHECK_CUSTOM_APP", "basic")
if (!app %in% c("basic", "phsshiny")) stop("PHSDEPLOY_CHECK_CUSTOM_APP must be basic or phsshiny.")
guid <- check_need(paste0("int_", app, "_guid"), "02-publish-internal.R")
internal_title <- check_need(paste0("int_", app, "_title"), "02-publish-internal.R")
title <- paste(internal_title, "custom address")
name <- paste0("phsdeploy-check-", check_stamp())
check_save(custom_app = app, custom_guid = guid, custom_internal_title = internal_title)

local({
  cld <- phs_connect_cld()

  check_step("Find the wildcard domain")
  domains <- cloud_list_domains(cld)
  wildcard <- domains[domains$wildcard %in% TRUE, , drop = FALSE]
  if (nrow(wildcard) != 1L) stop("Expected exactly one wildcard domain, found ", nrow(wildcard), ".")
  url <- cloud_public_url(wildcard, name)
  check_ok("Found the wildcard domain {.val {wildcard$name}}. Address: {.url {url}}")

  check_confirm(paste("create a private Cloud item at", url, "with the", app, "app"))

  check_step("Deploy (expect the address above, access: private)")
  result <- check_deploy(
    "cloud_custom_id", guid,
    title = title, domain_id = wildcard$id, name = name, client = client, cld = cld
  )
  check_save(cloud_custom_url = result$url)
  check_show_result(result)
  status <- check_public_status(result$url)
  if (!status %in% c("302", "307")) stop("Expected a 302 or 307 from the private address, got ", status, ".")
  check_ok("Deployed to {.url {result$url}}, which redirects to sign-in ({status}).")
})

check_done(8)
