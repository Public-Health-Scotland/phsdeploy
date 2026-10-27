# Check 9: make a product public, then update it while it is live.
#
# Making a product public is its own step, separate from deployment: can
# be handled at the release time for a product deployed ahead of release,
# or straight after the first deploy for one going straight to live.
# Any update must then leave it public.
#
# Needs: 08-custom-domain.R.
#
# Changes: private Cloud item from 08 made public (anyone-can-view) for
# the length of this check, and is made private again at the end.

source("checks/_helpers.R")

client <- phs_connect_int()
app <- check_need("custom_app", "08-custom-domain.R")
guid <- check_need("custom_guid", "08-custom-domain.R")
internal_title <- check_need("custom_internal_title", "08-custom-domain.R")
cloud_id <- check_need("cloud_custom_id", "08-custom-domain.R")
url <- check_need("cloud_custom_url", "08-custom-domain.R")
app_dir <- c(basic = "basic-shiny", phsshiny = "phsshiny-app")[[app]]

local({
  local_rsconnect_config()
  cld <- phs_connect_cld()

  check_confirm(paste("make", url, "public (anyone can view it)"))

  check_step("Make it public")
  cloud_set_access(cld, cloud_id, "public")
  status <- check_public_status(url)
  if (!identical(status, "200")) stop("Expected 200 from the public address, got ", status, ".")
  check_ok("Made {.url {url}} public. It answers 200 without signing in.")

  check_step("Update it while it is live (expect access: public)")
  check_quietly(
    "Publish data version 3 internally",
    check_publish_internal(client, app_dir, internal_title, data_version = 3, guid = guid)
  )
  result <- check_quietly(
    "Deploy the live update to Connect Cloud",
    deploy_bundle(guid, cloud_id = cloud_id, client = client, cld = cld)
  )
  check_show_result(result)
  if (!identical(result$access, "public")) stop("The update changed the access to ", result$access, ".")
  check_ok("Updated it while live. Still public, and {.url {url}} answers {check_public_status(url)}.")
  cli::cli_alert_info("Open {.url {url}} to see data version 3.")

  if (interactive()) readline("Press Enter to make it private again: ")

  check_step("Make it private again")
  cloud_set_access(cld, cloud_id, "private")
  check_ok("Made it private again. The address answers {check_public_status(url)}.")
})

check_done(9)
