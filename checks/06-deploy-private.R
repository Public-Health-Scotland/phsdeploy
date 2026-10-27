# Check 6: first deploy of the basic app to Connect Cloud, privately.
#
# This is a product deployed ahead of its release: `deploy_bundle()` creates
# a new Cloud item (default private) and deploys the internal bundle. No
# custom name, so it uses Cloud's default address.
#
# Needs: 02-publish-internal.R.
#
# Changes: creates one private Cloud item (removed by 14-delete.R).

source("checks/_helpers.R")

client <- phs_connect_int()
guid <- check_need("int_basic_guid", "02-publish-internal.R")
title <- check_need("int_basic_title", "02-publish-internal.R")

local({
  cld <- phs_connect_cld()

  check_confirm(paste0("create a private Cloud item titled \"", title, "\" and deploy into it"))

  check_step("Deploy (expect first_deploy: TRUE, access: private)")
  result <- check_deploy("cloud_basic_id", guid, client = client, cld = cld)
  check_show_result(result)
  check_ok("Deployed to new Cloud item {.val {result$cloud_id}}, access {.val {result$access}}.")

  check_step("The item as Cloud reports it")
  check_show_item(cloud_get_content(cld, result$cloud_id))
})

check_done(6)
