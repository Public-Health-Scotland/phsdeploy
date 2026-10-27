# Check 7: update a product on Connect Cloud with new data.
#
# Publishes a new bundle of the internal basic app with data version 2, then
# deploys it into the Cloud item from 06-deploy-private.R. An update must keep
# the same item, title and access. Open the item in Connect Cloud afterwards
# to see "Data version: 2".
#
# Needs: 06-deploy-private.R.
#
# Changes: both test items, creates nothing new.

source("checks/_helpers.R")

client <- phs_connect_int()
guid <- check_need("int_basic_guid", "02-publish-internal.R")
title <- check_need("int_basic_title", "02-publish-internal.R")
cloud_id <- check_need("cloud_basic_id", "06-deploy-private.R")

check_confirm("publish data version 2 of the internal basic app, then deploy it to its Cloud item")

check_step("Publish data version 2 internally")
check_quietly(
  "Publish data version 2 of the basic app internally",
  check_publish_internal(client, "basic-shiny", title, data_version = 2, guid = guid)
)
check_ok("Published data version 2.")

local({
  cld <- phs_connect_cld()
  before <- cloud_get_content(cld, cloud_id)

  check_step("Deploy the update (expect first_deploy: FALSE, same id, title and access)")
  result <- check_quietly(
    "Deploy the update to Connect Cloud",
    deploy_bundle(guid, cloud_id = cloud_id, client = client, cld = cld)
  )
  check_show_result(result)
  if (!identical(result$title, before$title)) stop("The update changed the title.")
  if (!identical(result$access, before$access)) stop("The update changed the access.")
  check_ok("Updated {.val {cloud_id}} with bundle {.val {result$bundle_id}}. Title and access unchanged.")
})

check_done(7)
