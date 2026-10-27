# Check 10: rename a live product's custom address, then rename it back.
#
# Uses the item from 08-custom-domain.R. It is made public for the length of
# the check, so the rename happens to a live product. The check shows whether
# the new address answers straight away or only after a redeploy, and what
# the old address returns once it has moved. Links to a renamed product's old
# address break, so `cloud_rename_address()` refuses without
# `confirm = TRUE`.
#
# Needs: 08-custom-domain.R.
#
# Changes: the item from 08 is public, and under a second name, for the
# length of this check. It is renamed back and made private again at the end,
# even if a step fails.

source("checks/_helpers.R")

client <- phs_connect_int()
guid <- check_need("custom_guid", "08-custom-domain.R")
cloud_id <- check_need("cloud_custom_id", "08-custom-domain.R")
url <- check_need("cloud_custom_url", "08-custom-domain.R")

local({
  local_rsconnect_config()
  cld <- phs_connect_cld()
  original <- cloud_get_content(cld, cloud_id)$vanity_name
  new_name <- paste0(original, "-renamed")

  check_confirm(paste0(
    "make ", url, " public, then rename it to ", new_name, " and back"
  ))

  # Whatever happens below, finish with the original name and private.
  on.exit({
    try(cloud_rename_address(cld, cloud_id, original, confirm = TRUE), silent = TRUE)
    try(cloud_set_access(cld, cloud_id, "private"), silent = TRUE)
  }, add = TRUE)

  check_step("Make it public")
  cloud_set_access(cld, cloud_id, "public")
  check_ok("Made it public. {.url {url}} answers {check_wait_status(url, '200')}.")

  check_step("Refuse a rename without confirmation")
  refused <- tryCatch(
    {
      cloud_rename_address(cld, cloud_id, new_name)
      FALSE
    },
    phsdeploy_rename_not_confirmed = function(e) TRUE
  )
  if (!refused) stop("The rename went ahead without confirm = TRUE.")
  check_ok("Refused, as expected, without {.code confirm = TRUE}.")

  check_step("Rename it")
  renamed <- cloud_rename_address(cld, cloud_id, new_name, confirm = TRUE)
  check_ok("Renamed. Cloud now reports the address {.url {renamed$new_url}}.")
  check_address_answers(renamed$new_url, guid, cloud_id, client, cld)
  cli::cli_alert_info(
    "The old address {.url {url}} now answers {check_public_status(url)}."
  )

  check_step("Rename it back")
  restored <- cloud_rename_address(cld, cloud_id, original, confirm = TRUE)
  check_ok("Renamed back to {.url {restored$new_url}}.")
  check_address_answers(restored$new_url, guid, cloud_id, client, cld)

  check_step("Make it private again")
  cloud_set_access(cld, cloud_id, "private")
  check_ok("Made it private again. The address answers {check_wait_status(url, c('302', '307'))}.")
})

check_done(10)
