# Check 1: connect to internal Connect and Connect Cloud.
#
# Needs: PHSDEPLOY_INT_URL and PHSDEPLOY_INT_KEY, and PHSDEPLOY_CLD_ACCOUNT,
# PHSDEPLOY_CLD_CLIENT_ID and PHSDEPLOY_CLD_CLIENT_SECRET.
#
# Changes: none.

source("checks/_helpers.R")

check_step("Settings (key and secret hidden)")
print(phsdeploy_config())
check_ok("All five settings are set.")

check_step("Internal Connect")
client <- phs_connect_int()
me <- client$me()
check_ok("Linked internal Connect as {.val {me$username}}.")

check_step("Connect Cloud")
local({
  # rsconnect stores the registered account here, and the folder is removed
  # when this block ends.
  local_rsconnect_config()
  cld <- phs_connect_cld()
  print(cld)
  check_ok("Linked Connect Cloud account {.val {cld$account}} ({.val {cld$account_id}}).")

  check_step("Accounts these credentials can see")
  accounts <- cloud_get_pages(cld, "/accounts", query = list(has_user_role = "true"))
  print(data.frame(id = pluck_chr(accounts, "id"), name = pluck_chr(accounts, "name")))
  check_ok("Listed {length(accounts)} account{?s}.")
})

check_done(1)
