# Check 15: list the apps on the shinyapps.io account.
#
# Read only. Expect every app on the account, archived ones included, with
# its status and dates.
#
# Needs: PHSDEPLOY_SIO_ACCOUNT, PHSDEPLOY_SIO_TOKEN and PHSDEPLOY_SIO_SECRET.
#
# Changes: none.

source("checks/_helpers.R")

local({
  check_step("Configuration")
  config <- phsdeploy_config("sio")
  print(config)

  check_step("Apps")
  apps <- sio_list_apps(config)
  print(utils::head(apps[order(apps$updated, decreasing = TRUE), ], 10))
  counts <- table(apps$status, useNA = "ifany")
  check_ok("Found {nrow(apps)} app{?s}: {paste(names(counts), counts, sep = ' ', collapse = ', ')}.")
})

check_done(15)
