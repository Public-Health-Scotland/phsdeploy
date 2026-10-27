# Check 2: publish the two test apps to internal Connect.
#
# This publishes `checks/apps/basic-shiny` as a new item titled
# "phsdeploy check ...", with data version 1. The manifest is written from the
# local library, recording the publisher's package repositories.
#
# With PHSDEPLOY_CHECK_CUSTOM_APP=phsshiny, it also publishes
# `checks/apps/phsshiny-app`. phsshiny is in a private GitHub repository, so
# internal Connect needs its own GitHub credential (a GitCredential for
# github.com, set by a Connect admin) to install it. Without one, the build
# fails, and there is no bundle to deploy to Connect Cloud either.
#
# Needs: shiny installed, and bslib and phsshiny for the phsshiny app.
#
# Changes: creates one or two items on internal Connect (removed by
# 12-delete.R, even if a build fails).

source("checks/_helpers.R")

client <- phs_connect_int()
stamp <- check_stamp()
with_phsshiny <- identical(Sys.getenv("PHSDEPLOY_CHECK_CUSTOM_APP"), "phsshiny")
titles <- c(basic = paste(check_title_prefix, "basic", stamp))
if (with_phsshiny) titles[["phsshiny"]] <- paste(check_title_prefix, "phsshiny", stamp)

check_confirm(paste("publish to internal Connect:", paste(titles, collapse = ", ")))

check_step("Publish the basic Shiny app")
basic_guid <- check_quietly(
  "Publish the basic app internally",
  check_publish_internal(client, "basic-shiny", titles[["basic"]], save_as = "int_basic_guid")
)
check_save(int_basic_title = titles[["basic"]])
check_ok("Published {.val {titles[['basic']]}} ({.val {basic_guid}}).")

if (with_phsshiny) {
  check_step("Publish the phsshiny app")
  phsshiny_guid <- check_quietly(
    "Publish the phsshiny app internally",
    check_publish_internal(
      client, "phsshiny-app", titles[["phsshiny"]], save_as = "int_phsshiny_guid"
    )
  )
  check_save(int_phsshiny_title = titles[["phsshiny"]])
  check_ok("Published {.val {titles[['phsshiny']]}} ({.val {phsshiny_guid}}).")
} else {
  cli::cli_alert_info("Skipped the phsshiny app. Set {.envvar PHSDEPLOY_CHECK_CUSTOM_APP} to {.val phsshiny} to include it.")
}

check_done(2)
