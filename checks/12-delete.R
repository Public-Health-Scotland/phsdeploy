# Check 12: delete everything the checks created.
#
# Only items recorded in this R session, whose title starts with
# "phsdeploy check", are deleted, so nothing else can be removed. Running it
# twice is safe: an item that's already gone is reported and skipped.
#
# Needs: none.
#
# Changes: deletes the Cloud items from 06 and 08, and the internal items from 02.

source("checks/_helpers.R")

state <- check_state()
cloud_ids <- unlist(state[c("cloud_basic_id", "cloud_custom_id")])
internal_guids <- unlist(state[c("int_basic_guid", "int_phsshiny_guid")])
if (!length(cloud_ids) && !length(internal_guids)) stop("Nothing recorded to delete.")

check_confirm(paste(
  "delete", length(cloud_ids), "Cloud item(s) and", length(internal_guids),
  "internal item(s) created by the checks"
))

local({
  cld <- phs_connect_cld()
  for (id in cloud_ids) {
    item <- cloud_get_content(cld, id)
    if (is.null(item)) {
      cli::cli_alert_info("Cloud item {.val {id}} is already gone.")
    } else if (!startsWith(item$title %||% "", check_title_prefix)) {
      cli::cli_alert_warning("Skipped Cloud item {.val {id}}: its title isn't a check title.")
    } else {
      cloud_delete_content(cld, id, confirm = TRUE)
      check_ok("Deleted Cloud item {.val {id}}.")
    }
  }
})

client <- phs_connect_int()
for (guid in internal_guids) {
  item <- tryCatch(connectapi::content_item(client, guid), error = function(e) NULL)
  if (is.null(item)) {
    cli::cli_alert_info("Internal item {.val {guid}} is already gone.")
  } else if (!startsWith(item$content$title %||% "", check_title_prefix)) {
    cli::cli_alert_warning("Skipped internal item {.val {guid}}: its title isn't a check title.")
  } else {
    connectapi::content_delete(item, force = TRUE)
    check_ok("Deleted internal item {.val {guid}}.")
  }
}

unlink(check_state_file)
check_done(12)
