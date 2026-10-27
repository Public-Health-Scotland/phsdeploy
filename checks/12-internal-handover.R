# Check 12: hand an internal item over to a group, and read tags.
#
# Adds a group to the basic test app's access list as a viewer, promotes it
# to collaborator (which deploying to Cloud needs), then removes it. Also
# lists the content your key sees, the group's members, and the tags.
#
# The group is PHS-PositConnect-TEST-RM unless PHSDEPLOY_CHECK_GROUP names
# another. It needs no members, since only the access list changes.
#
# Needs: 02-publish-internal.R.
#
# Changes: the group is on the test app's access list for the length of the
# check.

source("checks/_helpers.R")

client <- phs_connect_int()
guid <- check_need("int_basic_guid", "02-publish-internal.R")
group_name <- Sys.getenv("PHSDEPLOY_CHECK_GROUP", "PHS-PositConnect-TEST-RM")

check_step("Content your key lists")
items <- list_int_content(client)
mine <- items[items$guid %in% guid, , drop = FALSE]
if (nrow(mine) != 1L) stop("The test app isn't in the listing.")
check_ok("Listed {nrow(items)} item{?s}. The test app is there, with app_role {.val {mine$app_role}}.")

check_step("Find the group and its members")
group <- int_find_group(client, group_name)
members <- int_group_members(client, group$guid)
check_ok("Found {.val {group$name}}, with {nrow(members)} member{?s}.")

check_confirm(paste("add", group_name, "to the test app's access list, then remove it"))

check_step("Add the group as a viewer, then make it a collaborator")
added <- int_add_group_access(client, guid, group$guid, role = "viewer")
access <- int_list_access(client, guid)
if (!identical(access$role[access$principal_guid %in% group$guid], "viewer")) stop("The group isn't a viewer.")
promoted <- int_add_group_access(client, guid, group$guid)
access <- int_list_access(client, guid)
if (!identical(access$role[access$principal_guid %in% group$guid], "collaborator")) stop("The group isn't a collaborator.")
people <- int_list_people(client, guid)
if (!identical(people$role[[1]], "owner") || !group$name %in% people$name) stop("The people list is missing the owner or the group.")
again <- int_add_group_access(client, guid, group$guid)
if (again$changed) stop("Adding the group a second time changed something.")
check_ok("Viewer, then collaborator, and listed by name with the owner. Adding it again changed nothing.")

check_step("Remove the group")
int_remove_access(client, guid, promoted$permission_id)
access <- int_list_access(client, guid)
if (group$guid %in% access$principal_guid) stop("The group is still on the access list.")
check_ok("Removed.")

check_step("Tags")
tags <- int_list_tags(client)
item_tags <- int_content_tags(client, guid)
check_ok("The server has {nrow(tags)} tag{?s}, {sum(is.na(tags$parent_id))} of them categories. The test app has {nrow(item_tags)}.")

check_done(12)
