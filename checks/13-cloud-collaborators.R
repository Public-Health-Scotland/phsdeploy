# Check 13: match the deployer group to Cloud users, and manage a Cloud
# item's collaborators.
#
# Reads who your Cloud credential belongs to and the account's users, and
# matches the internal deployer group to them by email. Then, if
# PHSDEPLOY_CHECK_CLOUD_USER is set to a colleague's Cloud email, adds them as
# a collaborator on the basic test item and removes them again.
#
# The deployer group is PHS-PositConnect-PROD-Admin unless
# PHSDEPLOY_CHECK_DEPLOYER_GROUP names another.
#
# Needs: 06-deploy-private.R.
#
# Changes: with PHSDEPLOY_CHECK_CLOUD_USER, that person is a collaborator on
# the test item for the length of the check.

source("checks/_helpers.R")

client <- phs_connect_int()
cloud_id <- check_need("cloud_basic_id", "06-deploy-private.R")
group_name <- Sys.getenv("PHSDEPLOY_CHECK_DEPLOYER_GROUP", "PHS-PositConnect-PROD-Admin")
colleague <- Sys.getenv("PHSDEPLOY_CHECK_CLOUD_USER")

local({
  cld <- phs_connect_cld()

  check_step("Who you are on Cloud, and the account's users")
  me <- cloud_me(cld)
  users <- cloud_list_users(cld)
  roles <- table(users$role)
  check_ok("You are {.val {me$account_role}}. The account has {nrow(users)} user{?s}: {paste(names(roles), roles, sep = ' ', collapse = ', ')}.")

  check_step("Match the deployer group to Cloud users")
  group <- int_find_group(client, group_name)
  members <- int_group_members(client, group$guid)
  matched <- match_cloud_users(members, users)
  check_ok("{nrow(matched$matched)} of {nrow(members)} group member{?s} matched by email.")
  if (nrow(matched$unmatched)) {
    cli::cli_alert_warning("No Cloud user for: {.val {matched$unmatched$username}}.")
  }
  if (nrow(matched$cloud_only)) {
    cli::cli_alert_warning("{nrow(matched$cloud_only)} Cloud user{?s} not in the group.")
  }

  check_step("The test item's collaborators")
  before <- cloud_list_collaborators(cld, cloud_id)
  check_ok("{nrow(before)} member{?s}: {sum(before$role %in% 'collaborator')} collaborator{?s}.")

  if (!nzchar(colleague)) {
    cli::cli_alert_info("Set {.envvar PHSDEPLOY_CHECK_CLOUD_USER} to a colleague's Cloud email to try adding and removing a collaborator.")
    return(invisible())
  }
  target <- users[tolower(users$email) %in% tolower(colleague), , drop = FALSE]
  if (nrow(target) != 1L) stop("No single Cloud user has the email in PHSDEPLOY_CHECK_CLOUD_USER.")
  if (identical(target$user_id, me$user_id)) stop("PHSDEPLOY_CHECK_CLOUD_USER must be someone else, since Cloud doesn't let you add yourself.")

  check_confirm(paste("make", colleague, "a collaborator on the test item, then remove them"))

  check_step("Add them with a sync, then remove them")
  synced <- cloud_sync_collaborators(cld, cloud_id, target$user_id)
  if (nrow(synced$failed)) stop("The sync couldn't add them: ", synced$failed$reason[[1]])
  if (length(synced$invited)) {
    stop("They were invited but aren't a member yet, so the invitation needs accepting. Remove it in the Cloud interface if it isn't wanted.")
  }
  now <- cloud_list_collaborators(cld, cloud_id)
  if (!identical(now$role[now$user_id %in% target$user_id], "collaborator")) stop("They aren't a collaborator after the sync.")
  again <- cloud_sync_collaborators(cld, cloud_id, target$user_id)
  if (length(again$added)) stop("A second sync added them again.")
  cloud_remove_collaborator(cld, cloud_id, target$user_id)
  after <- cloud_list_collaborators(cld, cloud_id)
  if (target$user_id %in% after$user_id) stop("They are still a member after removing them.")
  check_ok("Added as a collaborator, a second sync changed nothing, and removed (made a viewer first).")
})

check_done(13)
