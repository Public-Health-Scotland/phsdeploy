# phsdeploy 0.1.0

* Connecting to Connect Cloud no longer uses rsconnect. `phs_connect_cld()` only checks the credential and looks up the account through the API, so a connection is safe to use in a Shiny app serving several people. `deploy_bundle()` registers with rsconnect itself, in a temporary folder, and deleting uses the API.
* `phsdeploy_int_config()` and `phsdeploy_cld_config()` build a configuration from values instead of environment variables.
* New functions for internal Connect: `list_int_content()`, access lists (`int_list_access()`, `int_add_group_access()`, `int_remove_access()`), groups (`int_find_group()`, `int_group_members()`) and tags (`int_list_tags()`, `int_content_tags()`, `int_add_tag()`, `int_remove_tag()`). `get_int_content()` gains `app_role`.
* New functions for Connect Cloud users and collaborators: `cloud_me()`, `cloud_list_users()`, `cloud_list_collaborators()`, `cloud_add_collaborator()`, `cloud_remove_collaborator()`, `cloud_sync_collaborators()` and `match_cloud_users()`.
* The Cloud item functions are exported: `cloud_list_content()`, `cloud_get_content()`, `cloud_set_access()`, `cloud_private_link()`, `cloud_set_private_link()`, `cloud_rename_address()`, `cloud_delete_content()`, `cloud_list_domains()` and `cloud_public_url()`. New `cloud_set_details()` changes a title or description. `cloud_delete_content()` now refuses unless `confirm = TRUE`.
* `deploy_bundle()` gains `collaborators`, and announces each step with a `phsdeploy_progress` message.
* New checks: `12-internal-handover.R` and `13-cloud-collaborators.R`. The delete check is now `14-delete.R`.
