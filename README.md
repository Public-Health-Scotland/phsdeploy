
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phsdeploy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Public-Health-Scotland/phsdeploy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Public-Health-Scotland/phsdeploy/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of phsdeploy is to provide a consistent pipeline for publishing
information products, including Shiny apps, R Markdown and Quarto
reports.

## Installation

You can install the development version of phsdeploy like so:

``` r
remotes::install_git(url = "git@github.com:public-health-scotland/phsdeploy")
```

Where network security settings require, it is possible to download a
zip of the repository and install like so:

``` r
# Install from local zip
remotes::install_local("<FILEPATH OF ZIPPED FILE>/phsdeploy-main.zip",
  upgrade = "never"
)
```

## Usage

Once installed, you can load phsdeploy like so:

``` r
library(phsdeploy)
```

Settings come from environment variables (`phsdeploy_config()`), or from
values (`phsdeploy_int_config()`, `phsdeploy_cld_config()`,
`phsdeploy_sio_config()`), for example
a credential an app stores for each user.

| Area | Functions |
|----|----|
| Connecting | `phs_connect_int()`, `phs_connect_cld()` |
| Internal content | `list_int_content()`, `get_int_content()`, `get_int_content_bundles()` |
| Internal access and groups | `int_list_access()`, `int_list_people()`, `int_add_group_access()`, `int_remove_access()`, `int_find_group()`, `int_group_members()`, `int_list_users()` |
| Internal tags | `int_list_tags()`, `int_content_tags()`, `int_add_tag()`, `int_remove_tag()` |
| Deploying to Cloud | `deploy_bundle()` |
| Cloud items | `cloud_list_content()`, `cloud_get_content()`, `cloud_set_access()`, `cloud_set_details()`, `cloud_rename_address()`, `cloud_delete_content()` |
| Cloud sharing | `cloud_private_link()`, `cloud_set_private_link()` |
| Cloud domains | `cloud_list_domains()`, `cloud_public_url()` |
| Cloud users and collaborators | `cloud_me()`, `cloud_list_users()`, `cloud_list_collaborators()`, `cloud_add_collaborator()`, `cloud_remove_collaborator()`, `cloud_sync_collaborators()`, `match_cloud_users()` |
| shinyapps.io (read only) | `sio_list_apps()` |

A typical deploy, with every deployer made a collaborator so any of them
can update the item later:

``` r
client <- phs_connect_int()
cld <- phs_connect_cld()
group <- int_find_group(client, "PHS-PositConnect-PROD-Admin")
deployers <- match_cloud_users(int_group_members(client, group$guid), cloud_list_users(cld))
result <- deploy_bundle(guid, client = client, cld = cld, collaborators = deployers$matched$cloud_user_id)
```

The `checks/` folder has scripts that run each part against the real
servers.

## Licence

This package is available under the [MIT Licence](LICENSE). This covers
both the codebase and any sample code in the documentation.

© Public Health Scotland, 2026. The documentation in this package is
licensed under the [Open Government Licence
3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3),
except where otherwise stated.
