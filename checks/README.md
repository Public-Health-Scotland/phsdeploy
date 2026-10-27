# Checks for phsdeploy

This directory contains scripts that run the individual components of the package, allowing testing of the real internal Connect server and the PHS Connect Cloud account. While unit tests have been developed alongside the codebase, these have been designed to use mocks. Each script is short and prints what it does and what to expect.

This folder is for developers, so it sits at the package root and is left out of the built package (`.Rbuildignore`). The scripts load the package from source with `pkgload::load_all()` to reach internal functions, so they can't run from an installed copy.

## Before you start

- Run on any IDE within PHS Posit Workbench, from the package root (the folder with `DESCRIPTION`), in **one R session**, in number order.
- Set environment variables: `PHSDEPLOY_INT_URL`, `PHSDEPLOY_INT_KEY`, `PHSDEPLOY_CLD_ACCOUNT`, `PHSDEPLOY_CLD_CLIENT_ID` and `PHSDEPLOY_CLD_CLIENT_SECRET` in `.Renviron`.
- Depends on packages: shiny, bslib and phsshiny, for the test apps.
- Every step that changes something on a server asks for confirmation first, in a dialog in RStudio. To run several checks without questions, or with `Rscript`, set `PHSDEPLOY_CHECK_CONFIRM` to `yes` first.

Each check can be run with:

```r
source("checks/01-connect.R")
```

Or all of them in order, without questions:

```r
Sys.setenv(PHSDEPLOY_CHECK_CONFIRM = "yes")
for (check in sprintf("checks/%02d-*.R", 0:12)) source(Sys.glob(check))
```

## What you'll see

Each step ends with a green tick saying what worked, and each check ends with "Check N passed". Detailed output from publishing and deploying goes to a log file instead of the console, at `file.path(tempdir(), "phsdeploy-checks.log")`. If a step fails, the last lines that step logged are shown with the error.

If a first deploy fails part-way (for example on a network timeout), run the same check again. It finds the item it recorded and finishes it as an update, rather than creating another.

## The checks

| Script | What it checks | Changes |
|---|---|---|
| `00-network.R` | Network route and connection times to every server used | Nothing |
| `01-connect.R` | Settings, internal sign-in, Cloud sign-in, account list | Nothing |
| `02-publish-internal.R` | Publishes the basic test app on PHS Connect (internal), and the phsshiny app when asked | Creates 1 or 2 items on the internal server |
| `03-download-bundle.R` | Content, bundle list, download, safe unpack | Nothing |
| `04-rewrite-manifest.R` | Package repositories rewritten for Cloud | Temporary copy only |
| `05-list-domains.R` | Custom domains and the addresses they give | Nothing |
| `06-deploy-private.R` | First deploy, private, default address | Creates 1 Cloud item |
| `07-update.R` | Data update into the same item, access unchanged | Updates both basic items |
| `08-custom-domain.R` | Custom name on the wildcard domain | Creates 1 Cloud item |
| `09-make-public.R` | Going public, then an update while live stays public | Public for the length of the check |
| `10-rename-address.R` | Renaming a live product's address, and renaming it back | Public, and under a second name, for the length of the check |
| `11-private-link.R` | Private link sharing: the link form, an update while shared, switching off and on | Sharing on for the length of the check, item stays private |
| `12-delete.R` | Deletes everything the checks created | Removes all the items created |

`03` accepts any internal content: set `PHSDEPLOY_CHECK_GUID` to try a real product's bundle, then run `04` on it.

`08` and `09` use the basic app by default. Set `PHSDEPLOY_CHECK_CUSTOM_APP` to `phsshiny` before running `02` to use the phsshiny app instead, which also tests that both servers can install phsshiny from its private GitHub repository. That needs:

- **internal Connect** to have its own GitHub credential (a `GitCredential` for `github.com`, set by a Connect admin), so `02` can build the app there. Without it, the build fails and there's no bundle to deploy to Cloud;
- **the Connect Cloud GitHub App** to have access to the phsshiny repository (a GitHub organisation admin grants this);
- that access recorded for the session, before running `08`: `options(phsdeploy.cloud_github_access = "Public-Health-Scotland")`.

Without the Cloud access, `08` stops before creating anything, with an error naming the private package.

## Safety

- Everything the checks create is titled `phsdeploy check ...`, and `12` deletes only recorded items with that title.
- The fixed landing-page domain is never used.
- Bundles, unpacked apps and rsconnect's records go to temporary folders.
- What the checks create is recorded in a file in `tempdir()`. Restarting R forgets it, so run `12-delete.R` before or remove leftover `phsdeploy check` items manually.
