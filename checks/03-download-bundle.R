# Check 3: download and unpack a bundle from internal Connect.
#
# Needs: the basic app from 02-publish-internal.R, or any content named as
# PHSDEPLOY_CHECK_GUID. Downloads to a temporary folder only.
#
# Changes: none.

source("checks/_helpers.R")

client <- phs_connect_int()
guid <- Sys.getenv("PHSDEPLOY_CHECK_GUID")
if (!nzchar(guid)) guid <- check_need("int_basic_guid", "02-publish-internal.R")

check_step("Content")
content <- get_int_content(guid, client)
str(content)
check_ok("Read {.val {content$title}}.")

check_step("Bundles")
bundles <- get_int_content_bundles(guid, client)
print(bundles)
check_ok("Listed {nrow(bundles)} bundle{?s}.")

check_step("Download the active bundle")
bundle <- download_bundle(guid, dir = tempfile("phsdeploy-check-download-"), client = client)
check_ok("Downloaded bundle {.val {bundle$bundle_id}}, SHA-256 {.val {substr(bundle$sha256, 1, 12)}}...")

check_step("Unpack it safely")
dir <- tempfile("phsdeploy-check-unpack-")
unpack_bundle(bundle$path, dir)
files <- list.files(dir, recursive = TRUE)
print(files)
check_save(unpacked_dir = dir)
check_ok("Unpacked {length(files)} file{?s}, with a {.file manifest.json}.")

check_done(3)
