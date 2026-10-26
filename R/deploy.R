#' Deploy an internal bundle to Posit Connect Cloud
#'
#' Downloads a bundle of internal content, rewrites its package repositories
#' to public ones, and deploys it to Connect Cloud with
#' `rsconnect::deployApp()`.
#'
#' * **First deploy** (no `cloud_id`): creates a new Cloud item, optionally
#'   with a custom address on one of the account's domains, then deploys into
#'   it. New items are always private.
#' * **Update** (`cloud_id` given): deploys into the existing item. Its
#'   address, title and access are kept, so a live product stays live.
#'
#' Making a product public is a separate step, done with `cloud_set_access()`
#' (internal) at its release time, or straight after the first deploy for a
#' product going straight to live.
#'
#' After deploying, the item is read back. Its address and access must be
#' what they were (or, for a first deploy, what was asked for), otherwise an
#' error is raised.
#'
#' @param guid The internal content's GUID.
#' @param cloud_id `NULL` for a first deploy, or the id of the Cloud item to
#'   update.
#' @param bundle_id The internal bundle to deploy, or `NULL` for the active
#'   bundle.
#' @param title The item's title on a first deploy, by default the internal
#'   title. Not used for an update, which keeps the item's own title.
#' @param domain_id For a first deploy only: a domain id from
#'   `cloud_list_domains()` (internal) for a custom address, or `NULL` for
#'   Cloud's default address.
#' @param name For a first deploy on a wildcard domain only: the custom name,
#'   so the address is `https://<name>.<domain>`.
#' @param client A connectapi client for internal Connect, by default
#'   `phs_connect_int()`.
#' @param cld A Connect Cloud connection from [phs_connect_cld()], or `NULL`
#'   to sign in for this deploy only. Pass one to reuse it across several
#'   deploys, from inside the same `local_rsconnect_config()` scope it was
#'   created in.
#'
#' @return A list with `guid`, `bundle_id`, `sha256`, `cloud_id`,
#'   `first_deploy`, `title`, `access`, `domain_id`, `name`, `url` (the custom
#'   address, or `NA`) and `repo_changes` (from the manifest rewrite).
#'
#'   Otherwise an error of class `phsdeploy_error`. A failed deploy into an
#'   item this call created raises `phsdeploy_deploy_failed` with the item's
#'   `cloud_id`, so the retry can update that item rather than create another.
#' @export
deploy_bundle <- function(guid, cloud_id = NULL, bundle_id = NULL, title = NULL,
                          domain_id = NULL, name = NULL,
                          client = phs_connect_int(), cld = NULL) {
  check_guid(guid)
  first_deploy <- is.null(cloud_id)
  if (!first_deploy) {
    check_content_id(cloud_id)
    if (!is.null(domain_id) || !is.null(name)) {
      cli::cli_abort(
        c(
          "An update can't change an item's address.",
          "i" = "Leave {.arg domain_id} and {.arg name} out when {.arg cloud_id} is given."
        ),
        class = c("phsdeploy_bad_argument", "phsdeploy_error")
      )
    }
  }
  if (!is.null(title)) check_string_arg(title, "title")
  if (!is.null(domain_id)) check_domain_id(domain_id)
  if (!is.null(name)) check_vanity_name(name, "name")

  work <- withr::local_tempdir(pattern = "phsdeploy-deploy-")
  if (is.null(cld)) {
    local_rsconnect_config()
    cld <- phs_connect_cld()
  }
  check_cld(cld)
  prepared <- prepare_bundle(guid, bundle_id, work, client)

  if (first_deploy) {
    title <- title %||% get_int_content(guid, client)$title
    domain <- find_domain(cld, domain_id)
    cloud_id <- create_cloud_item(cld, title, prepared$target, domain, name)
    expected <- list(vanity_name = name %||% NA_character_, domain_id = domain_id %||% NA_character_)
  } else {
    before <- cloud_get_content(cld, cloud_id)
    if (is.null(before)) abort_missing_item(cloud_id)
    title <- before$title
    expected <- list(
      vanity_name = flat_chr(before$vanity_name),
      domain_id = flat_chr(before$domain_id),
      access = before$access
    )
    domain <- find_domain(cld, flat_chr(before$domain_id))
    name <- before$vanity_name
  }

  run_deploy(cld, prepared$app_dir, cloud_id, title, work, first_deploy)
  after <- cloud_get_content(cld, cloud_id)
  if (is.null(after)) abort_missing_item(cloud_id)
  check_deployed_item(after, expected, first_deploy)

  list(
    guid = guid,
    bundle_id = prepared$bundle$bundle_id,
    sha256 = prepared$bundle$sha256,
    cloud_id = cloud_id,
    first_deploy = first_deploy,
    title = title,
    access = after$access,
    domain_id = flat_chr(after$domain_id),
    name = flat_chr(after$vanity_name),
    url = if (is.null(domain)) NA_character_ else cloud_public_url(domain, name),
    repo_changes = prepared$changes
  )
}

#' Download, unpack and rewrite a bundle, ready for Connect Cloud
#'
#' Everything happens inside `dir`, which the caller removes.
#'
#' @param guid The internal content's GUID.
#' @param bundle_id The bundle to use, or `NULL` for the active bundle.
#' @param dir A temporary folder to work in.
#' @param client A connectapi client for internal Connect.
#' @return A list with `bundle` (from `download_bundle()`), `app_dir`,
#'   `changes` (from `rewrite_manifest_repos()`) and `target` (from
#'   `manifest_target()`).
#' @noRd
prepare_bundle <- function(guid, bundle_id, dir, client) {
  bundle <- download_bundle(guid,
    dir = file.path(dir, "download"), bundle_id = bundle_id,
    client = client
  )
  app_dir <- file.path(dir, "app")
  unpack_bundle(bundle$path, app_dir)
  manifest <- file.path(app_dir, "manifest.json")
  changes <- rewrite_manifest_repos(manifest)
  list(bundle = bundle, app_dir = app_dir, changes = changes, target = manifest_target(manifest))
}

#' Find one of the account's domains by id
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param domain_id A domain id, or `NULL` or `NA` for no custom domain.
#' @return A one-row data frame from `cloud_list_domains()`, or `NULL` when
#'   `domain_id` is `NULL` or `NA`. Otherwise an error of class
#'   `phsdeploy_bad_domain`.
#' @noRd
find_domain <- function(cld, domain_id) {
  if (is.null(domain_id) || is.na(domain_id)) {
    return(NULL)
  }
  domains <- cloud_list_domains(cld)
  domain <- domains[domains$id %in% domain_id, , drop = FALSE]
  if (nrow(domain) != 1L) {
    cli::cli_abort(
      "Domain {.val {domain_id}} isn't one of the account's domains.",
      class = c("phsdeploy_bad_domain", "phsdeploy_error")
    )
  }
  domain
}

#' Create the Cloud item for a first deploy
#'
#' Refuses anything that could duplicate a product or take another product's
#' address, before creating anything:
#' * an item with the same title already exists, which usually means a first
#'   deploy is being repeated (pass its `cloud_id` to update it instead)
#' * a wildcard domain without a name, or a name already used on it
#' * a fixed domain with a name, or one that already holds an item.
#'
#' Cloud also rejects a duplicate name itself, as a second guard.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param title The item's title.
#' @param target The app mode and primary file, from `manifest_target()`.
#' @param domain A domain from `find_domain()`, or `NULL`.
#' @param name The custom name, or `NULL`.
#' @return The new item's id.
#' @noRd
create_cloud_item <- function(cld, title, target, domain, name) {
  existing <- cloud_find_content(cld, title)
  if (nrow(existing)) {
    cli::cli_abort(
      c(
        "A Cloud item titled {.val {title}} already exists.",
        "i" = "To update it, pass {.arg cloud_id}: {.val {existing$id}}."
      ),
      class = c("phsdeploy_title_taken", "phsdeploy_error"),
      ids = existing$id
    )
  }
  if (is.null(domain) && !is.null(name)) {
    cli::cli_abort(
      "A custom name needs a domain. Pass {.arg domain_id} too.",
      class = c("phsdeploy_bad_argument", "phsdeploy_error")
    )
  }
  if (!is.null(domain)) check_domain_free(cld, domain, name)

  item <- cloud_create_content(
    cld, title, target$app_mode, target$primary_file,
    domain_id = if (!is.null(domain)) domain$id, vanity_name = name
  )
  item$id
}

#' Check an address on a domain is free
#'
#' A wildcard domain holds one item per name. A fixed domain holds
#' a single item and doesn't take a name.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param domain A one-row domain data frame.
#' @param name The custom name, or `NULL`.
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_bad_argument`, `phsdeploy_name_taken` or
#'   `phsdeploy_domain_taken`.
#' @noRd
check_domain_free <- function(cld, domain, name) {
  if (isTRUE(domain$wildcard)) {
    if (is.null(name)) {
      cli::cli_abort(
        "Domain {.val {domain$name}} is a wildcard domain, so it needs a {.arg name}.",
        class = c("phsdeploy_bad_argument", "phsdeploy_error")
      )
    }
    if (cloud_name_in_use(cld, name, domain$id)) {
      cli::cli_abort(
        "The name {.val {name}} is already in use on {.val {domain$name}}.",
        class = c("phsdeploy_name_taken", "phsdeploy_error"),
        name = name, domain_id = domain$id
      )
    }
  } else {
    if (!is.null(name)) {
      cli::cli_abort(
        "Domain {.val {domain$name}} is a fixed domain, so it takes no {.arg name}.",
        class = c("phsdeploy_bad_argument", "phsdeploy_error")
      )
    }
    held_by <- cloud_contents_on_domain(cld, domain$id)
    if (length(held_by)) {
      cli::cli_abort(
        "Fixed domain {.val {domain$name}} already holds item {.val {held_by}}.",
        class = c("phsdeploy_domain_taken", "phsdeploy_error"),
        held_by = held_by
      )
    }
  }
  invisible(TRUE)
}

#' Deploy an unpacked bundle into a Cloud item
#'
#' `rlang_interactive = FALSE` matters: if the item can't be found, rsconnect
#' then stops instead of offering to create a new one. rsconnect's deployment
#' record goes inside `dir`, so nothing is written next to the app or kept.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param app_dir The unpacked app, with its rewritten manifest.
#' @param cloud_id The item to deploy into.
#' @param title The item's title, which rsconnect sends on every deploy.
#' @param dir The temporary work folder.
#' @param first_deploy `TRUE` if this call created the item.
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_deploy_failed`, with `cloud_id`.
#' @noRd
run_deploy <- function(cld, app_dir, cloud_id, title, dir, first_deploy) {
  records <- file.path(dir, "records")
  dir.create(records)
  result <- tryCatch(
    withr::with_options(
      list(rlang_interactive = FALSE),
      rsconnect::deployApp(
        appDir = app_dir,
        manifestPath = file.path(app_dir, "manifest.json"),
        appId = cloud_id,
        account = cld$account,
        server = cloud_server,
        appTitle = title,
        recordDir = records,
        forceUpdate = TRUE,
        launch.browser = FALSE,
        lint = FALSE,
        logLevel = "quiet"
      )
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    detail <- squish_scrub(conditionMessage(result), cloud_secrets(cld))
    cli::cli_abort(
      c(
        "Deploying to Posit Connect Cloud item {.val {cloud_id}} failed.",
        "x" = "{detail}",
        "i" = if (first_deploy) {
          "The item was created. To retry, pass {.code cloud_id = \"{cloud_id}\"}."
        }
      ),
      class = c("phsdeploy_deploy_failed", "phsdeploy_error"),
      cloud_id = cloud_id
    )
  }
  invisible(TRUE)
}

#' Check a Cloud item after deploying
#'
#' The address must be as expected with a first deploy private, and an
#' update not changing the access rights.
#'
#' @param item The item, read back after deploying.
#' @param expected A list with `vanity_name` and `domain_id`, and for an update
#'   `access`.
#' @param first_deploy `TRUE` for a first deploy.
#' @return `TRUE`, invisibly. Otherwise an error of class
#'   `phsdeploy_address_changed` or `phsdeploy_access_changed`.
#' @noRd
check_deployed_item <- function(item, expected, first_deploy) {
  address <- c(flat_chr(item$vanity_name), flat_chr(item$domain_id))
  if (!identical(address, c(expected$vanity_name, expected$domain_id))) {
    cli::cli_abort(
      "Cloud item {.val {item$id}} doesn't have the expected address after deploying.",
      class = c("phsdeploy_address_changed", "phsdeploy_error"),
      cloud_id = item$id
    )
  }
  access_ok <- if (first_deploy) {
    !identical(item$access, "public")
  } else {
    identical(item$access, expected$access)
  }
  if (!access_ok) {
    cli::cli_abort(
      "Cloud item {.val {item$id}} has unexpected access {.val {item$access}} after deploying.",
      class = c("phsdeploy_access_changed", "phsdeploy_error"),
      cloud_id = item$id
    )
  }
  invisible(TRUE)
}

#' Raise an error for a Cloud item that can't be found
#'
#' @param cloud_id The item's id.
#' @return Does not return, raises an error of class `phsdeploy_missing_item`.
#' @noRd
abort_missing_item <- function(cloud_id) {
  cli::cli_abort(
    c(
      "Posit Connect Cloud item {.val {cloud_id}} can't be found.",
      "i" = "It may have been deleted. Leave {.arg cloud_id} out to deploy it as a new item."
    ),
    class = c("phsdeploy_missing_item", "phsdeploy_error"),
    cloud_id = cloud_id
  )
}
