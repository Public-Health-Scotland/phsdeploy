#' Rename a Connect Cloud item's custom address
#'
#' Changes the custom name of an item on a wildcard domain, so its address
#' moves from `https://<old>.<domain>` to `https://<name>.<domain>`. Deploys
#' never do this, since they always keep an item's address.
#'
#' Renaming is rare and risky: every link already published to the old
#' address stops working. So it is refused unless `confirm = TRUE`, the new
#' name must be free on the domain, and the item is read back afterwards to
#' check the change took. Cloud only accepts a name together with its domain,
#' so both are sent.
#'
#' @param cld A `phsdeploy_cld` connection.
#' @param id The Cloud item's id.
#' @param name The new custom name, a lowercase DNS label.
#' @param confirm `TRUE` to confirm the old address may stop working.
#' @return A list with `cloud_id`, `old_url`, `new_url` and `renamed` (`FALSE`
#'   if the item already had that name), invisibly. Otherwise an error of
#'   class `phsdeploy_rename_not_confirmed`, `phsdeploy_missing_item`,
#'   `phsdeploy_bad_domain`, `phsdeploy_name_taken` or
#'   `phsdeploy_address_changed`.
#' @noRd
cloud_rename_address <- function(cld, id, name, confirm = FALSE) {
  check_cld(cld)
  check_content_id(id)
  check_vanity_name(name, "name")

  item <- cloud_get_content(cld, id)
  if (is.null(item)) abort_missing_item(id)
  domain <- find_domain(cld, flat_chr(item$domain_id))
  if (is.null(domain) || !isTRUE(domain$wildcard)) {
    cli::cli_abort(
      "Item {.val {id}} isn't on a wildcard domain, so it has no custom name to change.",
      class = c("phsdeploy_bad_domain", "phsdeploy_error")
    )
  }

  old_name <- flat_chr(item$vanity_name)
  old_url <- cloud_public_url(domain, old_name)
  new_url <- cloud_public_url(domain, name)
  if (identical(tolower(old_name), tolower(name))) {
    return(invisible(list(cloud_id = id, old_url = old_url, new_url = new_url, renamed = FALSE)))
  }
  if (!isTRUE(confirm)) {
    cli::cli_abort(
      c(
        "Renaming moves item {.val {id}} from {.url {old_url}} to {.url {new_url}}.",
        "!" = "Links already published to the old address will stop working.",
        "i" = "If that is intended, run again with {.code confirm = TRUE}."
      ),
      class = c("phsdeploy_rename_not_confirmed", "phsdeploy_error")
    )
  }
  if (cloud_name_in_use(cld, name, domain$id)) {
    cli::cli_abort(
      "The name {.val {name}} is already in use on {.val {domain$name}}.",
      class = c("phsdeploy_name_taken", "phsdeploy_error"),
      name = name, domain_id = domain$id
    )
  }

  cloud_call(
    cld, "PATCH", paste0("/contents/", id),
    body = list(vanity_name = name, domain_id = domain$id),
    error_class = name_taken_class,
    error_fields = list(name = name, domain_id = domain$id)
  )

  after <- cloud_get_content(cld, id)
  if (is.null(after)) abort_missing_item(id)
  if (!identical(flat_chr(after$vanity_name), name) ||
        !identical(flat_chr(after$domain_id), domain$id)) {
    cli::cli_abort(
      "Item {.val {id}} doesn't have the new address {.url {new_url}} after renaming.",
      class = c("phsdeploy_address_changed", "phsdeploy_error"),
      cloud_id = id
    )
  }
  invisible(list(cloud_id = id, old_url = old_url, new_url = new_url, renamed = TRUE))
}
