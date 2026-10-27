#' Connect Cloud users and item collaborators
#'
#' On Connect Cloud, an account role doesn't let anyone update an item: even
#' an admin must be a collaborator on it. Connect Cloud has no groups, so each
#' item needs every deployer added as a collaborator, one by one. These
#' functions read the account's users and keep an item's collaborators in
#' step with a list of users, normally the members of an internal Connect
#' group matched to Cloud users by email with [match_cloud_users()].
#'
#' A Cloud admin can manage the collaborators of an item they aren't on,
#' except themselves.
#'
#' @name cloud_members
#' @noRd
NULL

#' Who a Connect Cloud credential belongs to
#'
#' @param cld A Connect Cloud connection from [phs_connect_cld()].
#'
#' @return A list with `user_id`, `display_name`, `email` and `account_role`
#'   (the role in the connection's account: `owner`, `admin`, `publisher`,
#'   `viewer`, or `NA` if none). Otherwise an error of class
#'   `phsdeploy_cloud_http_error`.
#' @export
cloud_me <- function(cld) {
  check_cld(cld)
  me <- strip_record(cloud_json(cloud_call(cld, "GET", "/users/me")))
  account_id <- cloud_account_of(cld)
  roles <- Filter(function(r) identical(r$account_id %||% r$account$id, account_id), me$account_roles %||% list())
  list(
    user_id = flat_chr(me$id),
    display_name = flat_chr(me$display_name),
    email = flat_chr(me$email),
    account_role = if (length(roles)) flat_chr(roles[[1]]$role) else NA_character_
  )
}

#' The users of a Connect Cloud account
#'
#' @inheritParams cloud_me
#'
#' @return A data frame with one row per user, all columns character:
#'   `user_id`, `display_name`, `email` and `role` (`owner`, `admin`,
#'   `publisher` or `viewer`). Otherwise an error of class
#'   `phsdeploy_cloud_http_error`.
#' @export
cloud_list_users <- function(cld) {
  check_cld(cld)
  records <- cloud_get_pages(cld, paste0("/accounts/", cloud_account_of(cld), "/users"))
  member_frame(records)
}

#' Collaborators on a Connect Cloud item
#'
#' * `cloud_list_collaborators()` lists the item's members: its collaborators
#'   and viewers.
#' * `cloud_add_collaborator()` makes a user a collaborator. Cloud adds a
#'   member as a viewer first, so a new member is added and then promoted.
#' * `cloud_remove_collaborator()` removes a user from the item.
#' * `cloud_sync_collaborators()` makes the item's collaborators match a list
#'   of users.
#'
#' A user already in the wanted state is left alone, so each is safe to repeat.
#'
#' @inheritParams cloud_me
#' @param id The Cloud item's id.
#' @param user_id A Cloud user id, from [cloud_list_users()] or [cloud_me()].
#'
#' @return `cloud_list_collaborators()` returns a data frame with `user_id`,
#'   `display_name`, `email` and `role` (`collaborator` or `viewer`).
#'   `cloud_add_collaborator()` and `cloud_remove_collaborator()` return a list
#'   with `user_id` and `changed`, invisibly. Otherwise an error of class
#'   `phsdeploy_cloud_http_error`.
#' @export
#' @examples
#' \dontrun{
#' cld <- phs_connect_cld()
#' cloud_list_collaborators(cld, id)
#' cloud_add_collaborator(cld, id, user_id)
#' }
cloud_list_collaborators <- function(cld, id) {
  check_cld(cld)
  check_content_id(id)
  member_frame(cloud_get_pages(cld, paste0("/contents/", id, "/users")))
}

#' @rdname cloud_list_collaborators
#' @export
cloud_add_collaborator <- function(cld, id, user_id) {
  check_cld(cld)
  check_content_id(id)
  check_cloud_user_ids(user_id, single = TRUE)
  members <- cloud_list_collaborators(cld, id)
  role <- members$role[members$user_id %in% user_id]
  if (identical(role, "collaborator")) {
    return(invisible(list(user_id = user_id, changed = FALSE)))
  }
  path <- paste0("/contents/", id, "/users")
  if (!length(role)) {
    cloud_call(cld, "POST", path, body = list(user_id = user_id, role = "viewer"))
  }
  cloud_call(cld, "PATCH", paste0(path, "/", user_id), body = list(role = "collaborator"))
  invisible(list(user_id = user_id, changed = TRUE))
}

#' @rdname cloud_list_collaborators
#' @export
cloud_remove_collaborator <- function(cld, id, user_id) {
  check_cld(cld)
  check_content_id(id)
  check_cloud_user_ids(user_id, single = TRUE)
  resp <- cloud_call(cld, "DELETE", paste0("/contents/", id, "/users/", user_id), allow = 404L)
  invisible(list(user_id = user_id, changed = httr2::resp_status(resp) != 404L))
}

#' @rdname cloud_list_collaborators
#' @param user_ids The Cloud user ids who should be collaborators.
#' @param keep Cloud user ids to leave alone even if they aren't in
#'   `user_ids`, such as a pipeline's own user.
#' @param remove `TRUE` to also remove collaborators who aren't in `user_ids`
#'   or `keep`. By default nobody is removed, so a wrong list can only fail to
#'   add people, never take access away.
#'
#' @return `cloud_sync_collaborators()` returns a list with `added` and
#'   `removed` (user ids) and `failed`, a data frame of `user_id`, `action` and
#'   `reason` for the changes that couldn't be made. Adding yourself is always
#'   in `failed`, since Cloud doesn't allow it: another deployer's sync covers
#'   it. The item's author is never removed.
#' @export
cloud_sync_collaborators <- function(cld, id, user_ids, keep = character(), remove = FALSE) {
  check_cld(cld)
  check_content_id(id)
  check_cloud_user_ids(user_ids)
  check_cloud_user_ids(keep)
  if (!rlang::is_bool(remove)) {
    cli::cli_abort(
      "{.arg remove} must be {.code TRUE} or {.code FALSE}.",
      class = c("phsdeploy_bad_argument", "phsdeploy_error")
    )
  }
  item <- cloud_get_content(cld, id)
  if (is.null(item)) abort_missing_item(id)
  me <- cloud_me(cld)$user_id
  members <- cloud_list_collaborators(cld, id)
  collaborators <- members$user_id[members$role %in% "collaborator"]
  author <- flat_chr(item$author_id)

  failed <- list()
  fail <- function(user, action, reason) {
    failed[[length(failed) + 1L]] <<- data.frame(user_id = user, action = action, reason = reason)
  }
  added <- character()
  for (user in setdiff(unique(user_ids), c(collaborators, author))) {
    if (identical(user, me)) {
      fail(user, "add", "Cloud doesn't let you add yourself. Another deployer's sync adds you.")
      next
    }
    outcome <- tryCatch(cloud_add_collaborator(cld, id, user), phsdeploy_error = function(e) e)
    if (inherits(outcome, "error")) fail(user, "add", one_line(outcome)) else added <- c(added, user)
  }
  removed <- character()
  if (remove) {
    for (user in setdiff(collaborators, c(user_ids, keep, author))) {
      outcome <- tryCatch(cloud_remove_collaborator(cld, id, user), phsdeploy_error = function(e) e)
      if (inherits(outcome, "error")) fail(user, "remove", one_line(outcome)) else removed <- c(removed, user)
    }
  }
  failed <- if (length(failed)) do.call(rbind, failed) else data.frame(user_id = character(), action = character(), reason = character())
  list(added = added, removed = removed, failed = failed)
}

#' Pair internal Connect users with Connect Cloud users
#'
#' Deployers are expected to use the same email on both, so users are paired
#' by email, ignoring case and surrounding spaces. Anyone whose emails don't
#' match can be paired by hand in `user_map`, which is checked first.
#'
#' @param members Internal users, such as from [int_group_members()]: a data
#'   frame with `username` and `email`.
#' @param cloud_users Cloud users, from [cloud_list_users()]: a data frame
#'   with `user_id` and `email`.
#' @param user_map Optional pairs made by hand: a data frame with `username`
#'   and `cloud_user_id`.
#'
#' @return A list with:
#'   * `matched`: a data frame of `username`, `email`, `cloud_user_id` and
#'     `matched_by` (`email` or `user_map`)
#'   * `unmatched`: internal users with no Cloud user, as given
#'   * `cloud_only`: Cloud users who matched nobody, as given
#' @export
#' @examples
#' members <- data.frame(username = c("ann", "bob"), email = c("Ann@phs.scot", "bob@phs.scot"))
#' cloud_users <- data.frame(user_id = "u1", email = "ann@phs.scot")
#' match_cloud_users(members, cloud_users)
match_cloud_users <- function(members, cloud_users, user_map = NULL) {
  check_frame(members, c("username", "email"), "members")
  check_frame(cloud_users, c("user_id", "email"), "cloud_users")
  if (!is.null(user_map)) check_frame(user_map, c("username", "cloud_user_id"), "user_map")
  norm <- function(x) tolower(trimws(as.character(x)))

  mapped <- if (is.null(user_map)) rep(NA_character_, nrow(members)) else {
    user_map$cloud_user_id[match(members$username, user_map$username)]
  }
  mapped[!mapped %in% cloud_users$user_id] <- NA_character_
  by_email <- cloud_users$user_id[match(norm(members$email), norm(cloud_users$email), incomparables = c(NA, ""))]
  cloud_user_id <- ifelse(is.na(mapped), by_email, mapped)
  found <- !is.na(cloud_user_id)

  matched <- data.frame(
    username = as.character(members$username[found]),
    email = as.character(members$email[found]),
    cloud_user_id = cloud_user_id[found],
    matched_by = ifelse(is.na(mapped[found]), "email", "user_map"),
    stringsAsFactors = FALSE
  )
  list(
    matched = matched,
    unmatched = members[!found, , drop = FALSE],
    cloud_only = cloud_users[!cloud_users$user_id %in% cloud_user_id, , drop = FALSE]
  )
}

#' An error's message as one plain line
#'
#' @param e A condition.
#' @return Its message without formatting, with runs of spaces and line breaks
#'   made single spaces.
#' @noRd
one_line <- function(e) {
  trimws(gsub("\\s+", " ", cli::ansi_strip(conditionMessage(e))))
}

#' Membership records as a data frame
#'
#' Account and item membership records hold the user in a nested `user`.
#'
#' @param records A list of membership records.
#' @return A data frame with `user_id`, `display_name`, `email` and `role`.
#' @noRd
member_frame <- function(records) {
  records <- lapply(records, strip_record)
  user_field <- function(field) vapply(records, function(r) flat_chr(r$user[[field]]), character(1))
  data.frame(
    user_id = user_field("id"),
    display_name = user_field("display_name"),
    email = user_field("email"),
    role = vapply(records, function(r) flat_chr(r$role), character(1)),
    stringsAsFactors = FALSE
  )
}

#' Check Cloud user ids
#'
#' @param user_ids The value to check.
#' @param single `TRUE` if exactly one id is wanted.
#' @param call The environment of the calling function, shown in the error.
#' @return `user_ids`, invisibly. Otherwise an error of class
#'   `phsdeploy_bad_argument`.
#' @noRd
check_cloud_user_ids <- function(user_ids, single = FALSE, call = rlang::caller_env()) {
  ok <- is.character(user_ids) && !anyNA(user_ids) && all(grepl(guid_pattern, user_ids)) &&
    (!single || length(user_ids) == 1L)
  if (!ok) {
    arg <- rlang::caller_arg(user_ids)
    cli::cli_abort(
      if (single) "{.arg {arg}} must be one Cloud user id." else "{.arg {arg}} must be Cloud user ids.",
      class = c("phsdeploy_bad_argument", "phsdeploy_error"),
      call = call
    )
  }
  invisible(user_ids)
}

#' Check a data frame has the columns needed
#'
#' @param x The value to check.
#' @param columns The column names needed.
#' @param arg The argument name used in the error message.
#' @param call The environment of the calling function, shown in the error.
#' @return `x`, invisibly. Otherwise an error of class
#'   `phsdeploy_bad_argument`.
#' @noRd
check_frame <- function(x, columns, arg, call = rlang::caller_env()) {
  if (!is.data.frame(x) || !all(columns %in% names(x))) {
    cli::cli_abort(
      "{.arg {arg}} must be a data frame with {.field {columns}}.",
      class = c("phsdeploy_bad_argument", "phsdeploy_error"),
      call = call
    )
  }
  invisible(x)
}
