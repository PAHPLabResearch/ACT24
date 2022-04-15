#' Gather ACT24 information about activity intervals and overlap (multitasking)
#' @inheritParams act24_wrapper
#' @keywords internal
get_activity_info <- function(id = unique(a$id), a, verbose = TRUE) {

  if (verbose) cat("\nRetrieving activity info for", id)

  mxm <- data.frame(
    id = id, Timestamp = .minutes,
    stringsAsFactors = FALSE
  )

  intervals <- get_activity_intervals(a, verbose)
  matches <- get_activity_matches(intervals, verbose)

  initialize_activity_info(
    a, intervals, matches, mxm, verbose
  ) %>%
  check_nonprimary(verbose) %>%
  check_quota(verbose) %>%
  summarize_missing(verbose)

}

#' @rdname get_activity_info
#' @keywords internal
get_activity_intervals <- function(a, verbose) {

  if (verbose) cat("\n...getting activity intervals")
  intervals <- with(
    a,
    lubridate::interval(ActivityStartTime, ActivityEndTime)
  )
  lubridate::int_end(intervals) %<>% {. - 1}
  stats::setNames(intervals, a$Activity)

}

#' @rdname get_activity_info
#' @param intervals output from \code{\link{get_activity_intervals}}
#' @keywords internal
get_activity_matches <- function(intervals, verbose) {

  if (verbose) cat("\n...matching timestamps to intervals")

  .minutes$Timestamp %>%
  lapply(
    function(x) {
      lubridate::`%within%`(x, intervals) %>%
      which(.)
    }
  )

}

#' @rdname get_activity_info
#' @param new_matches updated list of matches for use in labeling newly
#'   confirmed activity indices
#' @keywords internal
update_info <- function(new_matches, info, verbose) {

  initial_missing <- length(info$incomplete)

  info$matches[info$incomplete] <- new_matches
  info$n_activities <- sapply(info$matches, length)

  needs_update <-
    (info$preliminary_labels$Primary_Activity == "Multitasking") %>%
    {. & info$n_activities == 1}

  if (any(needs_update)) {

    info$preliminary_labels[
      needs_update, "Primary_Activity"
    ] <- sapply(
      info$matches[needs_update],
      function(x) info$original$Activity[x]
    )

    info$preliminary_labels[
      needs_update, "Primary_Index"
    ] <- unlist(info$matches[needs_update])

    info$complete <- which(
      info$preliminary_labels$Primary_Activity!="Multitasking"
    )

    info$incomplete <- which(
      info$preliminary_labels$Primary_Activity=="Multitasking"
    )

  }

  if (verbose) {
    (initial_missing - length(info$incomplete)) %>%
      message("\nSuccessfully labeled ", ., " additional minutes of data")
  }

  info

}
