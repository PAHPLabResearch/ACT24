#' @rdname get_activity_info
#' @param matches output from \code{\link{get_activity_matches}}
#' @param mxm shell of minute-by-minute data containing id and timestamps
#' @keywords internal
initialize_activity_info <- function(a, intervals, matches, mxm, verbose) {

  if (verbose) cat("\n...initializing info object")
  n_activities <- sapply(matches, length)
  max_activities <- max(n_activities)

  dummy_val <- rep(NA, max_activities)
  dummy_ind <- seq_len(max_activities)

  list(
    original = a, intervals = intervals, matches = matches,
    n_activities = n_activities, max_activities = max_activities,
    dummy_val = dummy_val, dummy_ind = dummy_ind
  ) %>%
  c(., assembler(., "Activity"), assembler(., "Index")) %>%
  c(., preliminary_labels(., mxm, verbose)) %>%
  c(
    list(complete = which(
      .$preliminary_labels$Primary_Activity!="Multitasking"
    )),
    list(incomplete = which(
      .$preliminary_labels$Primary_Activity=="Multitasking"
    ))
  )

}

#' @rdname get_activity_info
#' @keywords internal
check_nonprimary <- function(info, verbose) {

  if (verbose) cat(
    "\n...checking for fully secondary activities"
  )

  zeroes <-
    (info$original$Duration_Primary == 0) %>%
    {seq(nrow(info$original))[.]}

  if (!length(zeroes)) return(info)

  info$incomplete %>%
  info$matches[.] %>%
  lapply(function(x) x[!x %in% zeroes]) %>%
  update_info(info, verbose)

}

#' @rdname get_activity_info
#' @keywords internal
check_quota <- function(info, verbose) {

  if (verbose) cat(
    "\n...checking for fully assigned activities"
  )

  missing_info <- get_missing_info(info, FALSE)

  if (nrow(missing_info$remaining) == 0) return(info)

  remaining <- missing_info$remaining$index

  info$incomplete %>%
  info$matches[.] %>%
  lapply(
    function(x) x[x %in% remaining]
  ) %>%
  update_info(info, verbose)

}

#' @rdname get_activity_info
#' @keywords internal
summarize_missing <- function(info, verbose) {

  get_missing_info(info, verbose) %>%
  {list(info = list(info), missing_info = list(.))} %>%
  lapply(function(x) x[[1]])

}
