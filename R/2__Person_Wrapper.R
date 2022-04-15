# Main Function -----------------------------------------------------------

#' Assemble minute-by-minute data for one participant's ACT24 responses
#'
#' @param id character. The desired id label
#' @param a data frame. ACT24 input (i.e., event-based data)
#' @param verbose logical. Print updates to console?
#' @seealso
#'   \code{\link{get_activity_info}} \cr
#'   \code{\link{assign_primary_activities}} \cr
#'   \code{\link{get_ee}}
#' @keywords internal
act24_wrapper <- function(id, a, verbose) {

  get_activity_info(id, a, verbose) %>%
  {assign_primary_activities(
    .$info, .$missing_info, verbose
  )} %>%
  get_ee(verbose) %>%
  final_form(verbose)

}

# Final Formatter (internal) ----------------------------------------------

final_form <- function(info, verbose) {

  if (verbose) cat("\n...Touching up the format")

  drops <-
    info[c("Activity", "Index")] %>%
    lapply(names) %>%
    stats::setNames(c("x", "y")) %>%
    do.call(union, .)

  f <-
    info %>%
    {.[["preliminary_labels"]]} %>%
    {stats::setNames(
      ., gsub("^Primary_", "", names(.))
    )} %>%
    {.[ ,!names(.) %in% drops]}

  new_names <-
    match("n_activities", names(f)) %>%
    append(names(f), "multitasking", .)

  f$multitasking <- f$n_activities > 1

  f[ ,new_names]

}
