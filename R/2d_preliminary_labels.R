#' @rdname get_activity_info
#' @keywords internal
preliminary_labels <- function(info, mxm, verbose) {

  info$Activity %>%
  data.frame(
    mxm,
    n_activities = info$n_activities,
    .,
    info$Index,
    Primary_Activity = ifelse(
      info$n_activities > 1, "Multitasking", .$Activity1
    ),
    stringsAsFactors = FALSE
  ) %>%
  within({
    Activity1 = ifelse(
      Primary_Activity=="Multitasking", Activity1, NA_character_
    )
    Index1 = ifelse(
      Primary_Activity=="Multitasking", Index1, NA_integer_
    )
    Primary_Index = ifelse(
      Primary_Activity=="Multitasking",
      NA_integer_,
      as.integer(as.character(info$Index$Index1))
    )
  }) %T>%
  {stopifnot(all(
    info$original$Activity[.$Primary_Index] == .$Primary_Activity,
    na.rm = TRUE
  ))} %>%
  list(preliminary_labels = .) %T>%
  {if (verbose) message(
    "\nSuccessfully labeled ",
    sum(
      .[[1]]$Primary_Activity != "Multitasking",
      na.rm = TRUE
    ),
    " minute(s) of data"
  )}

}
