#' @rdname get_activity_info
#' @param info internal object storing useful information related to activity
#'   intervals, multitasking, etc.
#' @param type The type of assembly to perform, either activity-based
#'   (\code{type="Activity}) or index-based (\code{type="Index})
#' @keywords internal
assembler <- function(info, type) {

  stopifnot(type %in% c("Activity", "Index"))

  info$matches %>%
    lapply(function(x) {
      switch(
        type,
        "Activity" =
          if (
            !length(names(info$intervals)[x])
          ) "Not reported" else names(info$intervals)[x],
        "Index" =
          x
      ) %>%
        c(info$dummy_val) %>%
        {.[info$dummy_ind]} %>%
        matrix(ncol = info$max_activities)
    }) %>%
    do.call(rbind, .) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(paste0(type, info$dummy_ind)) %T>%
    {if (type == "Activity") stopifnot(!anyNA(.$Activity1))} %>%
    list(.) %>%
    stats::setNames(type)

}
