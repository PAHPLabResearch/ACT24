act24_prep <- function(act) {

  act$RecallDate <- as.POSIXct(
    act$RecallDate, "UTC", format = "%m/%d/%Y"
  )

  act$ReportingDate <- as.POSIXct(
    act$ReportingDate, "UTC", format = "%m/%d/%Y"
  )

  act$ActivityStartTime <- as.POSIXct(
    act$ActivityStartTime, "UTC", format = "%m/%d/%Y %I:%M:%S %p"
  )

  act$ActivityEndTime <- as.POSIXct(
    act$ActivityEndTime, "UTC", format = "%m/%d/%Y %I:%M:%S %p"
  )

  stats::setNames(act, gsub("\\.+$", "", names(act)))

}

act24_setup_check <- function(a, ids) {

  ok <- TRUE

  a %<>%
    {.[ ,setdiff(names(.), .act24_drop_names)]} %>%
    structure(., row.names = seq(nrow(.)))

  id <- a$UserName[1]

  if (!id %in% ids) {
    warning(
      id, " does not have a matching entry in the",
      " data tracker. Skipping.", call. = FALSE
    )
    message(
      "\n", id, " does not have a matching entry in the",
      " data tracker. Skipping."
    )
    ok <- FALSE
  }

  ok

}

act24_fix_dates <- function(a, track, track_index) {

  if (is.character(a$ActivityStartTime)) {
    stopifnot(is.character(a$ActivityEndTime))
    a$ActivityStartTime %<>% as.POSIXct("UTC", "%m/%d/%Y %H:%M")
    a$ActivityEndTime %<>% as.POSIXct("UTC", "%m/%d/%Y %H:%M")
  }

  offset <-
    as.Date("1111-11-11") %>%
    difftime(as.Date(a$ActivityStartTime), ., units = "sec") %>%
    unique(.) %T>%
    {stopifnot(length(unique(.)) == 1)}

  a %>%
  {stats::setNames(., gsub("^UserName$", "id", names(.)))} %>%
  within({
    id = track$id[track_index]
    RecallDate = as.Date("1111-11-11")
    ActivityStartTime = ActivityStartTime - offset
    ActivityEndTime = ActivityEndTime - offset
  }) %>%
  structure(., row.names = seq(nrow(.)))

}
