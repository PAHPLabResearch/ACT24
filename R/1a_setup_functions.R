# csv reading/formatting --------------------------------------------------

  read_act_csv <- function(file, posix_format) {
    file %T>%
    {if (!file.exists(.)) stop(basename(file), " file not found")} %>%
    {suppressWarnings(data.table::fread(
      ., stringsAsFactors = FALSE
    ))} %>%
    data.frame(stringsAsFactors = FALSE) %>%
    act24_prep(posix_format) %T>%
    {if (!all(c("UserName", "RecallDate") %in% names(.))) stop(
      "ACT24 dataset does not have both `UserName` and `RecallDate` columns"
    )} %>%
    split(., paste(.$UserName, .$RecallDate))
  }

  act24_prep <- function(act, posix_format = "%m/%d/%Y %H:%M") {

    act %>%
    stats::setNames(., gsub("\\.+$", "", names(.))) %T>%
    {if (!all(
      c("RecallDate", "ReportingDate",
        "ActivityStartTime", "ActivityEndTime") %in%
      names(.)
    )) stop(
      "ACT24 dataset is missing these columns: ",
      paste(
        setdiff(
          c(
            "RecallDate", "ReportingDate",
            "ActivityStartTime", "ActivityEndTime"
          ),
          names(.)
        ),
        collapse = ", "
      )
    )} %>%
    within({

      RecallDate <- as.POSIXct(
        RecallDate, "UTC", format = "%m/%d/%Y"
      )

      ReportingDate <- as.POSIXct(
        ReportingDate, "UTC", format = "%m/%d/%Y"
      )

      ActivityStartTime <- as.POSIXct(
        ActivityStartTime, "UTC", format = posix_format
      )

      ActivityEndTime <- as.POSIXct(
        ActivityEndTime, "UTC", format = posix_format
      )

    }) %T>%
    {if (all(is.na(.$ActivityStartTime) & is.na(.$ActivityEndTime))) stop(
      "\nFailed to parse `ActivityStartTime` and `ActivityEndTime`",
      "\nTry passing a different value for the `posix_format` argument",
      " (currently set to \"", posix_format, "\")", call. = FALSE
    )}

  }

# Other setup functions ---------------------------------------------------

  not_okay <- function(a, ids) {

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

    !ok

  }

  act24_fix_dates <- function(a, track, track_index) {

    if (is.character(a$ActivityStartTime)) {
      stopifnot(is.character(a$ActivityEndTime))
      a$ActivityStartTime %<>% as.POSIXct("UTC", "%m/%d/%Y %H:%M")
      a$ActivityEndTime %<>% as.POSIXct("UTC", "%m/%d/%Y %H:%M")
    }

    stopifnot(
      inherits(a$ActivityStartTime, "POSIXt"),
      inherits(a$ActivityEndTime, "POSIXt")
    )

    offset <-
      as.Date("1111-11-11") %>%
      difftime(as.Date(a$ActivityStartTime), ., units = "sec") %>%
      unique(.) %T>%
      {stopifnot(length(unique(.)) == 1)}

    a %>%
    {stats::setNames(., gsub("^UserName$", "id", names(.)))} %>%
    within({
      id = paste0(id, "_TrackRow", track_index)
      RecallDate = as.Date("1111-11-11")
      ActivityStartTime = ActivityStartTime - offset
      ActivityEndTime = ActivityEndTime - offset
    }) %>%
    structure(., row.names = seq(nrow(.)))

  }

  strip_dates <- function(string) {
    gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}", "", string) %>%
    gsub(" +$", "", .) %>%
    gsub("^ +", "", .)
  }
