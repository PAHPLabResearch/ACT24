#' Deidentify ACT24 data
#'
#' @param track data frame of tracking information (must minimally contain
#'   \code{id} and \code{act_id} columns)
#' @param file character scalar giving path to the desired ACT24 data file (must
#'   be a \code{.csv} file)
#' @param verbose logical. Print updates to console?
#'
#' @return Tracking information about the ACT24 output. By design, the function
#'   has side effects to save the output to \code{.rds} files in the folder
#'   designated in the \code{out_dir} argument
#' @export
#'
#' @examples
deidentify_ACT24 <- function(
  track = .dummy_tracker,
  file = system.file("extdata/sample_file.csv", package = "ACT24"),
  out_dir = tempdir(),
  verbose = FALSE
) {

  if (!grepl(".csv$", file)) stop(
    basename(file), " is not a csv file", call. = FALSE
  )

  if (out_dir == tempdir()) message(
    "\nSaving output to temp directory (", out_dir, ")",
    "\nTo save it somwhere more convenient, provide a",
    " value for the `out_dir` argument"
  )

  ids <-
    is.na(track$act_id) %>%
    ifelse(track$id, track$act_id)

  track$act24_file <- NA_character_

  act <-
    file %T>%
    {stopifnot(file.exists(.))} %>%
    {suppressWarnings(data.table::fread(
      ., stringsAsFactors = FALSE
    ))} %>%
    data.frame(stringsAsFactors = FALSE) %>%
    act24_prep(.) %>%
    split(., paste(.$UserName, .$RecallDate))

  for (i in seq(act)) {

    timer <- PAutilities::manage_procedure(
      "Start", "\n\nProcessing ACT24 --", i, "of",
      length(act), verbose = verbose
    )

    ## Set up and check

    a <-
      act[[i]] %>%
      {.[ ,setdiff(names(.), .act24_drop_names)]}

    ok <- act24_setup_check(a, ids)
    if (!ok) {
      next
    } else {

      track_index <- match(a$UserName[1], ids)

      ## Format and de-identify

      a %<>% act24_fix_dates(track, track_index)

      track$act24_file[track_index] <- out_file

      ## Event data

      out_dir %>%
      file.path(paste0(a$id[1], "_ACT24_events.rds")) %>%
      saveRDS(a, .)

      ## Minute-by-minute data

      act24_wrapper(a$id[1], a, verbose) %>%
      saveRDS(file.path(
        out_dir,
        paste0(a$id[1], "_ACT24_60s.rds")
      ))

      ## Done!

      PAutilities::manage_procedure(
        "End", "\nDone! Processing took",
        round((proc.time() - timer)[3]/60, 1),
        "minutes.", verbose = verbose
      )

    }
  }

  message("\nCheck for hidden pop-up")

  "Processing complete. Please manually record warnings in" %>%
  paste("`data-raw/ACT24/1_warnings.txt`") %>%
  svDialogs::dlgMessage("ok")

  track

}
