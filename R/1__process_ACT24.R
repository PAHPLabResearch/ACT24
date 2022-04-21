#' Process an ACT24 report
#'
#' These reports contain aggregated data from many participants. The function
#' will process the data and save separate files for each participant.
#'
#' @param file character scalar giving path to the desired ACT24 data file (must
#'   be a \code{.csv} file)
#' @param out_dir character scalar giving path to the desired folder for saving
#'   output
#' @param out_format character scalar. Desired format of the output files,
#'   either \code{"rds"} or \code{"csv"}
#' @param deidentify logical. Deidentify the files (anonymize dates?)
#' @param verbose logical. Print updates to console?
#' @param ending_prompt logical. Run interactive prompting at the end of the
#'   process, to make sure warnings are logged?
#' @param posix_format character scalar indicating the way timestamps are
#'   formatted in \code{file} (see \url{https://www.stat.berkeley.edu/~s133/dates.html})
#' @param track data frame of tracking information (must minimally contain
#'   \code{id} and \code{act24_id} columns)
#'
#' @return Tracking information about the ACT24 output. By design, the function
#'   has side effects to save the output to \code{.rds} files in the folder
#'   designated in the \code{out_dir} argument, whereas the return value is
#'   designed for tracking the data/output.
#' @export
#'
#' @examples
#' process_ACT24(system.file("extdata/sample_file.csv", package = "ACT24"))
process_ACT24 <- function(
  file, out_dir = tempdir(), out_format = c("rds", "csv"),
  deidentify = FALSE, verbose = FALSE, ending_prompt = FALSE,
  posix_format = "%m/%d/%Y %H:%M", track = .dummy_tracker
) {

  #* Setup

    if (!grepl(".csv$", file)) stop(
      basename(file), " is not a csv file", call. = FALSE
    )

    if (out_dir == tempdir()) message(
      "\nSaving output to temp directory (", tempdir(), ")",
      "\n  To save it somwhere more convenient, provide a",
      " value for the `out_dir` argument,\n  or set",
      " `out_dir = choose.dir()` to pick a spot via GUI"
    )

    act <- read_act_csv(file, posix_format)

    if (deparse(substitute(track)) == ".dummy_tracker") {

      ids <-
        names(act) %>%
        gsub(" .*", "", .)

      track <- data.frame(id = names(act), act24_id = ids)

    } else {

      ids <-
        is.na(track$act24_id) %>%
        ifelse(track$id, track$act24_id)

    }

    track$act24_file <- NA_character_

    out_format <- match.arg(out_format)

  #* Run the loop

    for (i in seq(act)) {

      timer <- PAutilities::manage_procedure(
        "Start", "\n\nProcessing ACT24 --", i, "of",
        length(act), verbose = verbose
      )

      ## Set up and check

        a <-
          act[[i]] %>%
          dplyr::select(!dplyr::any_of(.act24_drop_names))

        if (not_okay(a, ids)) next

        track_index <- match(a$UserName[1], ids)

        out_files <-
          dplyr::nth(ids, i) %>%
          gsub(" ", "_", .) %>%
          paste0("_TrackRow", track_index) %>%
          paste0(c("__ACT24_events.", "__ACT24_60s."), out_format) %>%
          file.path(out_dir, .) %>%
          stats::setNames(c("events", "mxm"))

        if (any(file.exists(out_files))) {
          warning(
            "Skipping `", gsub("__.*$", "", basename(out_files["events"])),
            "` because output file(s) already exist(s)", call. = FALSE
          )
          if (verbose) message(
            "Skipping `", gsub("__.*$", "", basename(out_files["events"])),
            "` because output file(s) already exist(s)"
          )
          next
        }

      ## Format and de-identify

        a %<>% act24_fix_dates(track, track_index)

        track$act24_file[track_index] <-
          unname(out_files["events"]) %>%
          gsub("__.*\\.", "***.", .)

      ## Event data

        save_act24(a, out_files["events"])

      ## Minute-by-minute data

        dplyr::first(a$id) %>%
        act24_wrapper(a, verbose) %>%
        save_act24(out_files["mxm"])

      ## Done!

        PAutilities::manage_procedure(
          "End", "\nDone! Processing took",
          round((proc.time() - timer)[3]/60, 1),
          "minutes.", verbose = verbose
        )

    }

  #* Finish up

    message("\nCheck for hidden pop-up")

    "Processing complete. Please manually record warnings in" %>%
    paste("`data-raw/ACT24/1_warnings.txt`") %>%
    svDialogs::dlgMessage("ok")

    track %>%
    within({
      id = strip_dates(id)
      act24_id = strip_dates(act24_id)
    })

}
