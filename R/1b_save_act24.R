save_act24 <- function(a, out_file) {

  gsub("[^.]*\\.", "", out_file) %>%
  {switch(

    .,

    "rds" = saveRDS(a, out_file),
    "csv" = data.table::fwrite(a, out_file),

    stop(
      "Saving to `.", .,
      "` format is not currently supported",
      call. = FALSE
    )

  )}

}
