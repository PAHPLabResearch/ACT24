# Global Variables --------------------------------------------------------

if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "RecallDate", "ReportingDate", "ActivityStartTime",
  "ActivityEndTime", "METs", "Primary_Activity", "Primary_Index",
  "student", "target_freq"
))

.act24_drop_names <- c(
  "ProjectName", "RecallRecID", "RecallNo", "ReportingDate"
)

.act24_ee_names <- c(
  "id", "RecallDate", "Category", "Activity",
  "ActivityStartTime", "ActivityEndTime", "Duration",
  "Met_Sleep", "Met_Sed", "Met_Act1", "Met_Act2",
  "Percent_Sed", "Percent_Act1", "Percent_Act2",
  "BehaviorClassification", "ActivityLevel",
  "EverPrimaryIndicator_Mets", "EverPrimaryIndicator_BC_Mets",
  "Duration_Primary", "MetHours_Primary", "GEN_Posture",
  "GEN_Other", "GEN_Intensity"
)

.minutes <-
  as.POSIXct("1111-11-11 00:00:00", "UTC") %>%
  {. + (60 * (0:1439))} %>%
  data.frame(Timestamp = .)

.strf_mins <- strftime(.minutes$Timestamp, "%H:%M:%S", "UTC")

.dummy_tracker <- data.frame(
  id = "ID",
  act24_id = "ACT24_ID"
)

# Imports -----------------------------------------------------------------

#' @importFrom magrittr %>% %T>% %<>% %$%
NULL

# Package Documentation ---------------------------------------------------

#' R Interface for the Activities Completed over Time in 24 Hours Instrument
#'
#' Provides functions for processing ACT24 data in event and minute-by-minute
#' format, with additional steps to deidentify the input.
#'
#' @name ACT24-package
NULL
