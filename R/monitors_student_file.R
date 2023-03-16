#' Create student monitor report
#'
#' @description Equivalent to *Import/Export => Student => Monitors student file*
#'
#' @param database Character vector of length one specifying the database to connect to
#' @param format Character vector of length one specifying how the data should be
#' formatted. If `"long"` (the default) return separate *rows* for each weekday/pickup/dropoff.
#' If `"wide"` return separate *columns* for each weekday/pickup/dropoff.
#' @importFrom rlang .data
#' @export
#' @examplesIf Sys.getenv("RP_ODBC_NAME")!=""&Sys.getenv("RP_DATABASE")!=""
#'
#' student_monitor <- rp_report_student_monitor()
#'
#' ## data fields:
#' dplyr::glimpse(student_monitor[0, ])
#'
#'
rp_report_student_monitor <- function(database=Sys.getenv("RP_DATABASE"),
                                      format = c("long", "wide")) {

  format <- format[[1]]
  if(!format %in% c("long", "wide")) stop("`format` must be one of \"long\" or \"wide\"")
  ## get and prepare student data
  conn <- rp_connect(database = database)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  student <- dplyr::tbl(conn, "v_crosstab_student")
  student_data <- dplyr::select(
    student,
    "RecordID",
    "Student ID",
    "Last, First",
    "Program Description",
    School.Abbrev = "Default School Abbrev",
    School.Anchor.Abbrev = "Default School Anchor Abbrev" ,
    School.Name = "Default School Name"
  )
  student_data <- dplyr::collect(student_data)
  names(student_data) <- make.names(names(student_data))
  student_data <- dplyr::filter(
    student_data,
    tolower(trimws(.data$Program.Description)) %in%
      tolower(trimws(
        c("AC ACCOMMODATED CR FULL D",
          "ACA ACCOMMODATED CR AM",
          "ACA ACCOMMODATED CR PM",
          "CA CORNER AM",
          "CA CORNER PM",
          "CR CORNER FULL DAY",
          "DA DOOR AM",
          "DA DOOR PM",
          "DR DOOR FULL DAY",
          "M MEDICAL FULL DAY",
          "MA MEDICAL AM",
          "MA MEDICAL PM",
          "NA NONAMBULATORY FULL DAY",
          "NAP NONAMBULATORY PM")))
  )
  names(student_data) <- make.names(names(student_data))

  ## get and prepare info data
  info <- dplyr::collect(dplyr::tbl(conn, "StudentInfoFields"))
  names(info) <- make.names(names(info))
  info_data <- dplyr::mutate(
    info,
    INFO_WC = ifelse(.data$InfoID %in% c(1, 15) & .data$Description == "WC", .data$Description, NA),
    INFO_NEED_MONITOR = ifelse(.data$InfoID %in% c(1, 15) & .data$Description %in% c("1", "M"), .data$Description, NA))
  info_data <- dplyr::select(info_data, "StudentID", "INFO_WC", "INFO_NEED_MONITOR")
  info_data <- dplyr::filter(info_data, !is.na(.data$INFO_WC) | !is.na(.data$INFO_NEED_MONITOR))
  info_data <- dplyr::distinct(info_data)
  info_data <- dplyr::summarize(
    dplyr::group_by(info_data, .data$StudentID),
    INFO_WC = paste0(.data$INFO_WC[!is.na(.data$INFO_WC)], collapse=","),
    INFO_NEED_MONITOR = paste0(.data$INFO_NEED_MONITOR[!is.na(.data$INFO_NEED_MONITOR)], collapse = ","))
  info_data <- dplyr::mutate(info_data, dplyr::across(dplyr::everything(), function(x) ifelse(x == "", NA, x)))

  ## get and prepare pickup and dropoff data)
  pickup <- dplyr::collect(dplyr::tbl(conn, "v_crosstab_PickUpRouteBus"))
  names(pickup) <- make.names(names(pickup))
  pickup_data <- dplyr::rename(dplyr::distinct(pickup), Pickup.Route = "Route", Pickup.Bus = "Bus")
  dropoff <- dplyr::tbl(conn, "v_crosstab_DropOffRouteBus")
  dropoff_data <- dplyr::collect(dplyr::rename(dplyr::distinct(dropoff), Dropoff.Route = "Route", Dropoff.Bus = "Bus"))
  names(dropoff_data) <- make.names(names(dropoff_data))
  ## merge
  full_data <- dplyr::left_join(student_data, info_data, by = c(RecordID = "StudentID"))
  full_data <- dplyr::left_join(full_data, pickup_data, by = c(RecordID = "StudentID"))
  full_data <- dplyr::left_join(full_data, dropoff_data, by = c(RecordID = "StudentID", "Days"))
  names(full_data) <- gsub(".", "_", names(full_data), fixed = TRUE)
  full_data <- dplyr::relocate(full_data, "INFO_WC", "INFO_NEED_MONITOR", .after = "Program_Description")

  if(format == "wide") {
    ## reformat with separate bus and route columns by day
    wide_data <- dplyr::mutate(full_data, Days = factor(.data$Days, levels = c("M", "T", "W", "H", "F"), labels = c("Mon.", "Tue.", "Wed.", "Thu.", "Fri.")))
    wide_data <- tidyr::pivot_wider(
      wide_data,
      names_from = "Days",
      values_from = c("Pickup_Bus", "Pickup_Route", "Dropoff_Bus",  "Dropoff_Route"),
      names_vary = "slowest",
      names_sort = TRUE,
      names_glue = "{Days}_{.value}")

    # cleanup missing days (we could just drop entirely, but keeping for compatibility with RP export)
    wide_data <- dplyr::select(wide_data, -"RecordID", -dplyr::starts_with("NA_"), -dplyr::starts_with("_"))

    final_data <- wide_data
  }
  if(format == "long") {
    final_data <- tidyr::pivot_longer(
      full_data,
      cols = c("Pickup_Route", "Pickup_Bus", "Dropoff_Route", "Dropoff_Bus"),
      names_to = c("Shift", ".value"),
      names_sep = "_")
    final_data <- dplyr::rename(final_data, Day = "Days")
  }

  # cleanup names, for compatibility with RP export
  names(final_data) <- gsub("_", " ", names(final_data), fixed = TRUE)
  names(final_data) <- gsub("  ", ", ", names(final_data), fixed = TRUE)

  final_data
}
