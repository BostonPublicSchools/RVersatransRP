#' Get vehicle override report
#'
#' @inheritParams rp_connect
#' @export
rp_report_vehicle_overrides <- function(rp_odbc_name = Sys.getenv("RP_ODBC_NAME"),
                                        database = Sys.getenv("OS_DATABASE"),
                                        TZ = Sys.timezone(),
                                        conn = NULL) {
  #browser()
  conn <- rp_connect(rp_odbc_name = rp_odbc_name, database = database,TZ = TZ, conn = NULL)
  query <- readr::read_file(system.file("sql/os_daily_overrides.sql", package = "RVersatransRP"))
  DBI::dbGetQuery(conn, query)
}
