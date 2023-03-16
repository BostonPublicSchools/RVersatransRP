#' Guess DNS name
#'
#' @export
rp_guess_dsn <- function() {
  txt <- paste0(system2("powershell", args = c("-NoLogo", "-NonInteractive", "-NoProfile", "-Command Get-OdbcDsn"), stdout = TRUE), collapse = "\n")
  txt <- strsplit(txt, "\n\n")[[1]]
  txt <- txt[c(-1, -length(txt))]
  txt <- strsplit(txt, "\n")
  txt <- lapply(txt, function(x) {
    m <- do.call(rbind, strsplit(x, "\\W+:\\W+"))
    m2 <- m[, 2]
    names(m2) <- m[, 1]
    m2
  })
  values <- dplyr::bind_rows(txt)
  best_guess <- dplyr::filter(values, grepl("SQL", .data$DriverName))$Name
  if(length(best_guess) < 1) stop("`rp_odbc_name` must specify the ODBC data source pointing to the RP database")
  message("Note: `rp_odbc_name` not specified, using \"", best_guess[[1]], "\"")
  best_guess[[1]]
}

#' Connect to RP database
#'
#' @description  Establish a connection to a Versatrans Routing and Planning database using
#' the DBI/odbc packages in R.
#'
#' @details You must set up a ODBC data source for the Versatrans RP database. You must
#' be connected to the BPS network (e.g., via VPN if you are remote). See
#' https://support.microsoft.com/en-us/office/administer-odbc-data-sources-b19f856b-5b9b-48c9-8b93-07484bfab5a7
#'
#' #' Connection parameters `rp_odbc_name` and `database` should usually be
#' specified in a .Renviron file, see
#' https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf
#' if you are not familiar with seting envionment variables in R.
#'
#' @param rp_odbc_name Name of the Windows ODBC data source to use. Retrieved
#' from `RP_ODBC_NAME` environment variable by default but can be overridden here,
#'  see details.
#' @param database Name of the default database to connect to. Retrieved
#' from `RP_DATABASE` environment variable by default but can be overridden here,
#'  see details.
#' @param TZ The timezone used by the database server.
#' @param conn Existing DBI connection to use. If `NULL` (the default) a new connection will be opened.
#' @export
#' @examplesIf Sys.getenv("RP_ODBC_NAME")!=""&Sys.getenv("RP_DATABASE")!=""
#'
#' conn <- rp_connect()
#' {tbls <- DBI::dbListTables(conn); head(tbls)}
#' DBI::dbDisconnect(conn)
#'
rp_connect <- function(rp_odbc_name = Sys.getenv("RP_ODBC_NAME"),
                       database = Sys.getenv("RP_DATABASE"),
                       TZ = Sys.timezone(),
                       conn = NULL) {
  if(rp_odbc_name == "") rp_odbc_name <- rp_guess_dsn()
  if(database == "") {
    database <- "Sandbox"
    message("Note: `database` not specified, using \"Sandbox\"")
  }

  if(!inherits(conn, "Microsoft SQL Server")) {
    if(is.null(conn)) conn <- DBI::dbConnect(odbc::odbc(), rp_odbc_name, database = database, timezone = TZ, timezone_out = TZ)
  } else {
    if(DBI::dbGetInfo(conn)$dbname != database) {
      conn <- odbc::dbConnect(odbc::odbc(), rp_odbc_name, database = database, timezone = TZ, timezone_out = TZ)
    }
  }
  conn
}
