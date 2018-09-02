#' Query position tracking data from the ADSB-Exchange Amazon Redshift cluster
#'
#' Queries the ADSB-Exchange Amazon Redshift Cluster for the parameters:
#' ICAO code; 13 digit UNIX (milliseconds timestamp) at UTC;
#' latitude; longitude; to obtain a data.frame as input for further analysis.
#' Every observation of the aircraft position (latitude, longitude)
#' is linked with a timestamp at every point. This date is itslef linked to
#' aircraft identifier.
#'
#' @param con A connection object which is obtained using \code{\link{ConnectRS}}
#'
#' @param ica A character string of the ICAO code of an aircraft.
#' To find a specific ICAO code of an aircraft, go e.g. to
#' \href{https://www.flightradar24.com/59.09,13.09/6}{Flightradar24.com} and
#' choose an aircraft. The left hand pop-up window contains a section
#' describing the aircraft type, registration and MODE-S CODE which is the ICAO
#' code.
#'
#' @param start A character string of the start date and time when the aircraft
#' should start being tracked. Format: "YYYY-MM-DD hh:mm:ss" Time zone must be
#' UTC
#'
#' @param end A character string of the end date and time when the aircraft
#' should stop being tracked. Format: "YYYY-MM-DD hh:mm:ss" Time zone must be
#' UTC
#'
#' @param limit Is the maximum number of rows queried from the DB. Is set to
#' 10 by default to prevent large queries.
#'
#' @return Returns a data.frame which can be used for
#' analysis with the plotting function \code{\link{PlotAirways}}
#'
#' @details Function arguments ica, start and end are character strings.
#' Enter start and end in UTC date/time. Local times must be coverted
#' beforehand into UTC Time by using e.g. \code{\link{anytime}}
#'
#' GetHistoricADSBExchange(con, ica, start, end, limit = 10)
#'
#' @examples \dontrun{Not generally executable example:
#' GetHistoricADSBExchange(con = con, ica = "86D660", start = "2018-08-31 01:00:00",
#' end = "2018-08-31 12:10:00", limit = 10)}
#'
#' @importFrom DBI dbGetQuery
#' @import anytime dplyr
#'
#' @export GetHistoricADSBExchange


# Writing a function which retrieves parameters (columns) from
# a database, which are used as inputs for postion tracking analysis functions

GetHistoricADSBExchange <- function(con, ica, start, end, limit = 10){

# I)
# Translate start (I.I) and end (I.II) timepoints first into UNIX (10 digits).
# Later, when buliding the SQL query command (II), the UNIX (13 digit) format
# is created, since this is the format in which the timestamps are stored in
# the DB.

# I.I)
# Translate start timepoint for collecting observations of position data
# into unix (10 digit)
start <- start
start.unix <- as.numeric(as.POSIXct(
  start,
  format = "%Y-%m-%d %H:%M:%OS",
  tz = "UTC"))
start.unix

# I.II)
# Translate end timepoint for collecting observations of position data
# into unix (10 digit)
end <- end
end.unix <- as.numeric(as.POSIXct(
  end,
  format = "%Y-%m-%d %H:%M:%OS",
  tz = "UTC"))
end.unix

# II)

# II.I)
# Generate SQL statement to query DB.
# Query selects the variables ICAO code, 13 digit UNIX timestamp,
# latitude and longitude for a specific ICAO code from the position
# table. The selected rows are ordered by the timestamp (ascending).
# The range of selected timestamps is determined by the start and end values
# of the function inputs for those parameters.

SQL.statem <- paste(
  "SELECT", " ", "icao", ", ", "postime" ,
  ", ", "lat", ", ", "long",
  " FROM"," ", "position",
  " WHERE icao =", "'", ica, "'",
  " AND ", "postime",
  " >= ", "'", as.numeric(paste(start.unix, "000", sep ="")), "'",
  " AND ", "postime",
  " <= ", "'", as.numeric(paste(end.unix, "000", sep ="")),  "'",
  " ORDER BY ", "postime",
  " LIMIT" , " ", limit,";", sep = "")

# II.II)
# query the DB using the DB connection object and the SQL statement from II.I)
 df <- dbGetQuery(con, SQL.statem)
 df$postime <- anytime(as.numeric(substring(df$postime, 1, 10), tz = "UTC"))

 return(df)

}
