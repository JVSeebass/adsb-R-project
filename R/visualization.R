#' NoPlanes
#'
#' @description This function returns the number of aircrafts in a selected area.
#'
#' @param point A string containing the name of a city.
#'
#' @param radius The number of kilometers which should be considered as radius
#' for a circular area around the point.
#'
#' @return A sentence which defines the area and the number of aircraft in it.
#' "There are currently \emph{x} airplanes in the area \emph{y} km around \emph{city}."
#'
#' @examples NoPlanes("New York")
#' NoPlanes("Copenhagen", radius = 200)
#'
#' @details See also \code{\link{GetLiveData}}
#'
#' @export NoPlanes



NoPlanes <- function(point = "", radius=100) {
  if (is.character(point)) {
    data <- GetLiveData(point, radius)
    z <- length(data[, 1])
    message("There are currently ", z, " airplanes in the area ", radius,
            " km around ", point, ".", sep = "")
  } else {
    stop("Please take a city as function input.")
  }
}




#' PlotPlanes
#'
#' @description Creates an interactive map with markers at the current position of aircraft.
#' Clicking on the marker will provide further information about the aircraft.
#'
#' @param point A vector containing the latitude and longitude of a specific point or
#' a string containing the name of a city to determine a specicfic position as inital point.
#'
#' @param radius The number of kilometers which should be considered as radius
#' for a circular selection area around the point.
#'
#' @param country A string containing the country where an aircraft is registred.
#'
#' @param type A string containing a specific aircraft type.
#'
#' @examples PlotPlanes("London", 50)
#' PlotPlanes(country = "Algeria")
#' PlotPlanes(type = "A319")
#'
#' @details For further examples see \code{\link{GetLiveData}}
#'
#' @return An interactive worldmap on which the position of the aircraft is displayed.
#'
#' @importFrom leaflet %>%
#'
#' @import leaflet
#'
#' @export PlotPlanes

PlotPlanes <- function(point = "", radius = 100, country = "", type = "") {

  # choose type of data
  data <- GetLiveData(point, radius, country, type)

  # popup information
  info <- c()
  for (i in 1:length(data$acList)) {
    info <- c(info, paste("<b>From: </b>", data$From, "<br/>",
                          "<b>To: </b>", data$To, "<br/>",
                          "<b>Operator: </b>", data$Op, "<br/>",
                          "<b>Type: </b>", data$Type, "<br/>",
                          "<b>ICAO: </b>", data$ICAO,
                          sep = ""))
  }

  # create a map
  map <- leaflet::leaflet() %>% leaflet::addTiles()

  # plot a marker for every airplane with additional information
  map %>% leaflet::addMarkers(data$Long, data$Lat, popup = info)
}




#' PlotAirways
#'
#' @description A function which plots the flightroute of an aircraft.
#'
#' @param con A connection object which is obtained using \code{\link{ConnectRS}}
#'
#' @param ica A character string of the ICAO code of an aircraft (MODE-S CODE).
#' To find a specific MODE-S CODE of an aircraft, go e.g. to
#' \href{https://www.flightradar24.com/59.09,13.09/6}{Flightradar24.com} and
#' choose an aircraft. The left hand pop-up window contains a section
#' describing the aircraft type, registration and MODE-S CODE.
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
#' @return A map plotting the past postion marks of an aircraft in a
#' selected time frame.
#'
#' @details See also \code{\link{GetHistoricADSBExchange}}
#' for further information and how to get the con object
#'
#' @examples \dontrun{
#' PlotAirways(con = con, ica = "4010EB", start = "2016-06-09 05:32:49",
#' end = "2016-06-09 18:55:35", limit = 3)}
#'
#' @export PlotAirways

PlotAirways <- function(con, ica, start, end, limit = 10){
  data <- GetHistoricADSBExchange(con, ica, start, end, limit)
  info <- c()
  for (i in 1:length(data[, 1])) {
    info <- c(info, paste("<b>Time: </b>", data$postime,
                          sep = ""))
  }
  map  <- leaflet::leaflet() %>% leaflet::addTiles()
  map %>% leaflet::addPolylines(data$long, data$lat) %>%
    leaflet::addCircleMarkers(data$long, data$lat, popup = info, radius = .5)
}
