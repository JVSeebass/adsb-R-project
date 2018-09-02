#' GetLiveData
#'
#' @description This function provides information about current flights,
#' like position, destination and operator in a dataframe format.
#' Flights can be selected by different parameters.
#'
#' @param point A vector containing the latitude and longitude of a specific point or
#' a string containing the name of a city to determine a specicfic position as inital point.
#'
#' @param radius The number of kilometers which should be considered as radius
#' for a circular selection area around the point
#'
#' @param country A string containing the country where an aircraft is registered
#'
#' @param type A string containing a specific aircraft type
#'
#' @return A dataframe containing latitude, longitude, operator, type, country, start and destination
#' as well as the icao (identification) of the selected aircrafts
#'
#' @examples flights_berlin <- GetLiveData(point = c(52.5243, 13.4063))
#' flights_berlin2 <- GetLiveData(point = "Berlin")
#' flights_vienna50 <- GetLiveData(point = "Vienna", radius = 50)
#'
#' flights_australia <- GetLiveData(country = "Australia")
#'
#' flights_B738 <- GetLiveData(type = "B738")
#'
#' flights_mixture <- GetLiveData(point = "Paris", radius = 500, country = "Germany", type = "A320")
#'
#' @references For a table of the selectable cities see \code{\link[maps:world.cities]{world.cities}}.
#' For further information see also \url{https://www.adsbexchange.com/data/}
#'
#' @export GetLiveData


GetLiveData <- function(point = "", radius = 100, country = "", type = "") {

  # create search link for the api
  link <- paste("http://public-api.adsbexchange.com/",
                "VirtualRadar/AircraftList.json?", sep = "")

  if (is.character(point) == FALSE || point != "") {
      # convert city into point
      if(any(point == maps::world.cities[1])) {
        select <- which(maps::world.cities == point)
        if(length(select) > 1) {
          select <- select[which(max(maps::world.cities[select, 3]) == maps::world.cities[select, 3])]
        }
        point <- as.vector(maps::world.cities[select, 4:5])
      }

    # check for necessary conditions
      if(length(point) != 2) {
        stop("Point has to be a city or of length 2!")
      }
      if(point[1] < -90 || point[1] > 90) {
        stop("Latitude has to be between -90 and 90!")
      }
      if(point[2] < -180 || point[2] > 180) {
        stop("Longitude has to be between -180 and 180!")
      }
    link <- paste(link, "lat=", point[1],
                  "&lng=", point[2], "&fDstL=0&fDstU=", radius, sep = "", "&")
  }
  if (country != "") {
    link <- paste(link, "fCouS=", country, sep = "", "&")
  }

  if (type != "") {
    link <- paste(link, "fTypC=", type, sep = "", "&")
  }

  str <- stringr::str_length(link)

  if (substring(link, str, str) != "&") {
    warning("You did not choose any selection parameter.")
  } else {
    link <- substring(link, 1, str-1)
  }

  data <- jsonlite::read_json(link)


  # check if all data selected
  if (length(data$acList) > (data$totalAc - 10)) {
    warning("No selection done! If unintended, please check your function input.")
  }

  # check if no data selected
  if (length(data$acList) == 0) {
    stop("No matches found! Try another input.")
  }


  #data conversion#####

  # lat
  latvec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["Lat"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["Lat"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  latv <- latvec(data)

  # long
  longvec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["Lat"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["Long"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  longv <- longvec(data)

  # type
  typevec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["Type"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["Type"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  typev <- typevec(data)

  # Operator
  opvec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["Op"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["Op"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  opv <- opvec(data)

  # Country
  couvec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["Cou"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["Cou"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  couv <- couvec(data)

  # From
  fromvec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["From"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["From"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  fromv <- fromvec(data)

  # To
  tovec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["To"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["To"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  tov <- tovec(data)


  # ICAO
  icaovec <- function(data) {
    z <- rep(NA, length(data$acList))
    for (i in 1:length(data$acList)) {
      if (length(data[["acList"]][[i]][["Icao"]]) != 0) {
        z[i] <- data[["acList"]][[i]][["Icao"]]
      }
    }
    z <- unlist(z)
    return(z)
  }
  icaov <- icaovec(data)



  # dframe
  dframe <- data.frame(Lat = latv, Long = longv, Op = opv, Type = typev, Cou = couv,
                       From = fromv, To = tov, ICAO = icaov)

  # return #####
  return(dframe)
}
