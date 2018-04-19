
# info: latitude/Breitengrad von -90(Süd) bis +90(Nord)
# longitude/Längengrad von -180(West) bis 180(Ost)

library("jsonlite")
library("maps")


####Get data####
get_live_data <- function(point,radius = 100){
  library("jsonlite")
  if(TRUE==TRUE){
    if(length(point) != 2 && all(point != world.cities)){
      stop("Point has to be a city or of length 2!")
    }
    if(any(point == world.cities)){
      select <- which(world.cities == point)
      take <- select[which(max(world.cities[select,3])==world.cities[select,3])]
      point <- as.vector(world.cities[take,4:5])
    }
    if(point[1] < -90 || point[1] > 90){
      stop("Latitude has to be between -90 and 90!")
    }
    if(point[2] < -180 || point[2] > 180){
      stop("Latitude has to be between -180 and 180!")
    }
  }
  link <- paste("http://public-api.adsbexchange.com/VirtualRadar/AircraftList.json?lat=",point[1],
          "&lng=",point[2],"&fDstL=0&fDstU=",radius,sep = "")
  z <- read_json(link)
  return(z)
}


dat2 <- get_live_data(point="Berlin")


####Utility####

n_planes <- function(x){
  z <- length(x$acList)
  cat("There are currently", z, "airplanes in the selected area.")
}
n_planes(dat2)

n_planes2 <- function(point,radius=100){
  data <- get_live_data(point,radius)
  z <- length(data$acList)
  cat("There are currently ", z, " airplanes flying ",radius," km around ",point,".",sep = "")
}

n_planes2("Gottingen",50)

# bei Stadtnamen ö durch o ersetzen.