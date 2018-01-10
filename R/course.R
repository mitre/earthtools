#' compute initial bearing along a great circle path
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{bearing}}
#' 
#' @param latitude1 numeric vector of latitudes [degrees]
#' @param longitude1 numeric vector of longitudes [degrees] corresponding with latitude1
#' @param latitude2 numeric vector of latitudes [degrees]
#' @param longitude2 numeric vector of longitudes [degrees] corresponding with latitude2
#' @return numeric, bearing in degrees
#' @export
compute_bearing_initial <- function(latitude1, longitude1, latitude2, longitude2) {
  return(geosphere::bearing(
    p1=matrix(c(longitude1, latitude1), ncol = 2, byrow = FALSE),
    p2=matrix(c(longitude2, latitude2), ncol = 2, byrow = FALSE)))
}

#' compute terminal bearing along a great circle path
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{finalBearing}}
#' 
#' @param latitude1 numeric vector of latitudes [degrees]
#' @param longitude1 numeric vector of longitudes [degrees] corresponding with latitude1
#' @param latitude2 numeric vector of latitudes [degrees]
#' @param longitude2 numeric vector of longitudes [degrees] corresponding with latitude2
#' @return numeric, bearing in degrees
#' @export
compute_bearing_terminal <- function(latitude1, longitude1, latitude2, longitude2) {
  return(geosphere::finalBearing(
    p1=matrix(c(longitude1, latitude1), ncol = 2, byrow = FALSE),
    p2=matrix(c(longitude2, latitude2), ncol = 2, byrow = FALSE)))
}

#' compute bearing along a rhumb line
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{bearingRhumb}}
#' 
#' @param latitude1 numeric vector of latitudes [degrees]
#' @param longitude1 numeric vector of longitudes [degrees] corresponding with latitude1
#' @param latitude2 numeric vector of latitudes [degrees]
#' @param longitude2 numeric vector of longitudes [degrees] corresponding with latitude2
#' @return numeric, bearing in degrees
#' @export
compute_bearing_rhumb <- function(latitude1, longitude1, latitude2, longitude2) {
  return(geosphere::bearingRhumb(
    p1=matrix(c(longitude1, latitude1), ncol = 2, byrow = FALSE),
    p2=matrix(c(longitude2, latitude2), ncol = 2, byrow = FALSE)))
}