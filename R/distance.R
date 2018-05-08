#' compute spherical distance
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{distHaversine}}
#' 
#' @param latitude1 numeric vector of latitudes [degrees]
#' @param longitude1 numeric vector of longitudes [degrees] corresponding with latitude1
#' @param latitude2 numeric vector of latitudes [degrees]
#' @param longitude2 numeric vector of longitudes [degrees] corresponding with latitude2
#' @importFrom geosphere distHaversine
#' @return numeric, distance in nautical miles
#' @export
et_dist_haversine <- function(latitude1, longitude1, latitude2, longitude2) {
  return(distHaversine(
    p1=matrix(c(longitude1, latitude1), ncol = 2, byrow = FALSE),
    p2=matrix(c(longitude2, latitude2), ncol = 2, byrow = FALSE),
    r=.earth.radius.NMi))
}

#' compute spherical distance along a rhumb line
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{distRhumb}}
#' 
#' @param latitude1 numeric vector of latitudes [degrees]
#' @param longitude1 numeric vector of longitudes [degrees] corresponding with latitude1
#' @param latitude2 numeric vector of latitudes [degrees]
#' @param longitude2 numeric vector of longitudes [degrees] corresponding with latitude2
#' @importFrom geosphere distRhumb
#' @return numeric, distance in nautical miles
#' @export
et_dist_rhumb <- function(latitude1, longitude1, latitude2, longitude2) {
  return(distRhumb(
    p1=matrix(c(longitude1, latitude1), ncol = 2, byrow = FALSE),
    p2=matrix(c(longitude2, latitude2), ncol = 2, byrow = FALSE),
    r=.earth.radius.NMi))
}

#' compute spherical distance along path
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{alongTrackDistance}}. Per that documentation, this measures
#' the distance along the great circle defined by the \code{path_*} inputs to the point(s) 
#' on that path nearest the point(s) defined by \code{latitude} and \code{longitude}. 
#' Note that the input point(s) need not be on the path
#' 
#' @param latitude numeric vector of latitudes [degrees]
#' @param longitude numeric vector of longitudes [degrees] corresponding with latitude
#' @param path_latitude1 numeric vector of latitudes [degrees]
#' @param path_longitude1 numeric vector of longitudes [degrees] corresponding with path_latitude1
#' @param path_latitude2 numeric vector of latitudes [degrees]
#' @param path_longitude2 numeric vector of longitudes [degrees] corresponding with path_latitude2
#' @importFrom geosphere alongTrackDistance
#' @return numeric, distance along path in nautical miles
#' @export
et_dist_along_path <- function(latitude, longitude, path_latitude1, path_longitude1,
                            path_latitude2, path_longitude2) {
  return(alongTrackDistance(
    p1=matrix(c(path_longitude1, path_latitude1), ncol = 2, byrow = FALSE),
    p2=matrix(c(path_longitude2, path_latitude2), ncol = 2, byrow = FALSE),
    p3=matrix(c(longitude, latitude), ncol = 2, byrow = FALSE),
    r=.earth.radius.NMi))
}

#' compute spherical cross track distance (error)
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{dist2gc}}. Per that documentation, this measures
#' the distance from the input point(s) defined by \code{latitude} and 
#' \code{longitude} to the nearest location on the great circle
#' path defined by the \code{path_*} inputs. 
#' 
#' @param latitude numeric vector of latitudes [degrees]
#' @param longitude numeric vector of longitudes [degrees] corresponding with latitude
#' @param path_latitude1 numeric vector of latitudes [degrees]
#' @param path_longitude1 numeric vector of longitudes [degrees] corresponding with path_latitude1
#' @param path_latitude2 numeric vector of latitudes [degrees]
#' @param path_longitude2 numeric vector of longitudes [degrees] corresponding with path_latitude2
#' @importFrom geosphere dist2gc
#' @return numeric, cross track distance nautical miles
#' @export
et_dist_cross_track <- function(latitude, longitude, path_latitude1, path_longitude1,
                             path_latitude2, path_longitude2) {
  return(dist2gc(
    p1=matrix(c(path_longitude1, path_latitude1), ncol = 2, byrow = FALSE),
    p2=matrix(c(path_longitude2, path_latitude2), ncol = 2, byrow = FALSE),
    p3=matrix(c(longitude, latitude), ncol = 2, byrow = FALSE),
    r=.earth.radius.NMi))
}

#' compute spherical distance to a line/polygon
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{dist2Line}}. Per that documentation, this measures
#' the distance from 
#' 
#' @param latitude1 numeric vector of latitudes [degrees]
#' @param longitude1 numeric vector of longitudes [degrees] corresponding with latitude1
#' @param latitude2 numeric vector of latitudes [degrees] defining a polygon (or line)
#' @param longitude2 numeric vector of longitudes [degrees] corresponding with latitude2
#' @param latitude3 numeric vector of latitudes [degrees]
#' @param longitude3 numeric vector of longitudes [degrees] corresponding with latitude3
#' @importFrom geosphere dist2Line
#' @return numeric, distance along path in nautical miles
#' @export
et_dist_to_polygon <- function(latitude, longitude, poly_latitude, poly_longitude) {
  return(dist2Line(
    p1=matrix(c(longitude, latitude), ncol = 2, byrow = FALSE),
    p2=matrix(c(poly_longitude, poly_latitude), ncol = 2, byrow = FALSE),
    distfun=function(p1, p2) {return(geosphere::distHaversine(p1=p1, p2=p2, r=.earth.radius.NMi))}))
}