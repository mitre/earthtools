
# haversin <- function(x) {
#   return ((1 - cos(x)) / 2)
# }
# 
# ahaversin <- function(y) {
#   return (2 * asin(sqrt(y)))
# }
# #----
# #' R implementation of spherical distance via haversine
# #' 
# #'
# #' @author Kevin Sprong <ksprong@@mitre.org>
# calcSphericalDistance <- function(lat1, lon1, lat2, lon2) {
#   # spherical great circle distance, lat/lon can be scalars, 
#   # scalar + vector, or vectors of the same size
#   # ALL INPUTS IN DEGREES
#   
#   # convert to radians
#   lat1 = lat1*pi/180
#   lat2 = lat2*pi/180
#   lon1 = lon1*pi/180
#   lon2 = lon2*pi/180
#   
#   # haversine formula
# #   re = 3438.14021579022   # radius of earth in nm
#   return (.earth.radius.NMi * ahaversin(haversin(lat2 - lat1) + 
#                            cos(lat1) * cos(lat2) * haversin(lon2 - lon1)))
#   
# }


# #----
# #' true course between lat/lon points
# #' 
# #'
# #' @author Kevin Sprong <ksprong@@mitre.org>
# calcTrueCourse <- function(lat1, lon1, lat2, lon2) {
#   # function to return the true course between four equally sized 
#   # vectors of lat lon points
#   # ALL INPUTS IN DEGREES
#   # OUTPUT IN DEGREES
#   
#   # sign of x and y
#   sy <- lat2 - lat1 
#   sx <- lon2 - lon1
#   
#   # distance between x and y
#   dy <- calcSphericalDistance(lat1, lon1, lat2, lon1) 
#   dx <- calcSphericalDistance(lat1, lon1, lat1, lon2) 
#   
#   # use spherical distance corrected for positive/negative sign
#   iy = !is.na(sy) & sy < 0 
#   dy[iy] <- -1 * dy[iy]
#   
#   ix = !is.na(sx) & sx < 0 
#   dx[ix] <- -1 * dx[ix]
#   
#   # return aviation course converted from euclidean angle
#   return ((90 - 180/pi*(atan2(dy, dx))) %% 360)
# }


#' parse coordinates from latitude/longitude + direction strings
#' 
#' parse lat or lon strings of the form nnnnnn.nnnn<Direction>
#' 
#' @export
#' @author Kevin Sprong <ksprong@@mitre.org>
parse_lls <- function(strIn) {
  # parse string in for seconds -> decimal degrees and direction
  degreesOut <- as.double(substr(strIn, 1, nchar(strIn) - 1)) / 3600
  direc <- substr(strIn, nchar(strIn), nchar(strIn))
  
  # assign negative sign where appropriate
  degreesOut[!is.na(direc) & direc == "S"] <- (-1 * 
    degreesOut[!is.na(direc) & direc == "S"])
  degreesOut[!is.na(direc) & direc == "W"] <- (-1 * 
    degreesOut[!is.na(direc) & direc == "W"])
  
  return (degreesOut)
}


#' parse coordinates from latitude/longitude minute-second-direction strings
#' 
#' parse lat/lon strings of the form XXX-YY-ZZ.ZZZ<Direction>
#' 
#' @importFrom data.table tstrsplit
#' @export
#' @author Kevin Sprong <ksprong@@mitre.org>
parse_lldms <- function(strIn) {

  tmp_list <- data.table::tstrsplit(strIn ,"-")
  
  # third list element will have both arcseconds and direction
  n <- nchar(tmp_list[[3]])
  sec <- substr(tmp_list[[3]], 1, n - 1)
  direc <- substr(tmp_list[[3]], n, n)
  
  # smash together
  degreesOut <- (as.double(tmp_list[[1]]) + as.double(tmp_list[[2]])/60 + 
                   as.double(sec)/3600)
  
  degreesOut[!is.na(direc) & direc == "S"] <- (-1 * 
    degreesOut[!is.na(direc) & direc == "S"])
  degreesOut[!is.na(direc) & direc == "W"] <- (-1 * 
    degreesOut[!is.na(direc) & direc == "W"])
  
  return (degreesOut)
}


