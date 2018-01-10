.earth.radius.NMi = 3438.14021579022

#' get Earth's radius in nautical miles
#' 
#' Obviously the earth is not really a sphere, the
#' radius given here is an approximation such that
#' spherical geometry calculations return close to
#' accurate results
#' @return numeric, Earth's radius in nautical miles
#' @export
get_earth_radius <- function() {
  return(.earth.radius.NMi)
}