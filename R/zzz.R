.earth.radius.NMi = 3438.14021579022

#' get Earth's radius in nautical miles
#' 
#' Obviously the earth is not really a sphere, the
#' radius given here is an approximation such that
#' spherical geometry calculations return close to
#' accurate results
#' @return numeric, Earth's radius in nautical miles
#' @export
et_get_earth_radius <- function() {
  if (!is.null(getOption(".earthtools.earth.radius")))
    return(getOption(".earthtools.earth.radius"))
  
  return(.earth.radius.NMi)
}

#' @param radius Numeric scalar. The radius of the earth 
#'   (or any other sphere you care about) in nautical miles.
#' @rdname et_get_earth_radius
#' @export
et_set_earth_radius <- function(radius) {
  options(".earthtools.earth.radius"=radius)
}