#' compute turn magnitude
#' 
#' Due to angles being measured in degrees modulo 360 it can be
#' annoying to determine the magnitude of a turn from, for example,
#' 310 degrees to 30 degrees. This function simplifies the operation.
#' 
#' @param angle1 numeric vector, initial heading in degrees
#' @param angle2 numeric vector, terminal heading in degrees
#' @param turn_direction character string, should be one of
#'  \code{"closest", "left", "right"}
#' @export
et_turn_magnitude <- function(angle1, angle2, turn_direction="closest") {
  
  # make everything the same size
  n1 <- length(angle1)
  n2 <- length(angle2)
  nt <- length(turn_direction)
  if (n1>1 & n2>1 & n1!=n2)
    stop("inconsistent angle vectors")
  n <- max(c(n1, n2))
  if (n1<n & n1==1)
    angle1 <- rep(angle1, times=n)
  if (n2<n & n2==1)
    angle2 <- rep(angle2, times=n)
  if (nt<n & nt==1)
    turn_direction <- rep(turn_direction, times=n)
  
  # get direction
  turn_direction <- tolower(turn_direction)
  idL <- turn_direction=="left"
  idR <- turn_direction=="right"
  idC <- turn_direction=="closest"
  
  # compute output
  value <- vector(mode="numeric", length=n)
  
  if (any(idC)) {
    ans1 <- angle1[idC] - angle2[idC]
    ans2 <- 360 - abs(ans1)
    value[idC] <- pmin(abs(ans1), ans2)
  }
  
  if (any(idL)) {
    value[idL] <- (angle1[idL]-angle2[idL]) %% 360
  }
  
  if (any(idR)) {
    value[idR] <- (angle2[idR]-angle1[idR]) %% 360
  }
  
  return(value)
}