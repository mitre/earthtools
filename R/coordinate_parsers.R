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

  tmp_list <- tstrsplit(strIn ,"-")
  
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


