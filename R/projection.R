
#' compute location of coordinates after great circle projection
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{destPoint}}
#' 
#' 
#' @param x An object used to determine which implementation to use
#' @param ... It's an S3 thing. You wouldn't understand.
#' @param .data An object that inherits from \code{\link[base]{data.frame}}. In 
#'   general this will be on of \code{data.frame}, 
#'   \code{\link[data.table]{data.table}}, or \code{\link[dplyr]{tbl_df}}
#' @param latitude Either a numeric vector of latitudes [degrees] or the column
#'   of \code{.data} which contains latitudes. This maybe quoted or unquoted;
#'   see examples.
#' @param longitude Either a numeric vector of longitudes [degrees]
#'   corresponding with latitude or the column of \code{.data} which contains
#'   longitudes. This maybe quoted or unquoted; see examples.
#' @param bearing Either a numeric vector of bearings [degrees] or the column of
#'   \code{.data} which contains bearings/headings. This maybe quoted or
#'   unquoted see examples.
#' @param distance Either a numeric vector of projection distance [nautical
#'   miles] or the column of \code{.data} which contains projection distances.
#'   This maybe quoted or unquoted see examples.
#' @param output_type string in \code{c("matrix", "data.table", "data.frame", 
#'   "list")}
#' @param method Either \code{"GC"} [default] or \code{"rhumb"}. Used to declare
#'   either a great circle calculation or rhumb line calculation
#' @return If \code{.data} is supplied, an object of the same type and with the
#'   same columns as \code{.data} plus two more, \code{end_latitude} and
#'   \code{end_longitude}.  Otherwise, an object of type determined by
#'   output_type which will generally have two columns, latitude and longitude.
#'   If the input coordinates have length 1, then a named numeric vector is
#'   returned.
#'   
#' @examples
#' # basic use
#' compute_projection(39.86167, -104.6732, 90, 15)
#' compute_projection(39.86167, -104.6732, 86:90, 1:15)
#' 
#' # use inside a data.table
#' library(data.table)
#' apts <- data.table(airport=c("ATL", "DEN", "ORD", "SEA"),
#'                    latitude=c(33.63670, 39.86167, 41.97933, 47.44989),
#'                    longitude=c(-84.42786, -104.67317, -87.90739, -122.31178))
#' apts[, c("platitude", "plongitude"):=compute_projection(latitude, longitude, 90, 15)]
#' 
#' # use with magrittr
#' library(magrittr)
#' apts %>% compute_projection(latitude, longitude, 90, 15)
#' 
#' # columns as strings
#' lat_col <- names(apts)[2]
#' apts %>% compute_projection(lat_col, "longitude", 90, 15)
#' 
#' # predict next position
#' tracks <- data.frame(id = c("a","b","c"),
#'                      lat = 0,
#'                      lon = 0,
#'                      heading = 30,
#'                      ground_speed = seq(300,360, 30))
#' time_step <- 1/60 #one minute
#' 
#' tracks %>% compute_projection(lat, lon, heading, tracks$ground_speed*time_step)
#' 
#' @export
#' @importFrom geosphere destPoint
#' @rdname compute_projection
compute_projection <- function(x, ..., method="GC"){
  UseMethod("compute_projection")
}

#' @export
#' @rdname compute_projection
#' @importFrom dplyr rename bind_cols %>%
compute_projection.data.frame <- function(.data, latitude, longitude, bearing, distance, method="GC"){
  lat_ <- determine_val(.data, substitute(latitude))
  lon_ <- determine_val(.data, substitute(longitude))
  bearing_ <- determine_val(.data, substitute(bearing))
  dist_ <- determine_val(.data, substitute(distance))
  
  compute_projection.numeric(lat_, lon_, bearing_, dist_,output_type = class(.data)) %>%
    rename(end_latitude=latitude, end_longitude=longitude) %>%
    bind_cols(.data,.)%>%
    format_return(class(.data)) %>%
    return
}

# attempt to return vectors from .data or the value provided by supplied argument
determine_val <- function(.data, val){
  cls <- class(val)
  if(cls == "call"){
    val <- eval(val)
    cls <- class(val)
  }  
  if(cls == "name"){
    val <- ifelse(as.character(val) %in% names(.data), as.character(val), eval(val))
  }
  if(cls %in% c("integer","numeric") || is.numeric(val)){
    return(val)
  }
  return(.data[[which(names(.data)==val)]])
}

format_return <- function(.data, return_type){
  if("data.table" %in% return_type){
    data.table::setDT(.data)
    return(.data)
  }
  return(as(object = .data, Class = return_type[1]))
  
}

#' @export
#' @importFrom data.table data.table
#' @importFrom dplyr tbl tbl_df
#' @importFrom geosphere destPoint destPointRhumb
#' @rdname compute_projection
compute_projection.numeric <- function(latitude, longitude, bearing, distance, output_type="data.table", method="GC") {
  valid_out_types <- c("matrix", "data.table", "data.frame", "list", "tbl", "tbl_df")
  if (!any(output_type %in% valid_out_types)) {
    stop(paste0("invalid output type specified, must be one of:\n\t", 
                paste0(valid_out_types, collapse = ", ")))
  }
  
  if(method=="GC")
   points <- destPoint(p=matrix(c(longitude, latitude), ncol = 2, byrow = FALSE), 
                      b=bearing, d=distance, r=.earth.radius.NMi)
  else if (method == "rhumb")
   points <- destPointRhumb(p=matrix(c(longitude, latitude), ncol = 2, byrow = FALSE), 
                                       b=bearing, d=distance, r=.earth.radius.NMi)
  else
    stop("unknown method: ", method, ". Use either GC or rhumb")
  
  if ("matrix" %in% output_type) {
    colnames(points) <- c("longitude", "latitude")
    return(points[,2:1])
  } else if ("data.table" %in% output_type) {
    return(data.table(latitude=points[, "lat"], longitude=points[, "lon"]))
  } else if ("tbl_df" %in% output_type) {
    return(tbl_df(data.frame(latitude=points[, "lat"], longitude=points[, "lon"])))
  } else if ("tbl" %in% output_type) {
    return(tbl(data.frame(latitude=points[, "lat"], longitude=points[, "lon"])))
  } else if ("data.frame" %in% output_type) {
    return(data.frame(latitude=points[, "lat"], longitude=points[, "lon"]))
  } else if ("list" %in% output_type) {
    return(list(list(latitude=points[, "lat"]), list(longitude=points[, "lon"])))
  } 
}

#' compute location of coordinates after rhumb line projection
#' 
#' @description 
#' **DEPRECATED** Please use \code{compute_projection(method="rhumb")}
#' 
#' This function provides a convienant wrapper to 
#' \code{\link[geosphere]{destPointRhumb}}
#' 
#' @param latitude numeric vector of latitudes [degrees]
#' @param longitude numeric vector of longitudes [degrees] corresponding with latitude
#' @param bearing initial bearing [degrees]
#' @param distance projection distance [nautical miles]
#' @param output_type string in \code{c("matrix", "data.table", "data.frame", "list")}
#' @importFrom geosphere destPointRhumb
#' @return a \code{data.table} with columns latitude and longitude. If the input coordinates have length 1, then a named numeric vector is returned if 
#' @export
#' 
compute_projection_rhumb <- function(latitude, longitude, bearing, distance, output_type="data.table") {
  warning("compute_projection_rhumb is deprecated and will go away when earthtools 2.x.y is released.  Use compute_projection(method=\"rhumb\") instead")
 compute_projection.numeric(latitude, longitude, bearing, distance, output_type, method = "rhumb")
}
