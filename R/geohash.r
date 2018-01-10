#setClass("geohash", contains="character", representation = c(hash = "character"))
library(compiler)
library(memoise)
# library(stringr)


# geohash<-function (x){
#   new("geohash", x, hash = x)
# }
# 
# is.geohash <- function(x){
#   is("geohash",x)
# }

#' Decode a geohash to lat/lon
#' 
#' This function takes in a geohash string and returns a 3x2 matrix where the first 
#' column contains the decoded latitude information and the second contains the 
#' deocded longitude information. The first row contains the minimum value; the 
#' second row contains the maximum value and the third row contains the error. To ge the 
#' center of the geohash box, use the hash_center(x) function.
#' 
#' @param x the geohash string
#' @importFrom stringr str_length
#' @importFrom stringr str_sub
#' @importFrom stringr str_locate
#' 
#' @export
#' @author Seth Wenchel <swenchel@@mitre.org>
decode <- function(x){
  require("stringr")
  ll <- matrix(nrow =3, ncol=2, dimnames=list(row_names = c("min", "max", "error"),col_names = c("lat","lon")))
  len <- str_length(x)
  if(len==0)
    return(ll)
  
  ll[,1] <-c(-90, 90, 90)
  ll[,2] <- c(-180,180,180)
  BASE32 <- "0123456789bcdefghjkmnpqrstuvwxyz"
  BITS <- c(16, 8, 4, 2, 1)
  
  isEven <- T;
  
  for(i in seq(1, len)){
    c <- str_sub(string=x, start=i, end=i)
    pos <- as.numeric(unlist(str_locate(BASE32, c)))[1]-1
    for(mask in BITS){
      if(isEven){
        ll[,2] <- refineInterval(ll[,2], pos, mask)
      }
      else{
        ll[,1] <- refineInterval(ll[,1], pos, mask)
      }
      isEven = !isEven
    }
  }
  ll[3,]<- (ll[1,] - ll[2,])/2
  return(ll)
}




#' @importFrom bitops bitAnd
refineInterval<- function(v, pos, mask){
  if(bitops::bitAnd(pos,mask)>0)
    v[1]<-(v[1]+v[2])/2.0
  else
    v[2]<-(v[1]+v[2])/2.0
  return(v)
}

#' A faster version of decode
#' 
#' @export
#' @rdname decode
#' @importFrom compiler cmpfun
#' @author Seth Wenchel <swenchel@@mitre.org>
dcd <- cmpfun(decode)


#' Returns the center of a geohash
#' 
#' Given a geohash this function will calculate the center lat/lon coordinate.  
#' To get the bounds and the error, use the function dcd(x).
#' 
#' @param x a string representing a geohash
#' 
#' @export
#' @author Seth Wenchel <swenchel@@mitre.org>
hash_center <- function(x){
  box<-dcd(x)
  return(data.frame(lat=(sum(box[1:2,1])/2),lon=sum(box[1:2,2])/2))
}

hc_cmp <- cmpfun(hash_center)
enableJIT(3)

#' A faster version of hash_center(x).
#' 
#' Use this version with repeated calls to hash_center when you 
#' might be decoding the same hash more than once.
#' 
#' @export
#' @rdname hash_center
#' @importFrom memoise memoize
#' @author Seth Wenchel <swenchel@@mitre.org>
hc_cmp_mem <- memoize(hc_cmp)

#' encode point into a geohash string
#' 
#' @param latitude numeric value of latitude
#' @param longitude numeric value of latitude
#' @param precision integer length of return value
#' 
#' @return a string representation of the geohash of the point of length precision.
#' 
#' @importFrom stringr str_length
#' @importFrom stringr str_sub
#' @importFrom compiler cmpfun
#' @importFrom bitops bitOr
#' @export encode
#' @examples 
#' # should produce "ezs42"
#' library("earthtools")
#' encode(42.6, -5.6)
#' 
encode <- compiler::cmpfun(function(latitude, longitude, precision=5){
  is_even<-T
  lat<-c(-90,90)
  lon<-c(-180,180)
  bit <- 1
  ch <- 0
  theHash <- ""
  
  BASE32 <- "0123456789bcdefghjkmnpqrstuvwxyz"
  BITS <- c(16, 8, 4, 2, 1)
  
  while(stringr::str_length(theHash)<precision){
    if(is_even){
      mid_lon <- (lon[1]+lon[2])/2
      if(longitude>mid_lon){
        ch <- bitops::bitOr(ch, BITS[bit])
        lon[1] <- mid_lon
      }else
        lon[2] <- mid_lon
    }
    else{
      mid_lat <- (lat[1]+lat[2])/2
      if (latitude > mid_lat){
        ch <- bitops::bitOr(ch, BITS[bit])
        lat[1] <- mid_lat
      }else
        lat[2] <- mid_lat
    }
    is_even <- !is_even
    if(bit<5)
      bit <- bit+1
    else{
      theHash <- paste0(theHash, str_sub(BASE32,ch+1,ch+1))
      bit <- 1
      ch <- 0
    }
  }
  return(theHash)
})
