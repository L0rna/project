x <- individual [1:5, ]
checkmate::assert_numeric(x$uid)
#only gives error message if it doesnt work 

geosphere::destPoint(
  p= cbind(x$decimalLongitude, x$decimalLatitude),
  b= x$stemAzimuth, d = x$stemDistance
)


#' Calculate the location of a tem based on azimuth and distance
#'
#' @param decimalLongitude numeric vector of decimal longs
#' @param decimalLatitude numeric vector of decimal lats
#' @param stemAzimuth numeric vector of stem azimuths 
#' @param stemDistance numeric vector of stem dist
#'
#' @return a tibble of pairs of coordinates

get_stem_location <- function(
    decimalLongitude, decimalLatitude,
    stemAzimuth, stemDistance){
 
   geosphere::destPoint(
    p= cbind(decimalLongitude, decimalLatitude),
    b= stemAzimuth, d = stemDistance
  )|>
    tibble::as_tibble()
}

test <- get_stem_location(
  x$decimalLongitude, x$decimalLatitude,
  x$stemAzimuth, x$stemDistance
)


#input validation check
  checkmate::assert_numeric(decimalLatitude)
  checkmate::assert_numeric(decimalLongitude)
  checkmate::assert_numeric(stemAzimuth)
  checkmate::assert_numeric(stemDistance)

out <- geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude),
                            b = stemAzimuth, d = stemDistance) %>%
  tibble::as_tibble()


#chck outut for na's
checkmate::assert_false(any(is.na(out)))

return(out)


###
###
###
#### dowloaded script data ####
#### 
#### 
#' Calculate the location of a stem based on azimuth and distance
#'
#' @param decimalLongitude numeric vector of decimal longitudes
#' @param decimalLatitude numeric vector of decimal latitudes
#' @param stemAzimuth numeric vector of stem azimuths
#' @param stemDistance numeric vector of stem distances
#'
#' @return A pair of coordinates

get_stem_location <- function(decimalLongitude, decimalLatitude,
                              stemAzimuth, stemDistance)
  
  {
  # check inputs are correct type (numeric)
  checkmate::assert_numeric(decimalLatitude)
  checkmate::assert_numeric(decimalLongitude)
  checkmate::assert_numeric(stemAzimuth)
  checkmate::assert_numeric(stemDistance)
  
  
  
  
  
  
out <- geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude),
                              b = stemAzimuth, d = stemDistance) %>%
    tibble::as_tibble()
  
  # check output for NAs
  checkmate::assert_false(any(is.na(out)))
  
  return(out)
}
