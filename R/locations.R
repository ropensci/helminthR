#' Table of geographic location names, and associated coordinates
#'
#' Lists geographic locations that can be input to \code{\link{findHost}} or
#' \code{\link{findParasite}} and the corresponding latitude and longitude coordinates 
#' of the country's centroid. The georeferencing was performed dynamically using the 
#' Google Maps API, but they have since restricted access. The data on locations is now
#' provided in this data file called \code{locations} -- \code{data(locations)} -- and is based on 
#' an earlier usage of \code{ggmap}. The geographic coordinates may not be accurate, and users
#' should check for accuracy (and feel free to file an issue or PR on Github with corrections).
#' 
#'
#' @format
#'  \describe{
#'    \item{Location}{Name of geographic location}
#'    \item{Latitude}{Latitude of location centroid}
#'    \item{Longitude}{Longitude of location centroid}
#'  }
#'
#' @references Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005).
#' Host-Parasite Database of the Natural History Museum, London.
#' @name locations
#' @docType data
#' @keywords data
#' @usage data(locations)

if(getRversion() >= "2.15.1"){
  utils::globalVariables('locations')
}

