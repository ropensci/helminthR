#' @title Access London Natural History Museum host-helminth record database
#'
#' @description helminthR: A programmatic interface to the London Natural History Museum's Host-Parasite database. 
#' The package currently allows you to query by host species, parasite species, and geographic location. There is 
#' no information on prevalence or intensity. 
#'
#' @importFrom ggmap geocode
#' @importFrom XML xmlRoot xmlParse xmlAttrs getNodeSet
#' @importFrom rvest html html_nodes html_text html_attr
#' @importFrom magrittr %>%
#' @name helminthR-package
#' @aliases helminthR
#' @docType package
#' @author Tad Dallas \email{tdallas@@uga.edu}
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
NULL
