#' @title Access London Natural History Museum host-helminth record database
#'
#' @description helminthR: A programmatic interface to the London Natural History Museum's Host-Parasite database.
#'
#' The package currently allows you to query by host species, parasite species, 
#' and geographic location. No information is provided on parasite prevalence or intensity. 
#'
#' @importFrom utils tail globalVariables
#' @importFrom ggmap geocode
#' @importFrom rvest html html_nodes html_text html_attr
#' @importFrom magrittr %>%
#' @importFrom plyr ldply
#' @importFrom xml2 read_html read_xml xml_attr xml_text xml_children xml_find_all
#' @importFrom httr content GET stop_for_status
#' @name helminthR-package
#' @aliases helminthR
#' @docType package
#' @author Tad Dallas \email{tdallas@@ucdavis.edu}

NULL
