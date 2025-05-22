#' @title Access London Natural History Museum host-helminth record database
#'
#' @description 'helminthR': A programmatic interface to the London 
#'   Natural History Museum's host-parasite database.
#'
#' The package currently allows you to query by host species, parasite species, 
#' and geographic location. No information is provided on parasite prevalence or intensity. 
#'
#' @references Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005).
#' Host-Parasite Database of the Natural History Museum, London. 
#' <http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/>
#'
#' @importFrom utils tail globalVariables
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom plyr ldply
#' @importFrom xml2 read_html read_xml xml_attr xml_text xml_children xml_find_all
#' @importFrom httr content GET stop_for_status
#' @importFrom utils data
#'
#' @name helminthR-package
#' @aliases helminthR
#' @author Tad Dallas \email{tad.a.dallas@@gmail.com}

"_PACKAGE"
