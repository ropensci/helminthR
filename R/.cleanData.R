#' Clean host-parasite data in various ways
#' 
#' Given a host-parasite edgelist, this function can validate species names, provide further taxonomic information, remove records only to genus level, 
#' 
#' Use the `listLocations()` function for a list of possible locations.
#' 
#' @param edge Host-parasite edgelist obtained from `findLocation`, 
#'        `findHost`, or `findParasite`
#' @param genusLevel boolean flag to remove host and parasite species 
#'        where data are only available at genus level (default = FALSE)
#' @param validateRecords boolean flag to check host species names 
#'        against Catalogue of Life information (default = TRUE)
#' @param outputTaxon boolean flag to output taxonomic information on hosts
#'        (default = FALSE)
#' 
#' 
#' 
#' @return 
#' @author Tad Dallas
#' 
#' 
#'
#' 

.cleanData <- function(edge, genusLevel=FALSE, validateRecords = TRUE, 
	                   outputTaxon = FALSE){
  
 # remove entries not at species level
  if(genusLevel){

  }

 # validate records
  if(validateRecords){

  }


 # output taxonomic information?
  if(outputTaxon){
    
  }
  
  
 return(cleanEdge)
}






