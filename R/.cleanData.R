#' Clean host-parasite data in various ways
#' 
#' Given a host-parasite edgelist, this function can validate species names, provide further taxonomic information, remove records only to genus level, 
#' 
#' Use the `listLocations()` function for a list of possible locations.
#' 
#' @param edge Host-parasite edgelist obtained from `findLocation`, 
#'        `findHost`, or `findParasite`
#' @param speciesOnly boolean flag to remove host and parasite species 
#'        where data are only available at genus level (default = TRUE)
#' @param validateHosts boolean flag to check host species names 
#'        against Catalogue of Life information (default = TRUE)
#' @param validateParasites boolean flag to check parasite species names 
#'        against Catalogue of Life information (default = FALSE)
#' @param outputTaxon boolean flag to output taxonomic information on hosts
#'        (default = FALSE)
#' 
#' @return cleanEdge Host-parasite edgelist, but cleaned
#'
#' @author Tad Dallas
#' 
#' 
#' 

.cleanData <- function(edge, speciesOnly=FALSE, validateHosts = TRUE, 
                       validateParasites = FALSE, outputTaxon = FALSE){
  
 # remove entries not at species level
  if(speciesOnly){
    if(any(grep('sp', edge$Host))){
      edge <- edge[-grep(' sp', edge$Host), ]
    }
    if(any(grep('sp', edge$Parasite))){
      edge <- edge[-grep(' sp', edge$Parasite), ]
    }
  }

 validate <- function(hostName){
     hostName2 <- gsub(' ', '+', as.character(hostName))
     rootClife <- xmlRoot(xmlParse(paste("http://www.catalogueoflife.org/col/webservice?name=", hostName2, "&response=full", sep=''),
                     useInternalNodes=TRUE))
     if(xmlAttrs(rootClife)[['number_of_results_returned']] == 0){
      return(rep(NA, 8))
     }else{
       tax <- data.frame(
          ldply(getNodeSet(rootClife, 'result/classification/taxon/name'), xmlValue),
          ldply(getNodeSet(rootClife, 'result/classification/taxon/rank'), xmlValue)
        )
      colnames(tax) <- c('taxa', 'level')
      tax2 <- unique(tax)
      ret <- unlist(tax2[,1])
      names(ret) <- unlist(tax2[,2])
     }
   return(ret)
   }

 # validate host records
  if(validateHosts){
   taxDF <- ldply(edge$Host, validate)
   if(any(apply(taxDF,1,function(x){all(is.na(x))}))){
    rmv <- which(apply(taxDF,1, function(x){all(is.na(x))}))
    taxDF <- taxDF[-rmv,]
    edge <- edge[-rmv,]
   }
   edge$Host <- paste(taxDF$Genus, taxDF$Species, sep=' ')
  }

  #not enough data on COL to validate parasite records
  if(validateParasites){
    taxDF <- ldply(edge$Parasite, validate)
    if(any(apply(taxDF,1,function(x){all(is.na(x))}))){
     rmv <- which(apply(taxDF,1, function(x){all(is.na(x))}))
     taxDF <- taxDF[-rmv,]
     edge <- edge[-rmv,]
    }
   edge$Parasite <- paste(taxDF$Genus, taxDF$Species, sep=' ')
   }

   if(outputTaxon == FALSE){return(edge)}
   if(outputTaxon){return(list(HPedge = edge, HostTaxon = taxDF))}
}

