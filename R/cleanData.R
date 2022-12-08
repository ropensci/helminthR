#' Clean helminth parasite occurrence data
#'
#' Given a host-parasite edgelist, this function can validate species names,
#' provide further taxonomic information (thanks to the \code{'taxize'} package), 
#' and remove records only to genus level.
#'
#' Use \code{data(locations)} for a list of possible locations.
#'
#' @param edge Host-parasite edgelist obtained from \code{\link{findLocation}},
#'        \code{\link{findHost}}, or \code{\link{findParasite}}
#' @param speciesOnly boolean flag to remove host and parasite species
#'        where data are only available at genus level (default = FALSE)
#' @param validateHosts boolean flag to check host species names
#'        against Catalogue of Life information and output taxonomic
#'        information (default = FALSE)
#'
#' @return cleanEdge Host-parasite edgelist, but cleaned
#' @export
#' @author Tad Dallas
#'

cleanData <- function (edge, speciesOnly = FALSE, validateHosts = FALSE){
  if (speciesOnly) {
    if (length(grep("sp\\.", edge$Host)) > 0) {
      edge <- edge[-grep(" sp\\.", edge$Host), ]
    }
    if (length(grep("sp\\.", edge$Parasite)) > 0) {
      edge <- edge[-grep(" sp\\.", edge$Parasite), ]
    }
    if (length(grep("spp\\.", edge$Host)) > 0) {
      edge <- edge[-grep(" spp\\.", edge$Host), ]
    }
    if (length(grep("spp\\.", edge$Parasite)) > 0) {
      edge <- edge[-grep(" spp\\.", edge$Parasite), ]
    }

    if (length(grep(".*\\((.*)\\).*", edge$Host)) > 0) {
      edge <- edge[-grep(".*\\((.*)\\).*", edge$Host), ]
    }
    if (length(grep(".*\\((.*)\\).*", edge$Parasite)) > 0) {
      edge <- edge[-grep(".*\\((.*)\\).*", edge$Parasite), ]
    }
  }

  if (validateHosts) {
		tax <- taxize::classification(edge$Host, db='ncbi')
		tax2 <- tax[!is.na(tax)]
		taxdf <- plyr::ldply(tax2, function(x){
			ret <- rep(NA, 8);
			names(ret) <- c('kingdom', 'phylum', 
				'subclass', 'order','superfamily',
				'family','genus', 'subgenus')
			ret[na.omit(match(x$rank, names(ret)))] <- x$name[!is.na(match(x$rank, names(ret)))]
			return(ret)
			})
			taxdf$subclass <- NULL
			taxdf$subgenus <- NULL
    return(list(HPedge = edge, HostTaxon = taxdf))
  }
  return(edge)
}
