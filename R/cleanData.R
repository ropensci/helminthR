#' Clean helminth parasite occurrence data
#'
#' Given the data or a subset of the helminth interaction data 
#' (see `loadData()`), perform various cleaning functions on the
#'  data, including validation of species names, run taxize on 
#' hosts (again, as there are already host and parasite taxonomic
#' variables as part of this data.frame , and remove 
#' records only to genus level.
#'
#' Use \code{data(locations)} for a list of possible locations.
#'
#' @param data helminth interaction data
#' @param speciesOnly boolean flag to remove host and parasite species
#'        where data are only available at genus level (default = FALSE)
#' @param validateHosts boolean flag to check host species names
#'        against Catalogue of Life information and output taxonomic
#'        information (default = FALSE)
#'
#' @return data.frame with cleaned data
#' @export
#' @author Tad Dallas
#'

cleanData <- function (data, speciesOnly = FALSE, validateHosts = FALSE){
  if (speciesOnly) {
    ind <- grep(" sp\\.", data$hostScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(" sp\\.", data$parasiteScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(" spp\\.", data$hostScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(" spp\\.", data$parasiteScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(".*\\((.*)\\).*", data$hostScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(".*\\((.*)\\).*", data$parasiteScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(".*\\[", data$hostScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(".*\\[", data$parasiteScientificName)
    if (length(ind) > 0) {
      data <- data[!ind,]
    }
    ind <- grep(' NA', data$hostScientificName)
    if(length(ind)>0){
      data <- data[!ind,]
    }
  }

  if (validateHosts) {
		tax <- taxize::classification(data$hostScientificName, db='ncbi')
		tax2 <- tax[!is.na(tax)]
		taxdf <- lapply(tax2, function(x){
			ret <- rep(NA, 8);
			names(ret) <- c('kingdom', 'phylum', 
				'subclass', 'order','superfamily',
				'family','genus', 'subgenus')
			ret[na.omit(match(x$rank, names(ret)))] <- x$name[!is.na(match(x$rank, names(ret)))]
			return(ret)
			})
      taxdf <- do.call(rbind, taxdf)
      taxdf <- as.data.frame(taxdf)
			taxdf$subclass <- NULL
			taxdf$subgenus <- NULL
    return(list(HPedge = data, HostTaxon = taxdf))
  }
  return(data)
}
