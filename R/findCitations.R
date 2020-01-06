#' Find citations for given host, parasite, or location query.
#'
#' Given a previous query of host, parasite, or location host-parasite occurences, returns a list of citations.
#'
#' @param interactions Interaction dataframe from previous query (findLocation, findParasite, or findHost) that had citations = TRUE
#'
#' @return list of lists containing primary literature citation per interaction as provided by LMNH
#'
#' @author Anna Willoughby

library(xml2)
library(rvest)
library(tidyr)
library(reshape2)

# input needs to be data.frame with Reference urls in column 4 and Citation count in column 5
findCitations <- function(interactions = NULL){
  ref_list <- list() # prepare a container
  for(i in 1:nrow(interactions)){
    url <- interactions[i,4] # first reference link, will need to loop for all
    webpage <- read_html(url) #pull text from webpage
    cdtble <- html_node(webpage, "table") # extract reference table from webpage
    cdf <- html_table(cdtble, fill = TRUE) # convert to dataframe
    names(cdf) <- c("ref_id", "variable", "value") # name columns
    cdf <- cdf[which(is.na(str_extract(cdf$variable, "\\d")) == TRUE),]
    cdf <- cdf[,c(1,2,3)] # only keep columns of interest
    cdf$ref_id <- as.integer(substr(cdf$ref_id, 1, nchar(cdf$ref_id)-1)) # extract out count number
    cdf <- fill(cdf, ref_id) # fill in ref_id, relies on tidyr
    cdf <- dcast(cdf, ref_id ~ variable, fill = "", drop = FALSE, value.var = "value") # transform to see all metadata
    tmp <- cdf$`Reference:` # select just the references
    tmp <- gsub("\r|\n", "", tmp) # clean up references
    name <- paste('interaction_',i,'_references', sep='')
    ref_list[[name]] <- tmp # add to final dataframes
    }
  return(ref_list)
}

# currently this limits to 30 references per interaction
