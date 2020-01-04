#' Find citations for given host, parasite, or location query.
#'
#' Given a previous query of host, parasite, or location host-parasite occurences, returns a list of citations.
#'
#' @param interactions Interaction dataframe from previous query (findLocation, findParasite, or findHost)

#'
#' @return One column data.frame containing primary literature citation as provided by LMNH
#'
#' @author Anna Willoughby

library(xml2)
library(dplyr)
library(rvest)

# input needs to be data.frame with Reference urls in column 4
findCitations <- function(interactions = NULL){
  ref_df  <- NULL;
  for(i in 1:nrow(interactions)){
    url <- interactions[i,4] # first reference link, will need to loop for all
    webpage <- read_html(url) #pull text from webpage
    cdtble <- html_node(webpage, "table") # extract reference table from webpage
    cdf <- html_table(cdtble, fill = TRUE) # convert to dataframe
    cdf <- cdf[,c(2,3)] # only keep columns of interest
    names(cdf) <- c("variable", "citations") # name columns
    cdf <- filter(cdf, variable == "Reference:") # filter to just the reference
    cdf$variable <- NULL # remove column
    ref_df <- rbind(ref_df, cdf) # add to final dataframes
    }
  ref_df <- unique(ref_df) # remove duplicates
  return(ref_df)
}

# currently this limits to 30 references per interaction
