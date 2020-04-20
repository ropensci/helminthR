#' Find citations for given host, parasite, or location query.
#'
#' Given a previous query of host, parasite, or location host-parasite occurences, returns a list of citations.
#'
#' @param interactions Interaction dataframe from previous query (findLocation, findParasite, or findHost) that had citations = TRUE
#' @param original Boolean. Should the output include the references that are 
#'		not original sources (e.g. review or systematic study)? default is FALSE
#' @return list of lists containing primary literature citation per interaction as provided by LMNH
#'
#' @author Anna Willoughby
#' @examples
#'
#' \donttest{gorillaParasites <- helminthR::findHost("Gorilla", "gorilla", citation = TRUE)}
#' \donttest{findCitations(gorillaParasites)}

# input needs to be data.frame with Reference urls in column 4 and Citation count in column 5
findCitations <- function(interactions = NULL, original = FALSE){
  if(!original) {
    ref_list <- list() # prepare a container
    for(i in 1:nrow(interactions)){
      url <- interactions[i,4] # first reference link, will need to loop for all
      webpage <- read_html(url) #pull text from webpage
      cdtble <- html_node(webpage, "table") # extract reference table from webpage
      cdf <- html_table(cdtble, fill = TRUE) # convert to dataframe
      names(cdf) <- c("ref_id", "variable", "value") # name columns
      cdf <- cdf[which(nchar(cdf$variable)>4),] # remove rows with variable = digit
      cdf <- cdf[,c(1,2,3)] # only keep columns of interest
      cdf$ref_id <- as.integer(substr(cdf$ref_id, 1, nchar(cdf$ref_id)-1)) # extract out count number
      cdf$variable <- substr(cdf$variable, 1, nchar(cdf$variable)-1) # remove colon
      cdf <- fill(cdf, ref_id) # fill in ref_id
      cdf <- cdf[which(cdf$variable == "Reference"),] # filter to only rows of reference 
      tmp <- cdf$value # select just the references
      tmp <- gsub("\r|\n", "", tmp) # clean up references
      tmp <- gsub("                                                    ", "", tmp)# clean up references
      host <- interactions$Host[i]
      parasite <- interactions$Parasite[i]
      name <- paste(host,"-", parasite," Citations", sep='')
      ref_list[[name]] <- tmp # add to final dataframes
    }
    return(ref_list) }
  
  if(original){
    ref_list <- list() # prepare a container
    for(i in 1:nrow(interactions)){
      url <- interactions[i,4] # first reference link, will need to loop for all
      webpage <- read_html(url) #pull text from webpage
      cdtble <- html_node(webpage, "table") # extract reference table from webpage
      cdf <- html_table(cdtble, fill = TRUE) # convert to dataframe
      names(cdf) <- c("ref_id", "variable", "value") # name columns
      cdf <- cdf[which(nchar(cdf$variable)>4),] # remove rows with variable = digit
      cdf <- cdf[,c(1,2,3)] # only keep columns of interest
      cdf$ref_id <- as.integer(substr(cdf$ref_id, 1, nchar(cdf$ref_id)-1)) # extract out count number
      cdf$variable <- substr(cdf$variable, 1, nchar(cdf$variable)-1) # remove colon
      cdf <- fill(cdf, ref_id) # fill in ref_id
      cdf <- pivot_wider(cdf, variable, value) # transform to see all metadata
      cdf <- mutate(cdf, Comments = ifelse(is.null(cdf$Comments) == TRUE,"", cdf$Comments)) # create a comments column if doesn't exist
      cdf <- cdf[which(cdf$Comments != "not original"),]
      cdf <- cdf[which(cdf$Comments != "Not original"),]
      cdf <- cdf[which(cdf$Comments != "not original, epidemiology reviewed"),]
      cdf <- cdf[which(cdf$Comments != "systematics"),]
      cdf <- cdf[which(cdf$Comments != "larvae, not original"),]
      cdf <- cdf[which(cdf$Comments != "Larva, not original"),]
      cdf <- cdf[which(cdf$Comments != "life cycle and distribution reviewed"),]
      cdf <- cdf[which(cdf$Comments != "Not original, Mexican National Collections"),]
      tmp <- cdf$Reference # select just the references
      tmp <- gsub("\r|\n", "", tmp) # clean up references
      tmp <- gsub("                                                    ", "", tmp) # clean up references
      host <- interactions$Host[i]
      parasite <- interactions$Parasite[i]
      name <- paste(host,"-", parasite," Citations", sep='')
      ref_list[[name]] <- tmp # add to final dataframes
    }
    return(ref_list) }
}  
# currently this limits to 30 references per interaction
