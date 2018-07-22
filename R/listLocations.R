#' Lists geographic locations
#' 
#' Lists geographic locations that can be input to \code{\link{findHost}} or
#' \code{\link{findParasite}} and the corresponding latitude and longitude coordinates 
#' of the country's centroid. The georeferencing is performed dynamically using 
#' \code{ggmap} and is potentially incorrect. 
#' 
#' 
#' @return Returns a vector of locations. However, it is important to note that
#' some locations are nested inside of other locations. For instance, you can
#' search for all host-parasite associations in the Great Lakes as a whole, or
#' you can target Lake Ontario specifically. 
#'
#' @author Tad Dallas
#' @references Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005).
#' Host-Parasite Database of the Natural History Museum, London. 
#' @export
#' @examples
#' \donttest{helminthR::listLocations()}


listLocations <-function(){

  url <- "http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/database/location.jsp"
  args <- list(dbfnsRowsPerPage='500000', 
    x='13', y='5',
    paragroup=NULL,
    fmsubgroup=NULL,
    subgroup=NULL,
    fmparagenus='Contains',
    paragenus=NULL,
    fmparaspecies='Contains',
    paraspecies=NULL,
    fmhostgenus=NULL,
    hostgenus=NULL, 
    fmhostspecies=NULL,
    hostspecies=NULL,
    location=NULL,
    hstate=NULL,
    pstatus=NULL,
    showparasites='on',
    showhosts='on',
    showrefs='on',
    groupby=NULL,
    getlocation='select')
  loc0 <- GET(url, query = args)
  stop_for_status(loc0) 

	if(loc0$status_code != 200){
		stop('Error: the NHM website is temporarily unreachable. Please try again.')
	}

	locations <- content(loc0, 'parsed') %>%  
		html_nodes("option") %>%
		html_attr("value")

	if(any(locations == "")){
		loc <- locations[-which(locations == "")]
	}

	loc1 <- vector()
	for(i in seq_len(length(loc))){
		if(any(unlist(strsplit(loc[i],'')) == ":" )){
			locTmp <- unlist(strsplit(loc[i], ':'))
			loc1[i] <- trimws(locTmp[length(locTmp)])
		}else{
			loc1[i] <- loc[i]
		}
	}

  latLong <- geocode(loc1)
  ret <- data.frame(Location = loc1, Latitude = latLong[,1], 
		Longitude = latLong[,2], stringsAsFactors = FALSE)
  return(ret)
}
