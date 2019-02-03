#' Find host-parasite interactions for a given parasite species.
#'
#' Given a host genus and/or species, this function returns a matrix containing
#' host-parasite interaction data. Search available locations using 
#' \code{data(locations)}.
#'
#' \code{hostState} can take values 1-6 corresponding to if the recorded host 
#' was found 
#' \itemize{ 
#'		\item (1) "In the wild"
#'		\item (2) "Zoo captivity" 
#'		\item (3) "Domesticated"
#'		\item (4) "Experimental"
#'		\item (5) "Commercial source"
#'		\item (6) "Accidental infestation"
#'  }
#'
#' @param genus Parasite genus
#' @param species Parasite species
#' @param group Parasite group - Cestodes, Acanthocephalans, Monogeneans, 
#'   Nematodes, Trematodes, or Turbellarian etc. (Turb)
#' @param subgroup Parasite subgroup (family names largely)
#' @param location Location of host-parasite interaction.
#' @param citation Boolean. Should the output include the citation link and 
#'   the number of supporting citations? default is FALSE
#' @param hostState number corresponding to one of six different host states. 
#'   The default value is NULL
#'        includes all host states
#' @param speciesOnly boolean flag to remove host and parasite species
#'        where data are only available at genus level (default = FALSE)
#' @param validateHosts boolean flag to check host species names
#'        against Catalogue of Life information and output taxonomic
#'        information (default = FALSE)
#' @param removeDuplicates (boolean) should duplicate host-parasite 
#'   combinations be removed? (default is FALSE)
#'
#' @return Three (or five) column data.frame containing host species, 
#'   parasite species (shortened name and full name), and citation link and 
#'   number of citations (if \code{citation = TRUE}), with each row corresponding 
#'   to an occurrence of a parasite species on a host species.
#'
#' @author Tad Dallas
#' @seealso \code{\link{findHost}}
#' @references Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005).
#' Host-Parasite Database of the Natural History Museum, London. 
#' <http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/>
#' @export
#' @examples
#'
#' \donttest{strongHosts <- helminthR::findParasite(genus = "Strongyloides")}
#'
#' # An example of how to query multiple parasite species when 
#' # you have a vector of parasite species names
#'
#' parasites <- c("Ascaris aculeati", "Oxyuris flagellum")
#' \donttest{
#'  plyr::ldply(parasites, 
#'    function(x){
#'      helminthR::findParasite(unlist(strsplit(x, " "))[1], 
#'        unlist(strsplit(x," "))[2])
#'    }
#'  )
#' }

findParasite <- function(genus = NULL, species = NULL, 
		group = NULL, subgroup = NULL, 
		location = NULL, citation = FALSE,
    hostState = NULL, speciesOnly = FALSE,
    validateHosts = FALSE, removeDuplicates=FALSE) {

  if(!is.null(group)){
    if(all((group == c("Cestodes", "Acanthocephalans", "Monogeneans", 
			"Nematodes","Trematodes", "Turb")) == FALSE)){
      stop("Parasite group must be one of the following: Cestodes,
        Acanthocephalans, Monogeneans, Nematodes, Trematodes, Turb")
    }
  }

  if(!is.null(location)){
    if(location %in% locations[,1] == FALSE) {
        stop("Please choose a location from the possible 
					locations... data(locations)")
    }
  }
  url <- "http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/database/results.jsp"

  args <- list(dbfnsRowsPerPage='500000', 
    x='13', y='5',
    paragroup=group, 
    fmsubgroup='Contains',
    subgroup=subgroup,
    fmparagenus='Contains',
    paragenus=genus,
    fmparaspecies='Contains',
    paraspecies=species,
    fmhostgenus=NULL,
    hostgenus=NULL, 
    fmhostspecies=NULL,
    hostspecies=NULL,
    location=location,
    hstate=hostState,
    pstatus=NULL,
    showparasites='on',
    showhosts='on',
    showrefs='on',
    groupby='parasite',
    search='Search')
  hp <- GET(url, query = args)
  stop_for_status(hp) 
	
	if(hp$status_code != 200){
		stop("Error: the NHM website is temporarily unreachable. Please try again.")
	}

	hp2 <- content(hp, "parsed")

	names <- hp2 %>%
		html_nodes(".searchlink") %>%
		html_text()

	hpList <- matrix(names, ncol = 2, byrow = TRUE)
	parNames <- vapply(hpList[, 1], strsplit, " ", FUN.VALUE=list(character(1)))
	parNames2 <- lapply(parNames, function(a) {
		if (length(a) < 2) {
			return(a)
		}
		if(any(grepl(".*\\[(.*)\\].*", a)) | any(grepl(".*\\(.*\\).*", a))){
			return(paste(a[1], a[3], sep= " "))
		}else{
			return(paste(a[1], a[2], sep = " "))
		}
	})

  parNames3 <- lapply(parNames2, function(a) {
    gsub("\\(|\\)", "", a)
	})
	names(parNames3) <- NULL
	parNamesShort <- unlist(parNames3)
	ret <- data.frame(Host = hpList[, 2], Parasite = parNamesShort, 
		ParasiteFull = hpList[, 1], stringsAsFactors = FALSE)

	if(citation){
	  citeLinks <- hp2 %>% html_nodes("td~ td+ td a") %>% html_attr("href")
	  citeNumber <- hp2 %>% html_nodes("td~ td+ td a") %>% html_text()
	  citeNumber <- plyr::laply(strsplit(citeNumber, " "), 
			function(x){as.numeric(x[1])})

		citations <- paste("http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/database/", citeLinks, sep="")

	ret <- data.frame(Host = hpList[, 2], Parasite = parNamesShort,
		ParasiteFull = hpList[, 1],
		Reference = citations,
		CitationNumber = citeNumber, stringsAsFactors = FALSE)
	}

	if(removeDuplicates){
		ret <- ret[!duplicated(ret[,1:2]), ]
	}

	ret <- cleanData(ret, speciesOnly = speciesOnly, 
		validateHosts = validateHosts)
	return(ret)
}
