#' Find host-parasite interactions for a given location
#'
#' Given a location (available from `listLocations()`) this function returns
#' all host-parasite associations in that location.
#'
#' Use the `listLocations()` function for a list of possible locations.
#'
#' `hostState` can take values 1-6 corresponding to if the recorded host was found (1) "in the wild",
#'  (2) "Zoo captivity", (3) "Domesticated" , (4) "Experimental", (5) "Commercial source", or
#'  (6) "Accidental infestation". A vaule of NULL should be entered if you would like to include all hostStates.
#'
#' @param location Location of host-parasite interaction.
#' @param citation Boolean. Should the output include the citation link? default is FALSE
#' @param hostState number corresponding to one of six different host states. The default value is NULL
#'        includes all host states
#' @param speciesOnly boolean flag to remove host and parasite species
#'        where data are only available at genus level (default = TRUE)
#' @param validateHosts boolean flag to check host species names
#'        against Catalogue of Life information and output taxonomic
#'        information (default = FALSE)
#'
#' @return Three (or four) column data.frame containing host species, parasite species
#' (shortened name and full name), and citation link (optional), with each row
#' corresponding to an occurrence of a parasite species on a host species.
#'
#' @author Tad Dallas
#' @seealso \code{\link{findHost}}
#' @references Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005).
#' Host-Parasite Database of the Natural History Museum, London.
#' @examples
#'
#' \dontrun{ FrenchHostPars <- findLocation(location='France')}
#'

findLocation <- function(location = NULL, citation = FALSE, hostState = NULL,
                         speciesOnly = FALSE, validateHosts = FALSE) {

  if(is.null(location)){
    stop("Please choose a location from the possible locations in the listLocations() function")
  }

  data(locations)
  if (location %in% locations[,1] == FALSE) {
    stop("Please choose a location from the possible locations in the listLocations() function")
    }

    location1 <- gsub("\\+", "%2B", location)
    location2 <- gsub("\\(", "%28", location1)
    location3 <- gsub("\\)", "%29", location2)
    location4 <- gsub(" ", "+", location3)
    location <- location4

    hpUrl <- html(paste("http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/database/results.jsp?dbfnsRowsPerPage=50000&x=15&y=10&paragroup=&fmsubgroup=Starts+with&subgroup=&fmparagenus=Starts+with&paragenus=&fmparaspecies=Starts+with&paraspecies=&fmhostgenus=Contains&hostgenus=&fmhostspecies=Contains&hostspecies=&location=", location, "&hstate=", hostState, "&pstatus=&showparasites=on&showhosts=on&showrefs=on&groupby=parasite&search=Search", sep = ""))

    names <- hpUrl %>% html_nodes(".searchlink") %>% html_text()
    hpList <- matrix(names, ncol = 2, byrow = TRUE)
    parNames <- sapply(hpList[, 1], strsplit, " ")
    parNames2 <- lapply(parNames, function(a) {
        if (length(a) < 2) {
            return(a)
        } else {
            return(paste(a[1], a[2], sep = " "))
        }
    })
    parNames3 <- lapply(parNames2, function(a) {
        gsub("\\(|\\)", "", a)
    })
    names(parNames3) <- NULL
    parNamesShort <- unlist(parNames3)

    if(citation == FALSE){
    ret <- data.frame(Host = hpList[, 2], Parasite = parNamesShort,
                      ParasiteFull = hpList[, 1])
    }

    if(citation){
      citeLinks <- hpUrl %>% html_nodes("td~ td+ td a") %>% html_attr("href")
      citations <- paste("http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/database/", citeLinks, sep='')
      ret <- data.frame(Host = hpList[, 2], Parasite = parNamesShort,
                      ParasiteFull = hpList[, 1],
                      Reference = citations)
    }

    ret <- cleanData(ret, speciesOnly = speciesOnly , validateHosts = validateHosts)
    return(ret)
}
