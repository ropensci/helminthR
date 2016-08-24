#' Clean host-parasite data in various ways
#'
#' Given a host-parasite edgelist, this function can validate species names, provide further taxonomic information, remove records only to genus level,
#'
#' Use the `listLocations()` function for a list of possible locations.
#'
#' @param edge Host-parasite edgelist obtained from `findLocation`,
#'        `findHost`, or `findParasite`
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
#'
#'


cleanData <- function (edge, speciesOnly = FALSE, validateHosts = FALSE)
{
    if (speciesOnly) {
        if (length(grep("sp\\.", edge$Host)) > 0) {
            edge <- edge[-grep(" sp\\.", edge$Host), ]
        }
        if (length(grep("sp\\.", edge$Parasite)) > 0) {
            edge <- edge[-grep(" sp\\.", edge$Parasite), ]
        }
        if (length(grep(".*\\((.*)\\).*", edge$Host)) > 0) {
            edge <- edge[-grep(".*\\((.*)\\).*", edge$Host),
                ]
        }
        if (length(grep(".*\\((.*)\\).*", edge$Parasite)) > 0) {
            edge <- edge[-grep(".*\\((.*)\\).*", edge$Parasite),
                ]
        }
    }
    if (validateHosts) {
        validate <- function(hostName) {
            hostName <- as.character(hostName)
            if (length(grep("sp\\.", hostName)) == 1) {
                hostName2 <- unlist(strsplit(hostName, " "))[1]
            }
            else {
                hostName2 <- unlist(strsplit(hostName, " "))[1:2]
            }
            rootClife <- read_xml(paste("http://www.catalogueoflife.org/col/webservice?name=",
                hostName2[1], "&response=full", sep = ""))
            if (xml_attr(rootClife, "number_of_results_returned") ==
                0) {
                return(rep(NA, 8))
            }
            else {
                for (i in 1:length(xml_children(rootClife))) {
                  taxInfo <- xml_text(xml_find_all(xml_children(rootClife)[i],
                    "classification/taxon/name"))
                  names(taxInfo) <- xml_text(xml_find_all(xml_children(rootClife)[i],
                    "classification/taxon/rank"))
                  if (any(names(taxInfo) == "Genus") && taxInfo[which(names(taxInfo) ==
                    "Genus")] == hostName2) {
                    ret <- taxInfo
                    break
                  }
                }
            }
            return(ret)
        }

        taxMat <- matrix(NA, ncol = 8, nrow = nrow(edge))
        colnames(taxMat) <- c("Kingdom", "Phylum", "Class", "Order",
            "Superfamily", "Family", "Genus", "Subgenus")
        for (q in 1:nrow(edge)) {
            temp <- validate(edge$Host[q])
            taxMat[q, which(names(temp) %in% colnames(taxMat))] <- unlist(temp)
        }

       if (any(apply(taxMat, 1, function(x) {all(is.na(x))}))) {
            rmv <- which(apply(taxMat, 1, function(x) {all(is.na(x))}))
            taxMat <- taxMat[-rmv, ]
            edge <- edge[-rmv, ]
        }
        return(list(HPedge = edge, HostTaxon = taxMat))
    }
    return(edge)
}
