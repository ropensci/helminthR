#' Load the helminth interaction data 
#'
#'
#'
#' @param outdir output directory (default is the top-level of the package repo)
#'
#' @return data.frame of host-helminth interactions
#' @references Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005).
#' Host-Parasite Database of the Natural History Museum, London.
#' @export
#' @author Tad Dallas


loadData <- function(outdir='.'){
  if(!file.exists('helminths.csv')){
    unzip('inst/extdata/helminths.zip', files = NULL, 
        list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = outdir, unzip = "internal",
        setTimes = FALSE)
  }
  helminth <- read.csv('helminths.csv')
  return(helminth)
}