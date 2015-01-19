\name{findParasite}
\alias{findParasite}

\title{
Find host-parasite interactions for a given parasite species.
}
\description{
Given a host genus and/or species, this function returns a matrix containing host-parasite interaction data.
}

\usage{
findParasite(genus, species, location = "")
}


\arguments{
  \item{genus}{
Parasite genus
}
  \item{species}{
Parasite species
}
  \item{location}{
Location of host-parasite interaction. 
}
}

\details{
Use the `listLocations()` function for a list of possible locations.
}

\value{
Two column matrix containing host species and parasite species, with each row corresponding to an occurrence of a parasite species on a host species.
}
\references{
Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005). Host-Parasite Database of the Natural History Museum, London. URL.
}
\author{
Tad Dallas
}


\seealso{
 \code{\link{findHost}}
}

\examples{

giardiaHosts=findParasite(genus='Giardia', species='lamblia')
str(giardiaHosts)

}
