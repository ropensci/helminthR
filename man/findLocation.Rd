\name{findLocation}
\alias{findLocation}

\title{
Find host-parasite interactions for a given location
}
\description{
Given a location (available from `listLocations()`) this function returns all host-parasite associations in that location. 
}

\usage{
findLocation(location = "")
}


\arguments{
  \item{location}{
Location of host-parasite interaction. 
}
}

\details{
Use the `listLocations()` function for a list of possible locations.
}

\value{
Three column matrix containing host species, and parasite species (shortened name and full name), with each row corresponding to an occurrence of a parasite species on a host species.
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

FrenchHostPars=findLocation(location='France')
dim(FrenchHostPars)
}
