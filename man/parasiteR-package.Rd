\name{parasiteR}
\docType{package}

\title{Web scrapes parasite occurrence records from the London Natural History Museum's Host-Parasite database, which contains over a quarter of a million helminth records.
}

\description{This package uses `rvest` to scrape parasite occurrence records from the London Natural History Museum's Host-Parasite database. The package currently allows you to query by host species, parasite species, and geographic location. There is no information on prevalence or intensity, but references are provided, and can be programmatically accessed using the `getReferences` function.
}

\details{
\tabular{ll}{
Package: \tab parasiteR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-19\cr
License: \tab  Creative Commons CC0 1.0 Universal (CC-0)\cr
}

}
\author{
Tad Dallas <tdallas@uga.edu>
}



\references{

Gibson, D. I., Bray, R. A., & Harris, E. A. (Compilers) (2005). Host-Parasite Database of the Natural History Museum, London. URL.

}


\examples{

plParasites=findHost(genus='Peromyscus', species='leucopus')
str(plParasites)

evHosts=findParasite(genus='Exorchis', species='oviformis')
str(evHosts)
}
