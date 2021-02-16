helminthR
=======

[![Build Status](https://travis-ci.org/ropensci/helminthR.svg?branch=master)](https://travis-ci.org/ropensci/helminthR)
[![Windows Build Status](https://ci.appveyor.com/api/projects/status/rmq9euldm5gy9qup?svg=true)](https://ci.appveyor.com/project/taddallas/helminthr)
[![codecov.io](https://codecov.io/github/ropensci/helminthR/coverage.svg?branch=master)](https://codecov.io/github/ropensci/helminthR?branch=master)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/helminthR)](https://github.com/r-hub/cranlogs.app)


> Programmatically access the London Natural History Museum's [helminth database](https://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/index.html).

See software note in _Ecography_ ([available here](https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.02131))


### Installation

From GitHub


```r
# install.packages("devtools")
devtools::install_github("rOpenSci/helminthR")
library("helminthR")
```

From CRAN


```r
install.packages("helminthR")
```



### Main functions

#### `findHost()`

Given a host genus and (optionally) species and location, this function returns all host-parasite associations of a given host species. The example below determines all parasite records for helminth infections of _Gorilla gorilla_.


```r
gorillaParasites <- findHost('Gorilla', 'gorilla')
head(gorillaParasites)
```

#### `findParasite()`

Given a helminth parasite genus (and optionally species, and location), this function returns a list of host-parasite records for that parasite. In the example below, I query the database for occurrences of the genus _Strongyloides_.


```r
strongHosts <- findParasite(genus='Strongyloides')
str(strongHosts)
```



#### `listLocations()` and `findLocation()`

List all location names (`listLocations()`). These names can be given to the `findLocation()` function, which finds all host-parasite associations that have occurred in the given location. Below, I look at host-parasite associations recorded in France.



```r
FrenchHostPars <- findLocation(location='France')
str(FrenchHostPars)
```




### Contribute!

Feel free to fork it and contribute some functionality.  



## Meta

* Please [report any issues or bugs](https://github.com/ropensci/helminthR/issues).
* License: GPL-3
* Get citation information for `helminthR` in R doing `citation(package = 'helminthR')`
* Please note that this project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/).
By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
