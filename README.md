## helminthR

[![Build Status](https://travis-ci.org/ropensci/helminthR.svg?branch=master)](https://travis-ci.org/ropensci/helminthR)

> Programmatically access the London Natural History Museum's [helminth database](http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/index.html).

See software note in _Ecography_ ([available here](http://onlinelibrary.wiley.com/doi/10.1111/ecog.02131/full))

### Installation

```r
# From GitHub
# install.packages("devtools")
devtools::install_github("rOpenSci/helminthR")
library("helminthR")
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


[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
