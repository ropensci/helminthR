helminthR 1.0.11
==============

The package was archived due to a dependency on taxize which is now fixed. The package still depends on taxize, but taxize is back on CRAN and the build checks all pass. 




helminthR 1.0.10
==============

Tiny bug fix making sure to error out gracefully if web resource is unavailable. Mainly affected vignette, but additional code has been added to `findParasite`, `findHost`, and `findLocation` for error handling.


helminthR 1.0.9
==============

Fixing some NOTES and WARNINGS in the vignette creation process for the package. Switched from Travis CI to GitHub Actions for CI on GitHub. 



helminthR 1.0.8
==============

Small fix to the cleanData function to now use taxize to get host species taxonomic data.


helminthR 1.0.7
==============

Removed geocoding functionality previously present in listLocations(), as this now requires an API key. A cached version of the geographic coordinates of locations is provided as package data (`data(locations)`). 

Added extra catch in `cleanDat.R` to remove species who are identified as "something spp." instead of just removing those identified as "something sp.". 




helminthR 1.0.6
==============
* bug fix that was causing null results for some location specifications. 





helminthR 1.0.5
==============

* Released to CRAN.
