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
