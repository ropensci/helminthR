context("cleanData")

data <- structure(list(ID = 1:10, parasiteScientificName = c("Neoechinorhynchus agilis", 
"Pallisentis ophiocephali", "Corynosoma strumosum", "Corynosoma strumosum", 
"Corynosoma semerme", "Moniliformis moniliformis", "Acanthocephaloides propinquus", 
"Acanthocephaloides propinquus", "Yamagutisentis sp.", "Acanthocephaloides geneticus"
), hostScientificName = c("Liza abu", "Channa punctatus", "Phoca hispida botnica", 
"Phoca hispida saimensis", "Phoca hispida saimensis", "Eutamias NA", 
"Gobius niger", "Zosterisessor ophiocephalus", "Buglossidium luteum", 
"Arnoglossus laterna"), hostStatus = c("In the wild", "Commercial Source eg. Market", 
"In the wild", "In the wild", "In the wild", "In the wild", "In the wild", 
"In the wild", "In the wild", "In the wild"), parasiteStatus = c(NA_character_, 
NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
NA_character_, NA_character_, NA_character_, NA_character_), 
    country = c("Iraq", "India", "Gulf of Bothnia", "Gulf of Bothnia", 
    "Finland", "Nevada", "Western Mediterranean", "Western Mediterranean", 
    "Western Mediterranean", "Western Mediterranean")), row.names = c(NA, 
10L), class = "data.frame")


test_that("cleanData gets to species level", {
  skip_on_cran()
	tmp <- cleanData(data, speciesOnly=TRUE)
	expect_is(tmp, "data.frame")
	expect_gte(nrow(data), nrow(tmp))
})


test_that("cleanData returns list when host species are validated", {
  skip_on_cran()
	tmp <- cleanData(data[1:10,] , speciesOnly=FALSE, validateHosts=TRUE)
	expect_is(tmp, "list")
})



