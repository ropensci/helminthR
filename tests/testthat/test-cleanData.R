context("cleanData")

test_that("cleanData gets to species level", {
  skip_on_cran()
	mouse <- findHost(genus="Peromyscus", species="leucopus", citation=FALSE)
	tmp <- cleanData(mouse, speciesOnly=TRUE)
	expect_is(tmp, "data.frame")
	expect_gte(nrow(mouse), nrow(tmp))
})


test_that("cleanData returns list when host species are validated", {
  skip_on_cran()
	mouse <- findHost(genus="Peromyscus", species="leucopus", citation=FALSE)
	tmp <- cleanData(mouse[1,] , speciesOnly=FALSE, validateHosts=TRUE)
	expect_is(tmp, "list")
})



