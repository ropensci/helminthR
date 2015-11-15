context("findHost")

test_that("findHost finds the host", {
	mousey <- findHost(genus="Peromyscus", species="leucopus", citation=TRUE)
	expect_is(mousey, "data.frame")
	expect_more_than(nrow(mousey), 1)


	nothing <- findHost(genus = "Nothing")
	expect_equal(nrow(nothing),0)

	wildMousey <- findHost(genus="Peromyscus", species="leucopus", citation=TRUE, hostState=1)
	expect_is(wildMousey, "data.frame")

	taxMousey <-  findHost(genus="Peromyscus", species="leucopus", citation=TRUE,
	                                              hostState=1, speciesOnly=TRUE, validateHosts = TRUE)
	expect_is(taxMousey, "list")

})
