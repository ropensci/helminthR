context("findLocation")

test_that("findLocation finds hosts and parasite by location", {
  skip_on_cran()
	expect_error(findLocation("utopia"))
	France <- findLocation("France")
	expect_more_than(nrow(France), 2)

  speciesFrance <-  findLocation("France", speciesOnly=TRUE, validateHosts = FALSE)
	expect_is(speciesFrance, "data.frame")

})
