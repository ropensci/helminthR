context("findLocation")

test_that("findLocation finds hosts and parasite by location", {
  skip_on_cran()
	expect_error(findLocation("utopia"))
	France <- findLocation("France")
	expect_gt(nrow(France), 2)
})
