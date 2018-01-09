context("findParasite")

test_that("findParasite finds the parasite", {
	wormy <- findParasite(genus="Brachylaima")
	expect_is(wormy, "data.frame")
	expect_gt(dim(wormy)[1], 1)
})
