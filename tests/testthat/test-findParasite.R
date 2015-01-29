context("findParasite")

test_that("findParasite finds the parasite", {
	wormy= findParasite(genus="Brachylaima")
	expect_is(wormy, "data.frame")
	expect_more_than(dim(wormy)[1], 1)
	tick=findParasite(genus="Ixodes")
	expect_equal(nrow(tick),0)
})
	
