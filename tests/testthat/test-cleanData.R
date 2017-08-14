context("cleanData")

test_that("cleanData gets to species level", {
	mouse <- findHost(genus="Peromyscus", species="leucopus", citation=FALSE)
	tmp <- cleanData(mouse, speciesOnly=TRUE)
	expect_is(tmp, "data.frame")
	expect_gte(nrow(mouse), nrow(tmp))
})
