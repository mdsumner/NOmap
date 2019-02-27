context("test SOmap")

test_that("SOmap returns a SOmap object", {
    p <- SOmap(Trim = -45, Grats = TRUE, fronts = TRUE)
    expect_s3_class(p, "SOmap")
})
