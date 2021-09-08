test_that("all viz_ functions at least produce a plot", {

  for (f in ls(getNamespace("djprlabourdash"), pattern = "viz_")) {
    testthat::expect_s3_class(get(f)(), "gg")
  }

})
