
test_that("all viz_ functions at least produce a plot", {
  plot_funcs <- ls(getNamespace("djprlabourdash"), pattern = "viz_")

  plots <- lapply(plot_funcs, function(x) get(x)())

  for (p in plots) {
    testthat::expect_s3_class(p, "gg")
  }
})
