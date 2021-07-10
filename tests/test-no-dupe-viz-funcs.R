test_that("no duplicate viz_ function names", {
  viz_funcs <- ls("package:djprlabourdash", pattern = "viz_")

  expect_equal(length(viz_funcs),
               length(unique(viz_funcs)))
})
