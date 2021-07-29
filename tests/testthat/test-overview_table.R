test_that("table_overview() returns a flextable object", {
  dash_data <<- load_dash_data()

  x <- table_overview()

  expect_s3_class(x, "flextable")
})
