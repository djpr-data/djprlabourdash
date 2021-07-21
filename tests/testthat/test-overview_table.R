test_that("table_overview() returns a reactable htmlwidget", {
  dash_data <<- load_dash_data()

  x <- table_overview()

  expect_s3_class(x, "reactable")
  expect_s3_class(x, "htmlwidget")
})
