test_that("filter_dash_data() returns expected object", {
  dash_data <- load_dash_data()

  df <- filter_dash_data("A84423354L", dash_data)

  expect_s3_class(df, "tbl_df")
  expect_gt(nrow(df), 517)
  expect_gt(length(df), 30)
  expect_equal(length(unique(df$series)), 1)
  expect_identical(unique(df$series),
                   "Unemployment rate ;  Persons ;  > Victoria ;")
})
