test_that("tables produce appropriate outputs", {
  skip_if_offline()
  dash_data <<- load_dash_data()

  expect_s3_class(
    table_overview(),
    "reactable"
  )

  # expect_s3_class(table_overview(dashboard_or_briefing = "briefing"),
  #                 "flextable")
})
