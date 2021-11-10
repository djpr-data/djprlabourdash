test_that("labour_ui() returns a Shiny tag list", {
  ui <- labour_ui()
  expect_s3_class(ui, "shiny.tag.list")
  expect_gte(length(ui), 5)
})
