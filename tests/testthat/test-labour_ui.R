test_that("labour_ui() returns a Shiny tag and Shiny tag lists", {
  ui <- labour_ui()
  expect_s3_class(ui, c("shiny.tag.list", "shiny.tag"))
})
