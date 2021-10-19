test_that("knit_briefing knits briefing", {
  temp_loc <- tempfile(fileext = ".docx")
  briefing <- knit_briefing(path_out = temp_loc, quietly = TRUE)
  expect_true(file.exists(briefing))
})
