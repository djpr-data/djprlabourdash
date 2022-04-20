test_that("knit_briefing knits briefing", {
  skip_on_ci()

  briefing <- knit_briefing(path = tempdir(), quietly = TRUE, show = FALSE)
  expect_true(file.exists(briefing))
})
