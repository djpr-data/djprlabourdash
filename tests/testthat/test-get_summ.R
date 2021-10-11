test_that("get_summ() returns expected output", {
  ts_summ <- dash_data %>%
    djprshiny::ts_summarise()

  expect_equal(
    get_summ("A84601638A", series, df = ts_summ),
    "> Victoria ;  Accommodation and Food Services ;  Employed total ;"
  )
})
