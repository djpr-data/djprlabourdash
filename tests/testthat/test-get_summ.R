test_that("get_summ() returns expected output", {
  ts_summ <- dash_data %>%
    dplyr::filter(date <= as.Date("2021-08-01")) %>%
    djprshiny::ts_summarise()


  expect_equal(get_summ("A84601638A", latest_value, df = ts_summ),
               220805)

  expect_equal(get_summ("A84601638A", series, df = ts_summ),
               "> Victoria ;  Accommodation and Food Services ;  Employed total ;")

  expect_equal(get_summ("A84601638A", d_period_abs, df = ts_summ),
               24433)
})
