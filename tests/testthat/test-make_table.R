test_that("make_table() makes tables", {
  t_o <- table_overview()
  expect_s3_class(t_o, "flextable")

  t_o_header_row <- as.character(t_o$header$dataset[1, ])
  expect_equal(
    t_o_header_row,
    c(
      "",
      "Recent trend",
      "Current figures",
      "Change in latest period",
      "Change in past year",
      "Change since COVID",
      "Change during govt"
    )
  )

  t_o_final_col <- t_o$body$dataset$`SINCE NOV 2014`
  expect_type(t_o_final_col, "character")
  expect_true(all(nchar(t_o_final_col) >= 1))
})

test_that("make_table() works with data that starts after Nov 2014", {
  recent_table <- make_table(
    data = filter_dash_data(series_ids = c(
      "A84423354L",
      "A84423242V",
      "A84423466F"
    )) %>%
      filter(.data$date >= as.Date("2015-01-01"))
  )

  expect_s3_class(recent_table, "flextable")
  recent_table_last_col <- recent_table$body$dataset$`SINCE NOV 2014`
  expect_true(all(recent_table_last_col == "-"))
})

test_that("make_table()'s output has not changed", {
  skip_on_ci()

  to_june_2020 <- filter_dash_data(series_ids = c(
    "A84423354L",
    "A84423242V",
    "A84423466F"
  )) %>%
    dplyr::filter(.data$date <= as.Date("2020-06-01"))


  save_my_table <- function(df) {
    x <- make_table(df,
      destination = "briefing"
    )
    path <- tempfile(fileext = ".png")
    flextable::save_as_image(x,
      path = path,
      webshot = "webshot2"
    )
    path
  }

  expect_snapshot_file(
    save_my_table(to_june_2020),
    "table_test.png"
  )
})
