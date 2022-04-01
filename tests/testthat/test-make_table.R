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

test_that("make_table() returns correct columns for destination", {
  withr::with_envvar(c(R_DJPRLABOURDASH_TABLEDEST = "briefing"), {
    expect_equal(
      as.character(table_ind_employment()$header$dataset[1, , drop = T]),
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
  })
  withr::with_envvar(c(R_DJPRLABOURDASH_TABLEDEST = "dashboard"), {
    expect_equal(
      as.character(table_ind_employment()$header$dataset[1, , drop = T]),
      c(
        "",
        "Recent trend",
        "Current figures",
        "Change in latest period",
        "Change in past year",
        "Change since COVID"
      )
    )
  })
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
  to_june_2020 <- filter_dash_data(series_ids = c(
    "A84423354L",
    "A84423242V",
    "A84423466F"
  )) %>%
    dplyr::filter(.data$date <= as.Date("2020-06-01"))

  table <- make_table(to_june_2020,
    destination = "briefing"
  )

  expect_s3_class(table, "flextable")

  table_df <- table$body$dataset
  expect_s3_class(table_df, "data.frame")
  expect_equal(
    names(table_df),
    c(
      " ",
      "LAST 3 YEARS",
      "SERIES_ID",
      "JUN 2020",
      "SINCE MAY 2020",
      "SINCE JUN 2019",
      "SINCE MAR 2020",
      "SINCE NOV 2014"
    )
  )

  lapply(table_df$`LAST 3 YEARS`,
    expect_s3_class,
    class = "gg"
  )

  table_without_sparklines <- dplyr::select(
    table_df,
    -`LAST 3 YEARS`
  )
  expect_snapshot_output(table_without_sparklines)
})
