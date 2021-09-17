
text_overview_summary <- function(ts_summ = dash_data %>%
                                    # tidyr::unnest(.data$data) %>%
                                    ts_summarise()
                                  ) {
  emp_growth <- get_summ("A84423349V", "d_period_abs", ts_summ)
  unemp_rate <- round2(get_summ("A84423354L", "latest_value", ts_summ), 1)
  unemp_change <- round2(get_summ("A84423354L", "d_period_abs", ts_summ), 1)
  latest_period <- get_summ("A84423349V", "latest_period", ts_summ)
  prev_period <- format(get_summ("A84423349V", "prev_date", ts_summ), "%B")
  part_rate <- round2(get_summ("A84423355R", "latest_value", ts_summ), 1)
  part_change <- round2(get_summ("A84423355R", "d_period_abs", ts_summ), 1)

  emp_more_fewer <- dplyr::if_else(emp_growth > 0,
    " more ",
    " fewer "
  )

  unemp_rose_fell <- dplyr::if_else(unemp_change > 0,
    " rose ",
    " fell "
  )


  sentence_1 <- paste0(
    "There were ",
    scales::comma(abs(emp_growth)),
    emp_more_fewer,
    "Victorians in work in ",
    latest_period,
    " than in  ",
    prev_period,
    ". "
  )

  x <- if (3 > 2) {
    "foo"
  } else {
    "bar"
  }

  sentence_2 <- dplyr::case_when(
    emp_growth > 0 & unemp_change > 0 ~
    "This employment growth wasn't enough to stop unemployment rising: ",
    emp_growth > 0 & unemp_change < 0 ~
    "This employment growth pushed unemployment down: ",
    emp_growth < 0 & unemp_change > 0 ~
    "This fall in employment pushed up unemployment: ",
    emp_growth < 0 & unemp_change < 0 ~
    "Despite the fall in employment, unemployment also fell: "
  )

  sentence_3 <- paste0(
    "in ", latest_period, " the unemployment rate", unemp_rose_fell,
    "by ", unemp_change, " percentage points to ", unemp_rate, "%."
  )

  paste0(
    sentence_1,
    sentence_2,
    sentence_3
  )
}
