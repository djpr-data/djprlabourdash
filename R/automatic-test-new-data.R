

add_series_row <- function(df, series_id, window = NULL){

  series <- df |>
    select(date, contains(series_id)) |>
    select(1:2)

  unit <- case_when(
    grepl('Percent', colnames(series)[2]) ~ 'percent',
    grepl('000', colnames(series)[2]) ~ 'thousand',
    )

  series <- series |>
    rename_with(.cols = contains(series_id), .fn = ~ 'value')

  if (!is.null(window)){
    series <- series |>
      mutate(value = slider::slide_mean(.data$value, before = .env$window, complete = TRUE))
  }

  last <- series |> slice_nth_date(1)
  last_month <- series |> slice_nth_date(2)
  last_year <- series |> slice_nth_date(13)
  covid <- series |> filter(date == '2020-03-01')
  nov_2014 <- series |> filter(date == '2014-11-01')

  tibble(SERIES_ID = series_id,
         !!toupper(format(max(series$date), '%b %Y')) := last |> pull(-1),
         !!glue('SINCE {toupper(format(last_month$date, "%b %Y"))}') := last$value - last_month$value,
         !!glue('SINCE {toupper(format(last_year$date, "%b %Y"))}') := last$value - last_year$value,
         !!glue('SINCE {toupper(format(covid$date, "%b %Y"))}') := last$value - covid$value,
         !!glue('SINCE {toupper(format(nov_2014$date, "%b %Y"))}') := last$value - nov_2014$value
         ) |>
    mutate(across(-SERIES_ID, ~ case_when(
        unit == 'thousand' & .x < 1e3 ~ djprshiny::round2(.x, 1) * 1e3,
        unit == 'thousand' & .x >= 1e3 & .x < 1e5 ~ djprshiny::round2(.x, 0) * 1e3,
        unit == 'thousand' & .x >= 1e5 ~ signif(.x * 1e3, 4),
        TRUE ~ djprshiny::round2(.x, 1)
      ))
    )

}


slice_nth_date <- function(df, n){
  df |>
    slice_max(order_by = date, n = n) |>
    slice_min(order_by = date)
}





get_test_data <- function(){

  url <- djprdata:::get_latest_download_url(
    'https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release',
    '6202005\\.|6202016\\.|6202019\\.|6202023\\.'
  )

  #6202016 youth
  #6202019 hours worked
  #6202023 under-utilisation


  all_df <- purrr::map(url$url, function(url){


    suppressMessages({

      filename <- djprdata::download_excel(url)

      sheets <- readxl::excel_sheets(filename)
      sheets <- grep('data', sheets, ignore.case = TRUE, value = TRUE)

      sheet_data <- purrr::map(sheets, function(sht){
        df <- readxl::read_excel(filename, sht) |>
          rename(date = `...1`) |>
          mutate(date = as.Date(as.integer(date), origin = "1899-12-30"))
        colnames(df) <- c('date',
                          paste(colnames(df), df[c(1:2,9),], sep = ' ; ')[2:ncol(df)])
        df |>
          filter(!is.na(date)) |>
          mutate(across(-date, ~ as.numeric(.x)))
      })

      sheet_data |> purrr::reduce(full_join, by = "date")

    })

  }) |> purrr::reduce(full_join, by = "date")

  all_df

}

clean_table <- function(df){
  df |>
    select(-1, -2) |>
    mutate(across(-SERIES_ID, ~ stringr::str_replace_all(.x, '%|,|ppts|\\s*\\([^\\)]+\\)', '')),
           across(-SERIES_ID, ~ case_when(grepl('m', .x) ~ as.numeric(gsub('m', '', .x)) * 1e6,
                                          TRUE ~ as.numeric(.x))))

}




df <- get_test_data()

t1_actual <- djprlabourdash::table_overview()$body$dataset |>
  clean_table() |> as_tibble()


youth <- c(#"A84433601W",  # in table_overview() but not exported
           "A84424691V",
           "A84424687C",
           "A84424692W")

regional <- 'A84600079X'

t1_test <- dplyr::bind_rows(
   c("A84423354L",
     "A84423242V",
     "A84423466F",
     "A84423350C",
     "A84423349V",
     "A84423357V",
     #"pt_emp_vic",
     "A84423237A",
     "A84423461V",
     "A84423355R",
     "A84423243W",
     "A84423467J",
     "A84426256L",
     "A85223450L",
     "A85223451R",
     "A84423356T") |>
  purrr::map_dfr(~ add_series_row(df, .x)),
  #purrr::map_dfr(regional, ~ add_series_row(df, .x, 2)),
  purrr::map_dfr(youth, ~ add_series_row(df, .x, 11))
)


aj <- anti_join(t1_test, t1_actual) # get rows with errors
#dplyr::all_equal(t1_test, t1_actual |> filter(SERIES_ID %in% t1_test$SERIES_ID))

# sometimes numeric values aren't equal need to double check
all.equal(aj,
          t1_actual |> filter(SERIES_ID == aj$SERIES_ID)) # id columns










