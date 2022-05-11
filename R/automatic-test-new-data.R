

add_series_row <- function(df, series_id, window = NULL, covid_date = '2020-03-01'){

  message(series_id)

  series <- df |>
    select(date, contains(series_id)) |>
    select(1:2)

  unit <- case_when(
    grepl('Percent', colnames(series)[2]) ~ 'percent',
    grepl('000', colnames(series)[2]) ~ 'thousand',
    )

  series <- series |>
    rename_with(.cols = contains(series_id), .fn = ~ 'value') #|>
    #filter(!is.na(value))

  if (!is.null(window)){
    series <- series |>
      mutate(value = slider::slide_mean(.data$value, before = .env$window, complete = TRUE))
  }

  last <- series |> slice_nth_date(1)
  last_month <- series |> slice_nth_date(2)
  last_year <- series |> slice_nth_date(13)
  covid <- series |> filter(date == covid_date)
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

  url_detailed <- djprdata:::get_latest_download_url(
    'https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release',
    '6291016\\.|6291005\\.|6291002\\.|RQ1\\.|RM1\\.'
  )


  urls <- c(url$url,
            url_detailed$url)

  rm1 <- grep('RM1\\.', urls, ignore.case = TRUE, value = TRUE)
  rq1 <- grep('RQ1\\.', urls, ignore.case = TRUE, value = TRUE)
  #not_normal <- grep('RM1\\.|RQ1\\.', urls, ignore.case = TRUE, value = TRUE)


  #6202016 youth
  #6202019 hours worked
  #6202023 under-utilisation


  all_df <- purrr::map(urls, function(url){


    suppressMessages({

      filename <- djprdata::download_excel(url)

      sheets <- readxl::excel_sheets(filename)
      sheets <- grep('data', sheets, ignore.case = TRUE, value = TRUE)

      sheet_data <- purrr::map(sheets, function(sht){


        if (url == rm1) {

          df <- readxl::read_excel(filename, sht, skip = 3, col_types = c('numeric', rep('guess', 7))) |>
            dplyr::rename(date = 1) |>
            dplyr::mutate(date = as.Date(as.integer(date), origin = "1899-12-30")) |>
            tidyr::pivot_longer(cols = c(starts_with('Employ'),
                                         starts_with('Number'),
                                         starts_with('Unemployed'),
                                         contains('NILF')),
                                names_to = 'data_type') |>
            dplyr::mutate(Age = stringr::str_sub(Age, start = 1, end = stringr::str_locate(Age, ' years')[,1] - 1),
                          `Labour market region (SA4): ASGS (2011)` = stringr::str_to_lower(stringr::str_sub(`Labour market region (SA4): ASGS (2011)`, start = 5)),
                          value = dplyr::case_when(grepl('000',data_type) ~ value * 1000,
                                            TRUE ~ value),
                          data_type = stringr::str_to_lower(stringr::str_remove_all(data_type, " \\('000\\)")),
                          data_type = dplyr::case_when(data_type == "not in the labour force (nilf)" ~ "nilf total",
                                                TRUE ~ data_type)
                          ) |>
            tidyr::separate(col = data_type, sep = " ", into = c('employment_status', 'employment_type')) |>
            dplyr::group_by(date, Age, `Labour market region (SA4): ASGS (2011)`, employment_status) |>
            dplyr::summarise(value = sum(value)) |>
            tidyr::pivot_wider(names_from = employment_status, values_from = value) |>
            dplyr::mutate(unemploymentrate = unemployed / (employed + unemployed)) |>
            tidyr::pivot_longer(cols = c("employed", "unemployed", "nilf", "unemploymentrate"), names_to = "statistic") |>
            tidyr::pivot_wider(names_from = setdiff(everything(), one_of("date",'value')),
                               names_repair = 'minimal',
                               values_from = 'value',
                               names_sep = '_')


        } else if (url == rq1) {

        df <- readxl::read_excel(filename, sht, skip = 3, col_types = c('numeric', rep('guess', 7))) |>
          dplyr::rename(date = 1) |>
          dplyr::mutate(date = as.Date(as.integer(date), origin = "1899-12-30")) |>
          tidyr::pivot_longer(cols = c(starts_with('Employ'), starts_with('Number of hours')),
                              names_to = 'data_type') |>
          dplyr::mutate(`Labour market region (SA4): ASGS (2011)` = stringr::str_to_lower(stringr::str_sub(`Labour market region (SA4): ASGS (2011)`, start = 5)),
                        `Industry division of main job: ANZSIC (2006) Rev.2.0` = stringr::str_to_lower(`Industry division of main job: ANZSIC (2006) Rev.2.0`),
                        value = dplyr::case_when(grepl('000',data_type) ~ value * 1000),
                        data_type = stringr::str_to_lower(stringr::str_remove_all(data_type, " \\('000\\)")),
                        data_type = stringr::str_remove_all(data_type, " \\('000 hours\\)"),
                        data_type = stringr::str_remove_all(data_type, " \\('000 hours\\)"),
                        data_type = stringr::str_replace(data_type, "number of hours actually worked in all jobs", "hours worked"),
                        data_type = stringr::str_replace(data_type, " \\(employed ", " \\(")
                        ) |>
          dplyr::group_by(date, `Labour market region (SA4): ASGS (2011)`, `Industry division of main job: ANZSIC (2006) Rev.2.0`, data_type) |>
          dplyr::summarise(value = sum(value)) |>
          tidyr::pivot_wider(names_from = setdiff(everything(), one_of("date",'value')),
                           names_repair = 'minimal',
                           values_from = 'value',
                           names_sep = '_')

        } else {

          df <- readxl::read_excel(filename, sht)
          df <- df |>
            dplyr::rename(date = `...1`) |>
            dplyr::mutate(date = as.Date(as.integer(date), origin = "1899-12-30"))
          colnames(df) <- c('date',
                            paste(colnames(df), df[c(1:2,9),], sep = ' ; ')[2:ncol(df)])
          df <- df |>
            dplyr::filter(!is.na(date)) |>
            dplyr::mutate(across(-date, ~ as.numeric(.x)))

        }

        df

      })

      sheet_data |> purrr::reduce(dplyr::full_join, by = "date")

    })

  }) |> purrr::reduce(dplyr::full_join, by = "date")

  all_df

}

clean_table <- function(df){
  df |>
    select(-1, -2) |>
    mutate(across(-SERIES_ID, ~ stringr::str_replace_all(.x, '%|,|ppts|\\s*\\([^\\)]+\\)', '')),
           across(-SERIES_ID, ~ case_when(grepl('m', .x) ~ as.numeric(gsub('m', '', .x)) * 1e6,
                                          TRUE ~ as.numeric(.x))))
}


tests <- function(actual, test){

  aj <- anti_join(test, actual) # get rows with errors

  # sometimes numeric values aren't equal need to double check
  if (nrow(aj) > 0) {
    all.equal(aj,
              actual |> filter(SERIES_ID == aj$SERIES_ID)) # id columns
  } else {
    TRUE
  }

}



check_table_overview <- function(df){

  actual <- djprlabourdash::table_overview()$body$dataset |>
    clean_table() |> as_tibble()

  series <- list(
    employment = c("A84423354L",
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
                   "A84423356T"),
    youth =      c(#"A84433601W",  # in table_overview() but not exported
                    "A84424691V",
                    "A84424687C",
                    "A84424692W"),
    regional =     'A84600079X'
  )

  test <- dplyr::bind_rows(
    purrr::map_dfr(series$employment, ~ add_series_row(df, .x)),
    #purrr::map_dfr(series$regional, ~ add_series_row(df, .x, 2)),
    purrr::map_dfr(series$youth, ~ add_series_row(df, .x, 11))
  )

  tests(actual, test)

}



check_table_gr_sex <- function(df){

  actual <- djprlabourdash::table_gr_sex()$body$dataset |>
    clean_table() |> as_tibble()

  test <- purrr::map_dfr(
    c("A84423237A",
      "A84423461V",
      "A84423238C",
      "A84423462W",
      "A84423242V",
      "A84423466F",
      "A84423243W",
      "A84423467J"), ~ add_series_row(df, .x))

  tests(actual, test)

}


check_table_ind_unemp_state <- function(df){

  actual <- djprlabourdash::table_ind_unemp_state()$body$dataset |>
    clean_table() |> as_tibble()

  test <- purrr::map_dfr(c("A84423270C",
                              "A84423354L",
                              "A84423284T",
                              "A84423368A",
                              "A84423326C",
                              "A84423298F",
                              "A84423050A"),
                            ~ add_series_row(df, .x))

  tests(actual, test)

}



check_table_ind_unemp_summary <- function(df){

  actual <- djprlabourdash::table_ind_unemp_summary()$body$dataset |>
    clean_table() |> as_tibble()

  series <- list(
    youth = "A84424691V",
    rest = c(#"A84423050A",
             "A84423354L", # Unemp rate
             "A84423350C", # Unemp total
             "A85223451R", # Underut rate
             "A84423242V", # Male unemp
             "A84423466F") # Female unemp
  )

  test <- bind_rows(
    purrr::map_dfr(series$rest, ~ add_series_row(df, .x)),
    purrr::map_dfr(series$youth, ~ add_series_row(df, .x, 11))
  )

  tests(actual, test)

}




check_table_gr_youth_summary <- function(df){

  actual <- djprlabourdash::table_gr_youth_summary()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c(#"15-24_females_unemployment rate",
              #"15-24_males_unemployment rate"
              "A84424687C",
              "A84424688F",
              "A84424691V",
              "A84424692W",
              "A84424602F")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 11))

  tests(actual, test)

}



check_table_gr_youth_unemp_region <- function(df){

  #needs RM1.xlsx different transformation

  actual <- djprlabourdash::table_gr_youth_unemp_region()$body$dataset |>
    clean_table() |> as_tibble()

  series <-     c(
    "15-24_employed_ballarat",
    "15-24_employed_bendigo",
    "15-24_employed_geelong",
    "15-24_employed_hume",
    "15-24_employed_latrobe - gippsland",
    "15-24_employed_shepparton",
    "15-24_employed_victoria - north west",
    "15-24_employed_warrnambool and south west",
    "15-24_employed_rest of vic.",
    "15-24_unemployed_ballarat",
    "15-24_unemployed_bendigo",
    "15-24_unemployed_geelong",
    "15-24_unemployed_hume",
    "15-24_unemployed_latrobe - gippsland",
    "15-24_unemployed_shepparton",
    "15-24_unemployed_victoria - north west",
    "15-24_unemployed_warrnambool and south west",
    "15-24_unemployed_rest of vic.",
    "15-24_employed_melbourne - inner",
    "15-24_employed_melbourne - inner east",
    "15-24_employed_melbourne - inner south",
    "15-24_employed_melbourne - north east",
    "15-24_employed_melbourne - north west",
    "15-24_employed_melbourne - outer east",
    "15-24_employed_melbourne - south east",
    "15-24_employed_melbourne - west",
    "15-24_employed_mornington peninsula",
    "15-24_employed_greater melbourne",
    "15-24_unemployed_melbourne - inner",
    "15-24_unemployed_melbourne - inner east",
    "15-24_unemployed_melbourne - inner south",
    "15-24_unemployed_melbourne - north east",
    "15-24_unemployed_melbourne - north west",
    "15-24_unemployed_melbourne - outer east",
    "15-24_unemployed_melbourne - south east",
    "15-24_unemployed_melbourne - west",
    "15-24_unemployed_mornington peninsula",
    "15-24_unemployed_greater melbourne"
  )

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 11))

  tests(actual, test)

}



check_table_reg_metro_states_unemprate <- function(df){

  actual <- djprlabourdash::table_reg_metro_states_unemprate()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84599623K",
              "A84600145K",
              "A84600151F",
              "A84600157V",
              "A84600241K",
              "A84599791W")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}




check_table_reg_metro_emp <- function(df){

  actual <- djprlabourdash::table_reg_metro_emp()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84600141A",
              "A84599655C",
              "A84600015L",
              "A84600183X",
              "A84599553R",
              "A84600111L",
              "A84599847W",
              "A84599919W",
              "A84600021J",
              "A84600189L")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}




check_table_reg_metro_unemp <- function(df){

  actual <- djprlabourdash::table_reg_metro_unemp()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84600142C",
              "A84599656F",
              "A84600016R",
              "A84600184A",
              "A84599554T",
              "A84600112R",
              "A84599848X",
              "A84599920F",
              "A84600022K",
              "A84600190W")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}



check_table_reg_metro_unemprate <- function(df){

  actual <- djprlabourdash::table_reg_metro_unemprate()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84600145K",
              "A84599659L",
              "A84600019W",
              "A84600187J",
              "A84599557X",
              "A84600115W",
              "A84599851L",
              "A84599923L",
              "A84600025T",
              "A84600193C")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}




check_table_reg_metro_partrate <- function(df){

  actual <- djprlabourdash::table_reg_metro_partrate()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84600146L",
              "A84599660W",
              "A84600020F",
              "A84600188K",
              "A84599558A",
              "A84600116X",
              "A84599852R",
              "A84599924R",
              "A84600026V",
              "A84600194F")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}




check_table_reg_nonmetro_states_unemprate <- function(df){

  actual <- djprlabourdash::table_reg_nonmetro_states_unemprate()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84599629X",
              "A84600079X",
              "A84599785A",
              "A84599719C",
              "A84600247X",
              "A84599635V")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}



check_table_reg_nonmetro_emp <- function(df){

  actual <- djprlabourdash::table_reg_nonmetro_emp()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84600075R",
              "A84599661X",
              "A84600027W",
              "A84599667L",
              "A84599673J",
              "A84599679W",
              "A84599925T",
              "A84600117A",
              "A84600033T")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}



check_table_reg_nonmetro_unemp <- function(df){

  actual <- djprlabourdash::table_reg_nonmetro_unemp()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84600076T",
              "A84599662A",
              "A84600028X",
              "A84599668R",
              "A84599674K",
              "A84599680F",
              "A84599926V",
              "A84600118C",
              "A84600034V")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}



check_table_reg_nonmetro_unemprate <- function(df){

  actual <- djprlabourdash::table_reg_nonmetro_unemprate()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84595471L",
              "A84599665J",
              "A84600031L",
              "A84599671C",
              "A84599677T",
              "A84599683L",
              "A84599929A",
              "A84600121T",
              "A84600037A")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}



check_table_reg_nonmetro_partrate <- function(df){

  actual <- djprlabourdash::table_reg_nonmetro_partrate()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84599666K",
              "A84600032R",
              "A84599672F",
              "A84599678V",
              "A84599684R",
              "A84599930K",
              "A84600122V",
              "A84600038C",
              "A84600080J")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, 2))

  tests(actual, test)

}



check_table_industries_summary <- function(df){

  actual <- djprlabourdash::table_industries_summary()$body$dataset |>
    clean_table() |> as_tibble()

  series <- c("A84601662A",
              "A84601680F",
              "A84601683L",
              "A84601686V",
              "A84601665J",
              "A84601704L",
              "A84601707V",
              "A84601710J",
              "A84601638A",
              "A84601653X",
              "A84601689A",
              "A84601656F",
              "A84601713R",
              "A84601668R",
              "A84601695W",
              "A84601698C",
              "A84601650T",
              "A84601671C",
              "A84601641R",
              "A84601716W")

  test <- purrr::map_dfr(series, ~ add_series_row(df, .x, covid_date = '2020-02-01'))

  tests(actual, test)

}






run_checks <- function(){

  df <- get_test_data()

  check_table_overview(df) # table 1
  check_table_gr_sex(df) # table 2
  check_table_ind_unemp_state(df) # table 3

  check_table_gr_youth_summary(df) # table 4 (missing 2 series)



  #check_table_gr_youth_unemp_region(df) #RM1 # table 5



  check_table_reg_metro_states_unemprate(df) # table 6
  check_table_reg_metro_emp(df) # table 7
  check_table_reg_metro_unemp(df) # table 8
  check_table_reg_metro_unemprate(df) # table 9
  check_table_reg_metro_partrate(df) # table 10
  check_table_reg_nonmetro_states_unemprate(df) # table 11
  check_table_reg_nonmetro_emp(df) # table 12
  check_table_reg_nonmetro_unemp(df) # table 13
  check_table_reg_nonmetro_unemprate(df) # table 14

  #check_table_reg_nonmetro_partrate(df) # table 15



  check_table_industries_summary(df) # table 16
  #check_table_ind_unemp_summary(df) # not in report



}
















