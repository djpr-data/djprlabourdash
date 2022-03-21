

sms <- function(labour){

  # 3 Define summary stats
  latest_dates <- labour %>%
    group_by(series) %>%
    summarise(date = max(date)) %>%
    pull(date, name = series)

  latest_values <- labour %>%
    arrange(date) %>%
    group_by(series) %>%
    summarise(value = last(value)) %>%
    pull(value, name = series)

  latest_values_lag1 <- labour %>%
    arrange(date) %>%
    group_by(series) %>%
    summarise(value = nth(value, -2)) %>%
    pull(value, name = series)

  latest_delta <- labour %>%
    arrange(date) %>%
    group_by(series) %>%
    summarise(delta = last(value - lag(value))) %>%
    pull(delta, name = series)




  # 4 Format numbers / sms messages
  num_format <- function(x){
    switch(
      names(x),
      `vic total employed` = x %>%
        `/`(1000) %>%
        round(0) %>%
        format(big.mark = ",") %>%
        paste0("k"),
      `vic unemployment rate` = x %>%
        round(1) %>%
        paste0("%"),
      `regional vic unemployment rate` = x %>%
        round(1) %>%
        paste0("%"),
      stop("Number format undefined")
    )
  }

  message_emp_stock_template <- "ABS Labour Force {this_month} (sa): Vic employment {direction} {increment}{to} {stock}"
  messgae_unemploy_template <- "UN rate {direction} from {last_rate} ({last_month}) to {this_rate} ({this_month})"
  messgae_regional_unemploy_template <- "ABS Regional Labour Force {this_month} (3 month average)\nRegional UN rate {direction} from {last_rate} ({last_month}) to {this_rate} ({this_month})"

  message_emp_stock <- case_when(
    latest_delta["vic total employed"] > 100 ~ glue(
      message_emp_stock_template,
      this_month = format(latest_dates["vic total employed"], "%B"),
      direction  = "up",
      increment  = num_format(latest_delta['vic total employed']),
      to         = " to",
      stock      = num_format(latest_values['vic total employed'])
    ),
    latest_delta["vic total employed"] < -100 ~ glue(
      message_emp_stock_template,
      this_month = format(latest_dates["vic total employed"], "%B"),
      direction  = "down",
      increment  = num_format(abs(latest_delta['vic total employed'])),
      to         = " to",
      stock      = num_format(latest_values['vic total employed'])
    ),
    TRUE ~ glue(
      message_emp_stock_template,
      this_month = format(latest_dates["vic total employed"], "%B"),
      direction  = "unchanged",
      increment  = "",
      to         = " at",
      stock      = num_format(latest_values['vic total employed'])
    )
  )

  message_unemploy_rate <- case_when(
    latest_delta["vic unemployment rate"] > 0.01 ~ glue(
      messgae_unemploy_template,
      this_month = format(latest_dates["vic unemployment rate"], "%B"),
      last_month = format(latest_dates["vic unemployment rate"] - months(1), "%B"),
      direction  = "up",
      this_rate  = num_format(latest_values['vic unemployment rate']),
      last_rate  = num_format(latest_values_lag1['vic unemployment rate'])
    ),
    latest_delta["vic unemployment rate"] < -0.01 ~ glue(
      messgae_unemploy_template,
      this_month = format(latest_dates["vic unemployment rate"], "%B"),
      last_month = format(latest_dates["vic unemployment rate"] - months(1), "%B"),
      direction  = "down",
      this_rate  = num_format(latest_values['vic unemployment rate']),
      last_rate  = num_format(latest_values_lag1['vic unemployment rate'])
    ),
    TRUE ~ glue(
      messgae_unemploy_template,
      this_month = format(latest_dates["vic unemployment rate"], "%B"),
      last_month = format(latest_dates["vic unemployment rate"] - months(1), "%B"),
      direction  = "mostly unchanged",
      this_rate  = num_format(latest_values['vic unemployment rate']),
      last_rate  = num_format(latest_values_lag1['vic unemployment rate'])
    )
  )

  message_regional_unemploy <- case_when(
    latest_delta["regional vic unemployment rate"] > 0.01 ~ glue(
      messgae_regional_unemploy_template,
      this_month = format(latest_dates["regional vic unemployment rate"], "%B"),
      last_month = format(latest_dates["regional vic unemployment rate"] - months(1), "%B"),
      direction  = "up",
      this_rate  = num_format(latest_values['regional vic unemployment rate']),
      last_rate  = num_format(latest_values_lag1['regional vic unemployment rate'])
    ),
    latest_delta["regional vic unemployment rate"] < -0.01 ~ glue(
      messgae_regional_unemploy_template,
      this_month = format(latest_dates["regional vic unemployment rate"], "%B"),
      last_month = format(latest_dates["regional vic unemployment rate"] - months(1), "%B"),
      direction  = "down",
      this_rate  = num_format(latest_values['regional vic unemployment rate']),
      last_rate  = num_format(latest_values_lag1['regional vic unemployment rate'])
    ),
    TRUE ~ glue(
      messgae_regional_unemploy_template,
      this_month = format(latest_dates["regional vic unemployment rate"], "%B"),
      last_month = format(latest_dates["regional vic unemployment rate"] - months(1), "%B"),
      direction  = "mostly unchanged",
      this_rate  = num_format(latest_values['regional vic unemployment rate']),
      last_rate  = num_format(latest_values_lag1['regional vic unemployment rate'])
    )
  )

  sms_template_labour <- c(
    "LABOUR FORCE:",
    message_emp_stock,
    message_unemploy_rate,
    "",
    "DETAILED LABOUR FORCE:",
    message_regional_unemploy
  )

  writeLines(sms_template_labour, here::here('inst/sms.md'))

  sms_template_labour


}




reference_dates <- function(){

  url = 'https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release#media-releases'

  pub_dates <- read_html(url) |>
    html_table() |>
    purrr::keep(~ 'Publication' %in% colnames(.x)) |>
    purrr::flatten_df()

  pub_dates_out <- pub_dates |>
    dplyr::filter(grepl(pattern = lubridate::month(Sys.Date(),
                                                   label = TRUE,
                                                   abbr = FALSE),
                        .data$Publication)) |>
    dplyr::mutate(across(everything(), ~ stringr::str_replace(.x, ' 2022', '')))

  pub_dates_text <- pub_dates_out |>
    dplyr::mutate(period = paste0(`Start of Reference Week`,
                                  ' to ',
                                  `End of Reference Week`)) |>
    dplyr::pull(period)

  preamble <- paste0('The ABS released the latest detailed',
                     ' Labour Force figures at 11:30am today. This data was ',
                     'collected for the period ',
                     pub_dates_text,
                     ' {lubridate::year(Sys.Date())}.')

  return(list(dates = pub_dates_out,
              preamble = preamble))

}
