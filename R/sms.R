

#' Create Text for SMS
#'
#' @param labour data.frame
#'
#' @import glue
#' @import dplyr
#' @import here
#' @return character SMS text
#' @export
sms <- function(labour) {

  # 3 Define summary stats
  latest_dates <- labour %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(date = max(date)) %>%
    dplyr::pull(date, name = series)

  latest_values <- labour %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(value = last(value)) %>%
    dplyr::pull(value, name = series)

  latest_values_lag1 <- labour %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(value = nth(value, -2)) %>%
    dplyr::pull(value, name = series)

  latest_delta <- labour %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(delta = last(value - lag(value))) %>%
    dplyr::pull(delta, name = series)




  # 4 Format numbers / sms messages
  num_format <- function(x) {
    switch(names(x),
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

  message_emp_stock <- dplyr::case_when(
    latest_delta["vic total employed"] > 100 ~ glue::glue(
      message_emp_stock_template,
      this_month = format(latest_dates["vic total employed"], "%B"),
      direction  = "up",
      increment  = num_format(latest_delta["vic total employed"]),
      to         = " to",
      stock      = num_format(latest_values["vic total employed"])
    ),
    latest_delta["vic total employed"] < -100 ~ glue::glue(
      message_emp_stock_template,
      this_month = format(latest_dates["vic total employed"], "%B"),
      direction  = "down",
      increment  = num_format(abs(latest_delta["vic total employed"])),
      to         = " to",
      stock      = num_format(latest_values["vic total employed"])
    ),
    TRUE ~ glue::glue(
      message_emp_stock_template,
      this_month = format(latest_dates["vic total employed"], "%B"),
      direction  = "unchanged",
      increment  = "",
      to         = " at",
      stock      = num_format(latest_values["vic total employed"])
    )
  )

  message_unemploy_rate <- dplyr::case_when(
    latest_delta["vic unemployment rate"] > 0.01 ~ glue::glue(
      messgae_unemploy_template,
      this_month = format(latest_dates["vic unemployment rate"], "%B"),
      last_month = format(latest_dates["vic unemployment rate"] - months(1), "%B"),
      direction  = "up",
      this_rate  = num_format(latest_values["vic unemployment rate"]),
      last_rate  = num_format(latest_values_lag1["vic unemployment rate"])
    ),
    latest_delta["vic unemployment rate"] < -0.01 ~ glue::glue(
      messgae_unemploy_template,
      this_month = format(latest_dates["vic unemployment rate"], "%B"),
      last_month = format(latest_dates["vic unemployment rate"] - months(1), "%B"),
      direction  = "down",
      this_rate  = num_format(latest_values["vic unemployment rate"]),
      last_rate  = num_format(latest_values_lag1["vic unemployment rate"])
    ),
    TRUE ~ glue::glue(
      messgae_unemploy_template,
      this_month = format(latest_dates["vic unemployment rate"], "%B"),
      last_month = format(latest_dates["vic unemployment rate"] - months(1), "%B"),
      direction  = "mostly unchanged",
      this_rate  = num_format(latest_values["vic unemployment rate"]),
      last_rate  = num_format(latest_values_lag1["vic unemployment rate"])
    )
  )

  message_regional_unemploy <- dplyr::case_when(
    latest_delta["regional vic unemployment rate"] > 0.01 ~ glue::glue(
      messgae_regional_unemploy_template,
      this_month = format(latest_dates["regional vic unemployment rate"], "%B"),
      last_month = format(latest_dates["regional vic unemployment rate"] - months(1), "%B"),
      direction  = "up",
      this_rate  = num_format(latest_values["regional vic unemployment rate"]),
      last_rate  = num_format(latest_values_lag1["regional vic unemployment rate"])
    ),
    latest_delta["regional vic unemployment rate"] < -0.01 ~ glue::glue(
      messgae_regional_unemploy_template,
      this_month = format(latest_dates["regional vic unemployment rate"], "%B"),
      last_month = format(latest_dates["regional vic unemployment rate"] - months(1), "%B"),
      direction  = "down",
      this_rate  = num_format(latest_values["regional vic unemployment rate"]),
      last_rate  = num_format(latest_values_lag1["regional vic unemployment rate"])
    ),
    TRUE ~ glue::glue(
      messgae_regional_unemploy_template,
      this_month = format(latest_dates["regional vic unemployment rate"], "%B"),
      last_month = format(latest_dates["regional vic unemployment rate"] - months(1), "%B"),
      direction  = "mostly unchanged",
      this_rate  = num_format(latest_values["regional vic unemployment rate"]),
      last_rate  = num_format(latest_values_lag1["regional vic unemployment rate"])
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

  writeLines(sms_template_labour, here::here("inst/sms.md"))

  sms_template_labour
}
