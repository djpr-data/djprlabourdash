# Monthly update procedure

library(tidyverse)
library(lubridate)
library(readabs)
library(glue)
shell("git checkout main")



# 1 *MANUALLY* define survey reference period

# tibble::tribble(
#   ~Publication, ~Start.of.Reference.Week, ~End.of.Reference.Week, ~Start.of.Enumeration,  ~End.of.Enumeration,
#       "Nov-21",      "31st October 2021",   "13th November 2021",   "7th November 2021", "27th November 2021",
#       "Dec-21",     "28th November 2021",   "11th December 2021",   "5th December 2021", "23rd December 2021",
#       "Jan-22",       "2nd January 2022",    "15th January 2022",    "9th January 2022",  "29th January 2022",
#       "Feb-22",      "30th January 2022",   "12th February 2022",   "6th February 2022", "26th February 2022",
#       "Mar-22",     "27th February 2022",      "12th March 2022",      "6th March 2022",    "26th March 2022",
#       "Apr-22",         "3rd April 2022",      "16th April 2022",     "10th April 2022",    "30th April 2022"
#   )
ref_start <- "2nd January"
ref_end   <- "15th January"




# 2 Get data
req_series <- c(
  `vic total employed`             = "A84423349V",
  `vic unemployment rate`          = "A84423354L",
  `regional vic unemployment rate` = "A84600079X"
)

recode_series <- names(req_series)
names(recode_series) <- req_series

labour <- read_abs_series(req_series, show_progress_bars = F) %>%
  mutate(
    series = recode(series_id, !!!recode_series),
    value  = ifelse(unit == "000", value * 1000, value),
    ) %>%
  arrange(date) %>%
  group_by(series) %>%
  mutate(
    value = ifelse(
      series == "regional vic unemployment rate",
      (value + lag(value) + lag(value, 2)) / 3,
      value
      )
  ) %>%
  ungroup() %>%
  select(date, series, value)




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






# 5 Make SMS
temp_dir <- tempdir()
temp_txt <- paste(temp_dir, "Jobs brefing SMS.txt", sep = "/")

sms_template_labour <- c(
  "LABOUR FORCE:",
  message_emp_stock,
  message_unemploy_rate,
  "",
  "DETAILED LABOUR FORCE:",
  message_regional_unemploy
)

write_lines(sms_template_labour, temp_txt)
shell(paste("notepad", temp_txt))




# 6 Update data
stop("Make sure djprdashdata has updated")
source("data-raw/internal_data.R")




# 7 Clear cache
unlink("app-cache/*")




# 8 Make briefing
pkgload::load_all(".")
knit_briefing() %>%
  normalizePath() %>%
  shell.exec()




# 9 re-establish cache
app()




# 10 Commit & push
shell("git add .")
shell("git git commit -m \"Manual update\"")
# shell("git push")




# Make email (to be completed)
temp_email <- paste(temp_dir, "Jobs brefing SMS.txt", sep = "/")




