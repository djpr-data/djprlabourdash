
caption_make <- function(df,
                         series_for_date,
                         release_name) {
  latest_date <- df %>%
    dplyr::filter(.data$series_id == series_for_date) %>%
    # tidyr::unnest(dplyr::everything()) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    pull(.data$date) %>%
    max()

  paste0(
    "Based on Australian Bureau of Statistics data: ",
    release_name,
    ". ",
    "Note: most recent data is for ",
    format(latest_date, "%B %Y"),
    "."
  )
}

caption_lfs <- function(df = dash_data,
                        series_for_date = "A84423043C") {
  caption_make(
    df = df,
    series_for_date = series_for_date,
    release_name = "ABS Labour Force (monthly)"
  )
}

caption_lfs_det_m <- function(df = dash_data,
                              series_for_date = "A84599659L") {
  caption_make(
    df = df,
    series_for_date = series_for_date,
    release_name = "ABS Labour Force, Detailed (monthly)"
  )
}

caption_lfs_det_q <- function(df = dash_data,
                              series_for_date = "A84601680F") {
  caption_make(
    df = df,
    series_for_date = series_for_date,
    release_name = "ABS Labour Force, Detailed (quarterly)"
  )
}

caption_jobactive <- function(df = dash_data,
                              series_for_date = "jobactive_total_ballarat") {

  release_name <- "National Skills Commission jobactive caseload data"

  latest_date <- df %>%
    dplyr::filter(.data$series_id == series_for_date) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    pull(.data$date) %>%
    max()

  paste0(
    release_name,
    ". ",
    "Note: most recent data is for ",
    format(latest_date, "%B %Y"),
    "."
  )
}
