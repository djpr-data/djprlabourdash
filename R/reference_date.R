
#' @title Labour Force Reference Dates
#' @description Get Labour Force reference dates and generate email preamble
#'
#' @param detailed logical standard or detailed Labour Force release
#'
#' @importFrom rvest read_html html_elements html_table
#' @importFrom purrr keep flatten_df
#' @importFrom stringr str_replace
#' @import lubridate
#'
#' @return list
#' @export
reference_dates <- function(detailed = FALSE) {
  url <- "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release#media-releases"

  content <- rvest::read_html(url)

  reference_period <- content |>
    rvest::html_elements('.field--name-field-abs-reference-period .field__item') |>
    rvest::html_text()

  pub_dates <- content |>
    rvest::html_table() |>
    purrr::keep(~ "Publication" %in% colnames(.x)) |>
    purrr::flatten_df()

  pub_dates_out <- pub_dates |>
    dplyr::filter(grepl(
      pattern = lubridate::month(Sys.Date() %m-% months(1),
        label = TRUE,
        abbr = TRUE
      ),
      .data$Publication,
      ignore.case = TRUE
    )) |>
    dplyr::mutate(across(everything(), ~ stringr::str_replace(.x, " 2022", "")))

  pub_dates_text <- pub_dates_out |>
    dplyr::mutate(period = paste0(
      `Start of Reference Week`,
      " to ",
      `End of Reference Week`
    )) |>
    dplyr::pull(period)

  preamble <- paste0(
    "The ABS released the latest ",
    ifelse(detailed, 'detailed ', ''),
    "Labour Force figures at 11:30am today. This data was ",
    "collected for the period ",
    pub_dates_text,
    glue::glue(" {lubridate::year(Sys.Date())}.")
  )

  return(list(
    dates = pub_dates_out,
    reference_period = reference_period,
    preamble = preamble
  ))
}
