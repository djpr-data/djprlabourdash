
#' Get Labour Force Reference Dates
#'
#' @import rvest
#' @import httr
#' @import lubridate
#'
#' @return list
#' @export
#'
#' @examples
reference_dates <- function(){

  url = 'https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release#media-releases'

  pub_dates <- rvest::read_html(url) |>
    rvest::html_table() |>
    purrr::keep(~ 'Publication' %in% colnames(.x)) |>
    purrr::flatten_df()

  pub_dates_out <- pub_dates |>
    dplyr::filter(grepl(pattern = lubridate::month(Sys.Date() %m-% months(1),
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
                     glue::glue(' {lubridate::year(Sys.Date())}.'))

  writeLines(preamble, here::here('inst/preamble.md'))

  return(list(dates = pub_dates_out,
              preamble = preamble))

}
