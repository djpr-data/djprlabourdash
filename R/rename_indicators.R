#' Convenience function to give more descriptive names to specific indicators
#'
#' Intended for use with `make_table()`.
#'
#' @param data A dataframe containing an 'indicator' column and a 'series_id'
#' column.
#' @return A dataframe, with the indicator column modified if any specific
#' series IDs are encountered.
#' @details
#' This function looks for specific values in the `series_id` column; if encountered,
#' the `indicator` column in the row is modified.
#' @keywords internal

rename_indicators <- function(data) {
  data %>%
    dplyr::mutate(
      indicator = dplyr::case_when(
        .data$series_id == "A84423242V" ~ "Male unemployment rate",
        .data$series_id == "A84423466F" ~ "Female unemployment rate",
        .data$series_id == "A84433601W" ~ "Youth unemployment rate",
        .data$series_id == "A84424691V" ~ "Youth unemployment rate",
        .data$series_id == "A84423243W" ~ "Male participation rate",
        .data$series_id == "A84423467J" ~ "Female participation rate",
        .data$series_id == "A84433602X" ~ "Youth participation rate",
        .data$series_id == "A84424692W" ~ "Youth participation rate",
        .data$series_id == "A84600079X" ~ "Regional unemployment rate",
        .data$series_id == "A84424687C" ~ "Youth employment",
        .data$series_id == "A84423238C" ~ "Male unemployed people",
        .data$series_id == "A84423462W" ~ "Female unemployed people",
        .data$series_id == "A84433594K" ~ "Youth employment",
        .data$series_id == "15-24_males_unemployment rate" ~
          "Male youth unemployment rate",
        .data$series_id == "15-24_females_unemployment rate" ~
          "Female youth unemployment rate",
        .data$series_id == "A84433597T" ~
          "Unemployed youth",
        .data$series_id == "A84433476W" ~
          "Participation rate - youth not in full-time study",
        .data$series_id == "A84424602F" ~
          "Participation rate - youth not in full-time study",
        .data$series_id == "A84423349V" ~
          "Employed persons",
        .data$series_id == "A84423237A" ~
          "Male employment",
        .data$series_id == "A84423461V" ~
          "Female employment",
        .data$series_id == "A85223450L" ~
          "Underemployment rate",
        .data$series_id == "A84423354L" ~
          "Unemployment rate",
        .data$series_id == "A84433601W" ~
          "Youth unemployment rate",
        .data$series_id == "A84423350C" ~
          "Unemployed persons",
        TRUE ~ .data$indicator
      )
    )
}
