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
