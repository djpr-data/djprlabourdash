
caption_auto <- function(data,
                         notes = NULL) {
  sources <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::group_by(.data$table_no, .data$frequency, .data$date) %>%
    dplyr::summarise() %>%
    dplyr::mutate(
      cat_no = stringr::str_sub(.data$table_no, 1L, 4L),
      release = dplyr::case_when(
        cat_no == "6202" ~ "ABS Labour Force monthly",
        cat_no == "6291" &
          frequency == "Month" ~ "ABS Labour Force detailed monthly",
        cat_no == "6291" &
          frequency != "Month" ~ "ABS Labour Force detailed quarterly",
        TRUE ~ "."
      ),
      pretty_month = format(.data$date, "%B %Y"),
      desc = paste0(
        .data$release,
        " (latest data is from ",
        .data$pretty_month,
        ")"
      )
    ) %>%
    dplyr::pull(.data$desc) %>%
    unique()

  out <- combine_words(sources)

  if (length(sources > 1)) {
    out <- paste0("Sources: ", out, ".")
  } else {
    out <- paste0("Source: ", out, ".")
  }

  if (!is.null(notes)) {
    out <- paste0(out, "\nNotes: ", notes)
  }

  out
}
