
load_and_hide <- function() {
  df <- load_dash_data()

  # shinyjs::hide("loading_page")
  # shinyjs::show("main_content")

  df
}

#' Load data for the DJPR Labour Dashboard
#' @export
load_dash_data <- function() {
  crosstabs <- djprdashdata::lfs_lookup %>%
    dplyr::select(-dplyr::one_of(c(
      "cat_no",
      "table_no",
      "series",
      "series_type"
    )))

  df <- djprdashdata::download_abs_ts("abs-lfs")

  df %>%
    dplyr::left_join(crosstabs,
      by = "series_id"
    )
}
