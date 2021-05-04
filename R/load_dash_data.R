
load_and_hide <- function() {
  df <- load_dash_data()

  shinyjs::hide("loading_page")
  shinyjs::show("main_content")

  df
}


load_dash_data <- function() {
  crosstabs <- djprdashdata::lfs_lookup %>%
    dplyr::select(-dplyr::one_of(c("cat_no",
                                   "table_no",
                                   "series",
                                   "series_type")
                                 )
                  )

  djprdashdata::download_abs_ts("abs-lfs") %>%
    tidyr::unnest(cols = data) %>%
    dplyr::left_join(crosstabs,
                     by = "series_id")
}
