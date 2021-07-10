
load_and_hide <- function(branch = "main") {
  df <- load_dash_data(branch = branch)

  # if (shiny::isRunning()) {
  #   shinyjs::hide("loading_page")
  #   shinyjs::show("main_content")
  # }
  df
}

#' Load data for the DJPR Labour Dashboard
#'
#' @param branch Github branch of `djprdashdata` repo from which to load dash data
#' @export
load_dash_data <- function(branch = "main") {
  crosstabs <- djprdashdata::lfs_lookup %>%
    dplyr::select(-dplyr::one_of(c(
      "cat_no",
      "table_no",
      "series",
      "series_type"
    )))

  lfs <- djprdashdata::download_abs_ts("abs-lfs", branch = branch)

  lfs <- lfs %>%
    dplyr::left_join(crosstabs,
      by = "series_id"
    )

  lfs
}
