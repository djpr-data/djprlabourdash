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

  lfs <- lfs %>%
    tidyr::unnest(cols = .data$data) %>%
    dplyr::mutate_if(is.factor, as.character)

  lfs
}
