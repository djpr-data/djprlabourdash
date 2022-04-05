
load_and_hide <- function(branch = "main") {

  # Check if local data is up to date with Github version; if so
  # load local version, if not download from GH

  if (data_is_current()) {
    df <- dash_data
  } else {
    df <- load_dash_data(branch = branch)
  }

  # This switches off the 'loading data' message on the overview page
  if (shiny::isRunning()) {
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
  }

  df
}

# Load a file that tells us when the data on GitHub was last updated
# Avoid hard-coding the remote URL in multiple places
check_remote_updated <- function() {
  readLines(
    "https://github.com/djpr-data/djprdashdata/raw/main/data-raw/last_updated.txt"
  )
}

data_is_current <- function() {
  # dash_data_updated is an internal data object that tells us when the
  # data in djprlabourdash was last updated
  check_remote_updated() == dash_data_updated
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

  lfs <- lfs %>%
    tidyr::unnest(cols = .data$data) %>%
    dplyr::mutate_if(is.factor, as.character)

  lfs
}
