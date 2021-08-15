
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

data_is_current <- function() {
  # Load a file that tells us when the data on GitHub was last updated
  temp_loc <- tempfile(fileext = ".rds")
  remote_url <- "https://github.com/djpr-data/djprdashdata/blob/main/data-raw/last_updated.rds?raw=true"
  download.file(url = remote_url,
                destfile = temp_loc,
                mode = "wb",
                quiet = TRUE)
  remote_updated <- readRDS(temp_loc) %>%
    as.POSIXct()

  # dash_data_updated is an internal data object that tells us when the
  # data in djprlabourdash was last updated
  if (remote_updated == dash_data_updated) {
    TRUE
  } else {
    FALSE
  }
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
