
load_and_hide <- function() {
  df <- load_dash_data()

  shinyjs::hide("loading_page")
  shinyjs::show("main_content")

  df
}

load_dash_data <- function() {
  djprdashdata::download_abs_ts("abs-lfs")
}
