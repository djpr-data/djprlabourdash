
load_dash_data <- function() {
  df <- djprdashdata::download_abs_ts("abs-lfs")
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
  df
}
