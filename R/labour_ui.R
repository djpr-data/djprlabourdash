labour_ui <- function(...) {
  djprshiny::djpr_page(
    title = shiny::HTML("DJPR Jobs<br>Dashboard"),
    page_overview(),
    page_indicators(),
    page_inclusion(),
    page_regions(),
    page_industries()
  )
}
