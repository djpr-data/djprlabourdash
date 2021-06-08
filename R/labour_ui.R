labour_ui <- function(...) {
  djprshiny::djpr_page(
    title = "DJPR Jobs Dashboard",
    page_overview(),
    page_indicators(),
    page_groups(),
    page_regions(),
    page_industries()
  )
}
