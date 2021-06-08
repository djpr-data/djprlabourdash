labour_ui <- function(...) {
  addResourcePath("www", "inst/www")
  djprshiny::djpr_page(
    title = "DJPR Jobs Dashboard",
    logo = "www/spp_data_logo.png",
    page_overview(),
    page_indicators(),
    page_inclusion(),
    page_regions(),
    page_industries()
  )
}
