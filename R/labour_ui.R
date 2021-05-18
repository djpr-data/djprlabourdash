labour_ui <- function(...) {
  djprshiny::djpr_page(
    title = "DJPR Jobs Dashboard",
#     tags$script(HTML("var header = $('.navbar > .container-fluid');
# header.append('<div style=\"float:right\">SPP Data + Analytics</div>');
#     console.log(header)")
#     ),
    page_overview(),
    page_indicators(),
    page_groups(),
    page_regions() #,
    # page_industries()
  )
}
