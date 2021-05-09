labour_ui <- function(...) {
  fluidPage(
    ggiraph_js(),
    navbarPage(
      title = "DJPR Labour Dashboard",
      theme = djpr_shiny_theme(),
      lang = "en",
      position = "fixed-top",
      collapsible = TRUE,
      page_overview(),
      page_indicators(),
      page_groups(),
      page_regions(),
      page_industries()
    )
  )
}

spaced_tab_panel <- function(title,
                             ...) {
  tabPanel(
    title = title,
    br(),
    br(),
    br(),
    br(),
    ...
  )
}
