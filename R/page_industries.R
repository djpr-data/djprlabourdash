page_industries <- function(...) {
  djpr_tab_panel(
    title = "Industries",
    h1("Victoria's industries"),
    br(),
    htmltools::tags$i(
      paste0(
        "Data on employment by industry is released quarterly by the ABS. ",
        "Charts and tables on employment by industry will be added to this page prior to the next quarterly release."
      )
    ),
    br(),
    br(),
    htmlOutput("industries_footnote"),
    br()
  )
}
