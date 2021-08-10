page_overview <- function(...) {
  tabPanel(
    title = "Overview",
    ggiraph_js(),
    HTML(""),
    value = "tab-overview",
    br(),
    br(),
    br(),
    fluidRow(
      column(2),
      column(
        4,
        plotOutput("ur_bar_static", height = "125px")
        # ggiraph::girafeOutput("overview_ur_bar", height = "125px", width = "100%")
      ),
      column(
        4,
        htmlOutput("overview_ur_text",
          class = "float-none"
        )
      ),
      column(2)
    ),
    br(),
    centred_row(
      span("DJPR Jobs Dashboard",
        style = "font-size: 40px; color: #1F1547; font-family: 'Roboto Slab'"
      )
    ),
    br(),
    centred_row(
      uiOutput("main_table", height = "800px") %>%
        djpr_with_spinner(hide.ui = TRUE)
    ),
    br(),
    centred_row(htmlOutput("overview_footnote")),
    br(),
    centred_row(
      HTML("Note: this dashboard is a prototype in active development. The content is not yet complete. <a href='mailto:matt.cowgill@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard'>Feedback is welcome</a>."),
    ),
    br()
  )
}
