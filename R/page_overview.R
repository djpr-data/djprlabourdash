page_overview <- function(...) {
  loading_content <- div(
    id = "loading_page",
    br(),
    br(),
    br(),
    br(),
    centred_row(h4(" Loading data, please wait..."))
  )

  main_content <- shinyjs::hidden(
    div(
      id = "main_content",
      br(),
      br(),
      br(),
      centred_row(h1("DJPR Jobs Dashboard")),
      centred_row(htmlOutput("overview_text")),
      br(),
      centred_row(h3("Overview")),
      centred_row(
        tagList(
          reactable::reactableOutput("main_table") %>%
            djpr_with_spinner(),
          reactable_caption("All data seasonally adjusted, other than the youth unemployment which is a 3 month rolling average of unadjusted data.")
        )
      ),
      br(),
      centred_row(htmlOutput("overview_footnote")),
      br()
    )
  )

  tabPanel(
    title = "Overview",
    shinyjs::useShinyjs(),
    loading_content,
    main_content
  )

  # tabPanel(
  #   title = "Overview",
  #   br(),
  #   br(),
  #   br(),
  #   br(),
  #   main_content,
  #   ...
  # )
}
