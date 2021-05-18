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
      centred_row(h1("Victorian Jobs Dashboard")),
      centred_row(htmlOutput("overview_text")),
      br(),
      # centred_row(
      #   tagList(
      #     "Find out more on the ",
      #     actionLink("link_indicators", "indicators"),
      #     " page or in the table below."
      #   )
      # ),
      centred_row(h3("Overview")),
      centred_row(
        tagList(
          reactable::reactableOutput("main_table"),
          reactable_caption()
        )
      ),
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
