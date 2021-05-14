page_overview <- function(...) {


  loading_content <- div(
    id = "loading_page",
    br(),
    br(),
    br(),
    br(),
    centred_row(" Loading data, please wait...")
  )

  main_content <- shinyjs::hidden(
    div(
      id = "main_content",
      br(),
      br(),
      br(),
      br(),
      centred_row(h1("Example overview")),
      centred_row("Lorem i psum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
      centred_row(
        reactable::reactableOutput("main_table")
      ),
      br()
    )
  )

  tabPanel(title = "Overview",
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
