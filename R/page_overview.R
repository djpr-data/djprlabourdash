page_overview <- function(...) {


  # loading_content <- div(
  #   id = "loading_page",
  #   centred_row(" Loading data, please wait...")
  # )

  main_content <- shinyjs::hidden(
    div(
      id = "main_content",
      centred_row(h1("Example overview")),
      centred_row("Lorem i psum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
      centred_row(
        reactable::reactableOutput("main_table")
      ),
      centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
      djpr_plot_ui("plot1"),
      centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
      br()
    )
  )

  # spaced_tab_panel(title = "Overview",
  #                  shinyjs::useShinyjs(),
  #                  loading_content,
  #                  main_content
  #
  # )

  spaced_tab_panel(
    "Overview",
    main_content,
    ...
  )
}
