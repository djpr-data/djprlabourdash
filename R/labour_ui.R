labour_ui <- function(...) {
  navbarPage(
    theme = djpr_shiny_theme(),
    position = "fixed-top",
    title = "DJPR Labour Dashboard",
    overview_page()

  )
}

overview_page <- function(...) {
  navbarPage(title = "Overview",
             br(),
             centred_row(h1("Example overview")),
             centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
             djpr_plot_ui("plot1"),
             djpr_plot_ui("plot2"),
             centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
             br()
  )
}
