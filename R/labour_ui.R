labour_ui <- function(...) {
  fluidPage(
    navbarPage(
      theme = djpr_shiny_theme(),
      lang = "en",
      position = "fixed-top",
      collapsible = TRUE,
      title = "DJPR Labour Dashboard",
      overview_page(),
      indicators_page(),
      states_page(),
      groups_page(),
      regions_page(),
      explore_page()
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

overview_page <- function(...) {
  loading_content <- div(
    id = "loading_page",
    centred_row(" Loading data, please wait...")
  )

  main_content <- shinyjs::hidden(
    div(
      id = "main_content",
      centred_row(h1("Example overview")),
      centred_row("Lorem i psum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
      djpr_plot_ui("plot1"),
      centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
      br()
    )
  )

  spaced_tab_panel(
    title = "Overview",
    shinyjs::useShinyjs(),
    loading_content,
    main_content
  )
}

indicators_page <- function(...) {
  spaced_tab_panel(
    title = "Indicators",
    centred_row("Example indicators")
  )
}

states_page <- function(...) {
  spaced_tab_panel(
    title = "States",
    centred_row("Compare states")
  )
}

groups_page <- function(...) {
  spaced_tab_panel(
    title = "Groups",
    centred_row("Compare groups")
  )
}

regions_page <- function(...) {
  spaced_tab_panel(
    title = "Regions",
    centred_row("Compare regions")
  )
}

explore_page <- function(...) {
  spaced_tab_panel(
    title = "Explore",
    centred_row("Explore the data")
  )
}
