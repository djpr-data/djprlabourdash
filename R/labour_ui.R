labour_ui <- function(...) {
  fluidPage(
  #   list(tags$head(tags$style(HTML("
  #     # .navbar {min-height: 40px;},
  #     .navbar.navbar-default {background-color: #000000 !important;}
  #
  # ")))),
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
  ))
}

spaced_tab_panel <- function(title,
                             ...) {
  tabPanel(title = title,
           br(),
           br(),
           br(),
           br(),
           ...)
}

overview_page <- function(...) {
  spaced_tab_panel(title = "Overview",
             centred_row(h1("Example overview")),
             centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
             djpr_plot_ui("plot1"),
             djpr_plot_ui("plot2"),
             centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
             br()

  )
}

indicators_page <- function(...) {
  spaced_tab_panel(title = "Indicators",
             centred_row("Example indicators"))
}

states_page <- function(...) {
  spaced_tab_panel(title = "States",
                   centred_row("Compare states"))
}

groups_page <- function(...) {
  spaced_tab_panel(title = "Groups",
                   centred_row("Compare groups"))
}

regions_page <- function(...) {
  spaced_tab_panel(title = "Regions",
                   centred_row("Compare regions"))
}

explore_page <- function(...) {
  spaced_tab_panel(title = "Explore",
                   centred_row("Explore the data"))
}

