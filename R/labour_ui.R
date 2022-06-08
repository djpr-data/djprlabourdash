labour_ui <- function(...) {
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = shiny::HTML("DJPR Victorian<br/>Jobs Update"),
      titleWidth = "40%"
    ),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("Home", tabName = "overview", selected = TRUE),
        shinydashboard::menuItem("Indicators", tabName = "indicators"),
        shinydashboard::menuItem(
          text = "Groups",
          tabName = "groups",
          shinydashboard::menuSubItem("Sex", tabName = "sex"),
          shinydashboard::menuSubItem("Age", tabName = "age"),
          shinydashboard::menuSubItem("Long-term Unemployed", tabName = "ltunemp"),
          shinydashboard::menuSubItem("Aboriginal Victorians", tabName = "aboriginal"),
          shinydashboard::menuSubItem("People with Disabilities", tabName = "disability"),
          shinydashboard::menuSubItem("Refugees", tabName = "migration")
        ),
        shinydashboard::menuItem(
          text = "Regions",
          tabName = "regions",
          shinydashboard::menuSubItem("Victorian Regions", tabName = "vicregions"),
          shinydashboard::menuSubItem("Australian Regions", tabName = "ausregions")
        ),
        shinydashboard::menuItem("Industries", tabName = "industries"),
        shinydashboard::menuItem("Methodology", tabName = "methodology"),
        shinydashboard::menuItem("Legal", tabName = "legal")
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      djprshiny::djpr_dash_theme(),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shiny::tags$script("document.title='Vic Jobs Daashboard'"),
      shiny::tags$head(
        shiny::tags$style(".wrapper {
                          background-color: white !important;
                        }")
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem("overview", page_overviewUI()),
        shinydashboard::tabItem("indicators", page_indicatorsUI()),
        shinydashboard::tabItem("sex", page_sexUI()),
        shinydashboard::tabItem("age", page_ageUI()),
        shinydashboard::tabItem("ltunemp", page_ltunempUI()),
        shinydashboard::tabItem("aboriginal", page_aboriginalUI()),
        shinydashboard::tabItem("disability", page_disabilityUI()),
        shinydashboard::tabItem("migration", page_migrationUI()),
        shinydashboard::tabItem("vicregions", page_vicregionsUI()),
        shinydashboard::tabItem("ausregions", page_ausregionsUI()),
        shinydashboard::tabItem("industries", page_industriesUI()),
        shinydashboard::tabItem("methodology", page_methodologyUI()),
        shinydashboard::tabItem("legal", page_legalUI())
      )
    )
  )
}
