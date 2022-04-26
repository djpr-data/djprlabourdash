labour_ui <- function(...) {

  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = shiny::HTML("DJPR Victorian<br/>Jobs Update"),
      titleWidth = "40%"
      ),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = 'tabs',
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
          shinydashboard::menuSubItem("Migrants", tabName = "migration")
          ),
        shinydashboard::menuItem(
          text = "Regions",
          tabName = "regions",
          shinydashboard::menuSubItem("Victorian Regions", tabName = "vicregions"),
          shinydashboard::menuSubItem("Australian Regions", tabName = "ausregions")
          ),
        shinydashboard::menuItem("Industries", tabName = "industries")
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      djprshiny::djpr_dash_theme(),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shinydashboard::tabItems(
        shinydashboard::tabItem("overview",   page_overview()),
        shinydashboard::tabItem("indicators", page_indicators()),
        shinydashboard::tabItem("sex",        page_sex()),
        shinydashboard::tabItem("age",        page_age()),
        shinydashboard::tabItem("ltunemp",    page_ltunemp()),
        shinydashboard::tabItem("aboriginal", page_aboriginal()),
        shinydashboard::tabItem("disability", page_disability()),
        shinydashboard::tabItem("migration",  page_migration()),
        shinydashboard::tabItem("vicregions", page_vicregions()),
        shinydashboard::tabItem("ausregions", page_ausregions()),
        shinydashboard::tabItem("industries", page_industries())
      )
    )

  )
}
