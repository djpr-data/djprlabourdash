labour_ui <- function(...) {
  shinydashboard::dashboardPage(
    title = "Victorian Labour Force Update",
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("Home", tabName = "overview", selected = TRUE),
        shinydashboard::menuItem("Indicators", tabName = "indicators"),
        shinydashboard::menuItem(
          text = "Groups",
          tabName = "groups",
          startExpanded = TRUE,
          shinydashboard::menuSubItem("Sex", tabName = "sex"),
          shinydashboard::menuSubItem("Age", tabName = "age"),
          shinydashboard::menuSubItem("Long-term unemployed", tabName = "ltunemp"),
          shinydashboard::menuSubItem("Aboriginal Victorians", tabName = "aboriginal"),
          shinydashboard::menuSubItem("People with disability", tabName = "disability"),
          shinydashboard::menuSubItem("Refugees", tabName = "migration")
        ),
        shinydashboard::menuItem(
          text = "Regions",
          tabName = "regions",
          startExpanded = TRUE,
          shinydashboard::menuSubItem("Victorian Regions", tabName = "vicregions"),
          shinydashboard::menuSubItem("Australian Regions", tabName = "ausregions")
        ),
        shinydashboard::menuItem("Industries", tabName = "industries"),
        shinydashboard::menuItem("Methodology", tabName = "methodology"),
        shinydashboard::menuItem("Copyright & Disclaimer", tabName = "legal") %>%
          shiny::tagAppendAttributes(
            .cssSelector = "a",
            style = "font-size: 10px; font-style: italic; display: inline-table; text-align: right; height: 12px;"
          )
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "djprshiny/dashboard.css"),
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "djprshiny/bs5-card2.css"),
        shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
        shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
        shiny::tags$script("$(document).ready(function(){$('a.legalLink').click(function(){$('.sidebar-menu a[data-value=\"legal\"]').trigger('click');})});"),
        shiny::tags$script("$(document).ready(function(){$('body').addClass('fixed');});"),
        shiny::tags$style(
          ".skin-blue .main-header .navbar {background: transparent; height: 50px;} .skin-blue .main-header .logo {display: none;} .skin-blue .main-header .navbar .sidebar-toggle {height: auto;} #sidebarCollapsed {padding-top: 50px;} .content{ margin-top: -50px}"
          )
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
