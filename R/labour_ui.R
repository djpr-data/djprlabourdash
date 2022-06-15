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
          startExpanded = TRUE,
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
      djprshiny::djpr_dash_theme(),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shiny::tags$script("document.title='Vic Jobs Dashboard'"),
      shiny::tags$script("$(document).ready(function(){$('a.legalLink').click(function(){$('.sidebar-menu a[data-value=\"legal\"]').trigger('click');})});"),
      shiny::tags$style(".wrapper {background-color: white !important;}"),
      shiny::tags$style("a:focus-visible {outline: -webkit-focus-ring-color auto 1px;outline-offset: 1px; !important; color:#087d81; !important;}"),
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
