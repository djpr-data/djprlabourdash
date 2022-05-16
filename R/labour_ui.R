labour_ui <- function(...) {
  djprshiny::djpr_page(
    title = shiny::HTML("DJPR Jobs<br>Dashboard"),
    htmltools::tags$head(
      htmltools::tags$link(rel = "icon", href = "favicon.ico")
    ),
    page_overviewUI(),
    page_indicatorsUI(),
    navbarMenu(
      "Groups",
      page_sexUI(),
      page_ageUI(),
      page_ltunempUI(),
      page_aboriginalUI(),
      page_disabilityUI(),
      page_migrationUI()
    ),
    navbarMenu(
      "Regions",
      page_vicregionsUI(),
      page_ausregionsUI()
    ),
    page_industriesUI()
  )
}
