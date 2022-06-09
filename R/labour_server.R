
labour_server <- function(input, output, session) {

  # Load data and create persistent objects ----
  Sys.setenv("R_DJPRLABOURDASH_TABLEDEST" = "dashboard")

  page_overview(input, output, session)
  page_indicators(input, output, session)
  page_sex(input, output, session)
  page_age(input, output, session)
  page_ltunemp(input, output, session)
  page_aboriginal(input, output, session)
  page_disability(input, output, session)
  page_migration(input, output, session)
  page_vicregions(input, output, session)
  page_ausregions(input, output, session)
  page_industries(input, output, session)

  observeEvent(input$fromoverview_tolegal |
                 input$fromindicators_tolegal |
                 input$fromsex_tolegal |
                 input$fromage_tolegal |
                 input$fromltunemp_tolegal |
                 input$fromaboriginal_tolegal |
                 input$fromdisability_tolegal |
                 input$frommigration_tolegal |
                 input$fromvicregions_tolegal |
                 input$fromausregions_tolegal |
                 input$fromindustries_tolegal |
                 input$frommethodology_tolegal,
               {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })
}
