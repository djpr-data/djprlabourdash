
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

  observeEvent(input$fromoverview_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromindicators_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromsex_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromage_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromltunemp_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromaboriginal_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromdisability_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$frommigration_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromvicregions_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromausregions_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$fromindustries_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })

  observeEvent(input$frommethodology_tolegal,  {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })
}
