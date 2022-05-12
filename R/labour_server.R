
labour_server <- function(input, output, session) {

  plt_change <- reactive(input$plt_change) %>%
    debounce(2)

  # Load data and create persistent objects ----
  Sys.setenv("R_DJPRLABOURDASH_TABLEDEST" = "dashboard")
  dash_data <<- get_dash_data()
  dash_data_updated <<- attr(dash_data, "date_updated")
  if (shiny::isRunning()) {
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
  }

  series_latestdates <- dash_data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$date)

  # Page Footnotes
  footnote <- reactive({
    req(dash_data)
    latest <- max(series_latestdates)
    div(
      shiny::HTML(
        paste0(
          "This dashboard is produced by the <b>Strategy and Priority ",
          "Projects - Data + Analytics</b> team at the Victorian Department ",
          "of Jobs, Precincts and Regions. The <b>latest data in this ",
          "dashboard is for ",
          format(latest, "%B %Y"),
          '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'
        )
      ),
      style = "color: #828282; font-size: 0.75rem"
    )
  })

  output$aboriginal_footnote <- output$vicregions_footnote <- output$disability_footnote <- output$migration_footnote <- output$overview_footnote <- output$indicators_footnote <- output$inclusion_footnote <- output$regions_footnote <- output$industries_footnote <- renderUI({
    footnote()
  })

  page_overview(input, output, session, plt_change)
  page_indicators(input, output, session, plt_change)
  page_sex(input, output, session, plt_change)
  page_age(input, output, session, plt_change)
  page_ltunemp(input, output, session, plt_change)
  page_aboriginal(input, output, session, plt_change)
  page_disability(input, output, session, plt_change)
  page_migration(input, output, session, plt_change)
  page_vicregions(input, output, session, plt_change)
  page_ausregions(input, output, session, plt_change)
  page_industries(input, output, session, plt_change)

}
