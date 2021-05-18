#' @import djprshiny
#' @import djprdashdata
#' @import shiny

labour_server <- function(input, output, session) {
  # Load data and create persistent objects ----
  dash_data <<- load_and_hide()

  ts_summ <<- dash_data %>%
    tidyr::unnest(cols = data) %>%
    djprshiny::ts_summarise()

  plt_change <- reactive(input$plt_change) %>%
    debounce(50)

  # Overview ------

  output$footnote_text <- renderUI({
    req(dash_data)
    latest <- max(ts_summ$latest_date)
    div(
    htmltools::HTML(
    paste0("This dashboard is produced by the <b>Strategy and Priority ",
           "Projects Data + Analytics</b> team at the Victorian Department ",
           "of Jobs, Precincts and Regions. The <b>latest data in this ",
           "dashboard is for ",
           format(latest, "%B %Y"),
           "</b>. Please contact Matt Cowgill with any comments or feedback."
           )
    ),
    style = "color: #828282; font-size: 0.75rem"
    )
  })

  output$overview_text <- renderUI({
    text_overview_summary(ts_summ)
  })

  output$main_table <- reactable::renderReactable({
    req(dash_data)
    ids <- c(
      "A84423349V",
      "A84423356T",
      "A84423355R",
      "A84423354L",
      "A84423350C"
    )

    df <- filter_dash_data(ids, dash_data)

    overview_table(data = df)
  })

  # Indicators -----

  output$ind_empgrowth_sincecovid_text <- renderUI({
    text_active(
      paste(
        "There were XX million Victorians employed in XX, up from XX million in XX.",
        "Employment grew by XX per cent over the year to XX,",
        "a",
        dplyr::case_when(
          get_summ("A84423349V", ptile_d_year_abs) < 0.33 ~
          "relatively sluggish",
          get_summ("A84423349V", ptile_d_year_abs) > 0.67 ~
          "relatively rapid",
          TRUE ~ "normal"
        ),
        "pace of growth for Victoria compared to historical trends.",
        "Over the past year, employment across Australia grew by XX per cent.",
        "Employment in Victoria is XX per cent",
        dplyr::if_else(sign(get_summ("A84423349V", d_year_perc)) > 0,
          "above",
          "below"
        ),
        "its pre-COVID level."
      ),
      c(
        round2(get_summ("A84423349V", latest_value) / 1000000, 3),
        get_summ("A84423349V", latest_period),
        round2(get_summ("A84423349V", prev_value) / 1000000, 3),
        format(get_summ("A84423349V", prev_date), "%B"),
        round2(get_summ("A84423349V", d_year_perc), 1),
        format(get_summ("A84423349V", latest_date), "%B"),
        round2(get_summ("A84423043C", d_year_perc), 1),
        round2(get_summ("A84423349V", d_year_perc), 1)
      )
    )
  })


  djpr_plot_server(
    id = "ind_emp_sincecovid_line",
    plot_function = viz_ind_emp_sincecovid_line,
    date_slider = FALSE,
    data = filter_dash_data(c("A84423043C", "A84423349V")) %>%
      dplyr::filter(date >= as.Date("2020-01-01")),
    date_slider_value_min = as.Date("2020-01-01"),
    plt_change = plt_change
  )

  # Indicators: dot point text of employment figures
  # output$ind_emp_dotpoints <- renderUI({
  #   dp1 <- text_active(
  #     paste(
  #       "There were XX Victorians employed,",
  #       "of whom XX were in full-time work."
  #     ),
  #     c(
  #       scales::comma(get_summ("A84423349V", latest_value)),
  #       scales::comma(get_summ("A84423357V", latest_value))
  #     )
  #   )
  #
  #   dp2 <- text_active(
  #     paste(
  #       "Employment ",
  #       dplyr::if_else(get_summ("A84423349V", d_period_abs) > 0,
  #         "rose",
  #         "fell"
  #       ),
  #       "by XX people (XX per cent) in the month to XX",
  #       "and by XX people (XX per cent) over the year."
  #     ),
  #     c(
  #       scales::comma(get_summ("A84423349V", d_period_abs)),
  #       get_summ("A84423349V", d_period_perc),
  #       get_summ("A84423349V", latest_period),
  #       scales::comma(get_summ("A84423349V", d_year_abs)),
  #       get_summ("A84423349V", d_period_perc)
  #     )
  #   )
  #
  #   tags$div(
  #     tags$ul(
  #       tags$li(dp1),
  #       tags$li(dp2)
  #     )
  #   )
  # })

  # Indicators: table of employment indicators
  output$ind_emp_table <- reactable::renderReactable({
    indicators_table()
  })

  # Indicators: line chart of annual employment growth in Vic & Aus

  djpr_plot_server("ind_empgro_line",
                   viz_ind_empgro_line,
                   data = filter_dash_data(c(
                     "A84423349V",
                     "A84423043C"
                   )),
                   date_slider_value_min = Sys.Date() - (365 * 5),
                   plt_change = plt_change)

  # Indicators: line chart of emp-pop by sex
  djpr_plot_server("ind_emppopratio_line",
                   viz_ind_emppopratio_line,
                   data = filter_dash_data(c("A84423356T",
                                             "A84423244X",
                                             "A84423468K")
                   ),
                   date_slider_value_min = Sys.Date() - (365.25 * 10),
                   plt_change = plt_change)

  # Indicators: dot plot of unemp rate by state
  djpr_plot_server("ind_unemp_states_dot",
                   viz_ind_unemp_states_dot,
                   data = filter_dash_data(
                     c("A84423354L",
                       "A84423270C",
                       "A84423368A",
                       "A84423340X",
                       "A84423326C",
                       "A84423284T",
                       "A84423312R",
                       "A84423298F",
                       "A84423050A")

                   ),
                   date_slider = FALSE,
                   plt_change = plt_change)

  # Regions ------
  output$reg_unemprate_map <- leaflet::renderLeaflet({
    map_unemprate_vic()
  })

  output$reg_unemprate_bar <- renderPlot({
    df <- filter_dash_data(c(
      "A84600253V",
      "A84599659L",
      "A84600019W",
      "A84600187J",
      "A84599557X",
      "A84600115W",
      "A84599851L",
      "A84599923L",
      "A84600025T",
      "A84600193C",
      "A84599665J",
      "A84600031L",
      "A84599671C",
      "A84599677T",
      "A84599683L",
      "A84599929A",
      "A84600121T",
      "A84600037A"
    )) %>%
      dplyr::group_by(series_id) %>%
      dplyr::mutate(value = zoo::rollmeanr(value, 3, fill = NA)) %>%
      dplyr::filter(.data$date == max(.data$date))

    df %>%
      viz_reg_unemprate_bar()
  })


  djpr_plot_server("reg_unemprate_multiline",
    viz_reg_unemprate_multiline,
    date_slider = TRUE,
    data = filter_dash_data(c(
      "A84600253V",
      "A84599659L",
      "A84600019W",
      "A84600187J",
      "A84599557X",
      "A84600115W",
      "A84599851L",
      "A84599923L",
      "A84600025T",
      "A84600193C",
      "A84599665J",
      "A84600031L",
      "A84599671C",
      "A84599677T",
      "A84599683L",
      "A84599929A",
      "A84600121T",
      "A84600037A"
    )) %>%
      dplyr::group_by(series_id) %>%
      dplyr::mutate(value = zoo::rollmeanr(value, 3, fill = NA)) %>%
      dplyr::filter(!is.na(value)),
    date_slider_value_min = as.Date("2014-11-29"),
    plt_change = plt_change
  )

  output$text_emp_regions <- renderUI({
    text_reg_regions_sincecovid()
  })

  djpr_plot_server("reg_emp_regions_sincecovid_line",
    viz_reg_emp_regions_sincecovid_line,
    date_slider = FALSE,
    data = filter_dash_data(c(
      "A84600141A",
      "A84600075R"
    )) %>%
      dplyr::group_by(series_id) %>%
      dplyr::mutate(value = zoo::rollmeanr(value, 3, fill = NA)) %>%
      dplyr::filter(date >= as.Date("2020-01-01")),
    plt_change = plt_change
  )

  djpr_plot_server("reg_unemprate_dispersion",
    viz_reg_unemprate_dispersion,
    data = filter_dash_data(c(
      "A84600253V",
      "A84599659L",
      "A84600019W",
      "A84600187J",
      "A84599557X",
      "A84600115W",
      "A84599851L",
      "A84599923L",
      "A84600025T",
      "A84600193C",
      "A84599665J",
      "A84600031L",
      "A84599671C",
      "A84599677T",
      "A84599683L",
      "A84599929A",
      "A84600121T",
      "A84600037A"
    ),
    df = dash_data
    ) %>%
      dplyr::group_by(series_id) %>%
      dplyr::mutate(value = zoo::rollmeanr(value, 3, fill = NA)),
    date_slider_value_min = as.Date("2014-01-01"),
    plt_change = plt_change
  )

  output$reg_sa4 <- renderPlot(
    {
      map_reg_sa4(sa4 = input$focus_region)
    },
    height = 350
  )

  output$table_region_focus <- reactable::renderReactable({
    reactable_region_focus(sa4 = input$focus_region)
  })

  reg_sa4unemp_cf_broadregion_withtitle <- reactive({
    viz_reg_sa4unemp_cf_broadregion(sa4 = input$focus_region)
  })

  output$reg_sa4unemp_cf_broadregion_title <- renderUI({
    djpr_plot_title(extract_labs(reg_sa4unemp_cf_broadregion_withtitle()))
  })

  output$reg_sa4unemp_cf_broadregion <- renderPlot({
    plot <- reg_sa4unemp_cf_broadregion_withtitle()
    plot$labels$title <- NULL
    plot
  })

  # Links to pages -----
  observeEvent(input$link_indicators, {
    updateNavbarPage(session, "navbarpage", "tab-indicators")
  })

  observeEvent(input$link_overview, {
    updateNavbarPage(session, "navbarpage", "tab-overview")
  })

  observeEvent(input$link_regions, {
    updateNavbarPage(session, "navbarpage", "tab-regions")
  })

  observeEvent(input$link_groups, {
    updateNavbarPage(session, "navbarpage", "tab-groups")
  })

  observeEvent(input$link_industries, {
    updateNavbarPage(session, "navbarpage", "tab-industries")
  })
}

app <- function(...) {
  shiny::shinyApp(labour_ui(), labour_server)
}
