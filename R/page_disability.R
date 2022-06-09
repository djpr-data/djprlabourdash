page_disabilityUI <- function(...) {
  fluidRow(
    column_nopad(
      width = 4,
      djprshiny::djpr_h1_box("People with Disabilities"),
      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "Jobactive caseload."
      )
    ),
    box(
      width = 8,
      uiOutput("table_jobactive_pwd") %>%
        djpr_with_spinner()
    ),
    djpr_async_ui(
      width = 6,
      id = "gr_pwd_jobact_sincecovid_line",
      date_slider("gr_pwd_jobact_sincecovid_line", table_no = "jobactive")
    ),
    djpr_async_ui(
        width = 6,
        id = "gr_pwd_jobactive_bar"
    ),

    height_sync("gr_pwd_jobact_sincecovid_line", "gr_pwd_jobactive_bar"),


    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        "This dashboard is produced by the <b>Strategy and Priority ",
        "Projects - Data + Analytics</b> team at the Victorian Department ",
        "of Jobs, Precincts and Regions. The <b>latest data in this ",
        "dashboard is for ", format(data_dates$jobactive$max, "%B %Y"),
        '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'
      ),
      shiny::div(style = 'position:right;bottom:20px',
                 shiny::actionLink('fromdisability_tolegal',
                                   "Disclaimer and Copyright",
                                   style = 'material-flat',
                                   color = 'success')
      )
    )
  )
}


page_disability <- function(input, output, session) {
  output$table_jobactive_pwd <- renderUI({
    table_jobactive_pwd() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  })

  djpr_async_server(
    id = "gr_pwd_jobact_sincecovid_line",
    plot_fun = viz_gr_pwd_jobact_sincecovid_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
        "jobactive_pwd_ballarat",
        "jobactive_pwd_bendigo",
        "jobactive_pwd_barwon",
        "jobactive_pwd_gippsland",
        "jobactive_pwd_goulburn/murray",
        "jobactive_pwd_inner metropolitan melbourne",
        "jobactive_pwd_north eastern melbourne",
        "jobactive_pwd_north western melbourne",
        "jobactive_pwd_south coast of victoria",
        "jobactive_pwd_south eastern melbourne and peninsula",
        "jobactive_pwd_western melbourne",
        "jobactive_pwd_wimmera mallee",
        "jobactive_total_ballarat",
        "jobactive_total_bendigo",
        "jobactive_total_barwon",
        "jobactive_total_gippsland",
        "jobactive_total_goulburn/murray",
        "jobactive_total_inner metropolitan melbourne",
        "jobactive_total_north eastern melbourne",
        "jobactive_total_north western melbourne",
        "jobactive_total_south coast of victoria",
        "jobactive_total_south eastern melbourne and peninsula",
        "jobactive_total_western melbourne",
        "jobactive_total_wimmera mallee"
      )) %>%
      dplyr::filter(date >= as.Date("2019-03-31"))
  )

  djpr_async_server(
    id = "gr_pwd_jobactive_bar",
    plot_fun = viz_gr_pwd_jobactive_bar,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
        "jobactive_pwd_ballarat",
        "jobactive_pwd_bendigo",
        "jobactive_pwd_barwon",
        "jobactive_pwd_gippsland",
        "jobactive_pwd_goulburn/murray",
        "jobactive_pwd_inner metropolitan melbourne",
        "jobactive_pwd_north eastern melbourne",
        "jobactive_pwd_north western melbourne",
        "jobactive_pwd_south coast of victoria",
        "jobactive_pwd_south eastern melbourne and peninsula",
        "jobactive_pwd_western melbourne",
        "jobactive_pwd_wimmera mallee"
      ))
  )

  observeEvent(input$fromdisability_tolegal, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })
}
