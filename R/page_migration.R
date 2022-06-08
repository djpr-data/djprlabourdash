page_migrationUI <- function(...) {

  fluidRow(

    column_nopad(
      width = 4,

      djprshiny::djpr_h1_box("Refugees"),

      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "Jobactive caseload."
      )
    ),

    box(
      width = 8,
      uiOutput("table_jobactive_refugees") %>%
        djpr_with_spinner()
    ),

    djpr_async_ui("gr_refugee_jobact_sincecovid_line", width = 6),
    djpr_async_ui("gr_refugee_jobactive_bar", width = 6),
    height_sync(
      "gr_refugee_jobact_sincecovid_line",
      "gr_refugee_jobactive_bar"
    ),

    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        "This dashboard is produced by the <b>Strategy and Priority ",
        "Projects - Data + Analytics</b> team at the Victorian Department ",
        "of Jobs, Precincts and Regions. The <b>latest data in this ",
        "dashboard is for ",  format(data_dates$jobactive$max, "%B %Y"),
        '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'

      )
    )
  )
}


page_migration <- function(input, output, session) {
  output$table_jobactive_refugees <- renderUI({
    table_jobactive_refugees() %>%
      flextable::htmltools_value()
  })

  djpr_async_server(
    id = "gr_refugee_jobact_sincecovid_line",
    plot_fun = viz_gr_refugee_jobact_sincecovid_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "jobactive_refugee_ballarat",
      "jobactive_refugee_bendigo",
      "jobactive_refugee_barwon",
      "jobactive_refugee_gippsland",
      "jobactive_refugee_goulburn/murray",
      "jobactive_refugee_inner metropolitan melbourne",
      "jobactive_refugee_north eastern melbourne",
      "jobactive_refugee_north western melbourne",
      "jobactive_refugee_south coast of victoria",
      "jobactive_refugee_south eastern melbourne and peninsula",
      "jobactive_refugee_western melbourne",
      "jobactive_refugee_wimmera mallee",
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
    )
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31"))
  )

  djpr_async_server(
    id = "gr_refugee_jobactive_bar",
    plot_fun = viz_gr_refugee_jobactive_bar,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "jobactive_refugee_ballarat",
      "jobactive_refugee_bendigo",
      "jobactive_refugee_barwon",
      "jobactive_refugee_gippsland",
      "jobactive_refugee_goulburn/murray",
      "jobactive_refugee_inner metropolitan melbourne",
      "jobactive_refugee_north eastern melbourne",
      "jobactive_refugee_north western melbourne",
      "jobactive_refugee_south coast of victoria",
      "jobactive_refugee_south eastern melbourne and peninsula",
      "jobactive_refugee_western melbourne",
      "jobactive_refugee_wimmera mallee"
    )
    )
#    download_button = FALSE,
  )
}
