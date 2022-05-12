page_aboriginalUI <- function(...) {
  djpr_tab_panel(
    title = "Aboriginal Victorians",
    br(),
    paste0("This section explores the labour force status for Aboriginal Victorians."),
    br(),
    h2(br(), "Aboriginal Victorians: jobactive caseload"),
    uiOutput("table_jobactive_aboriginal") %>%
      djpr_with_spinner(),
    br(),
    djpr_plot_ui("gr_abor_jobactive_sincecovid_line"),
    br(),
    djpr_plot_ui("gr_abor_jobactive_bar"),
    br(),
    htmlOutput("aboriginal_footnote"),
    br()
  )
}


page_aboriginal <- function(input, output, session, plt_change) {

  output$table_jobactive_aboriginal <- renderUI({
    table_jobactive_aboriginal() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_abor_jobactive_sincecovid_line",
                   viz_gr_abor_jobactive_sincecovid_line,
                   plt_change = plt_change,
                   data = filter_dash_data(c(
                     "jobactive_indigenous_ballarat",
                     "jobactive_indigenous_bendigo",
                     "jobactive_indigenous_barwon",
                     "jobactive_indigenous_gippsland",
                     "jobactive_indigenous_goulburn/murray",
                     "jobactive_indigenous_inner metropolitan melbourne",
                     "jobactive_indigenous_north eastern melbourne",
                     "jobactive_indigenous_north western melbourne",
                     "jobactive_indigenous_south coast of victoria",
                     "jobactive_indigenous_south eastern melbourne and peninsula",
                     "jobactive_indigenous_western melbourne",
                     "jobactive_indigenous_wimmera mallee",
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
                   ),
                   df = dash_data
                   ) %>%
                     dplyr::filter(date >= as.Date("2019-03-31")),
                   date_slider = FALSE
  )

  djpr_plot_server("gr_abor_jobactive_bar",
                   viz_gr_abor_jobactive_bar,
                   data = filter_dash_data(c(
                     "jobactive_indigenous_ballarat",
                     "jobactive_indigenous_bendigo",
                     "jobactive_indigenous_barwon",
                     "jobactive_indigenous_gippsland",
                     "jobactive_indigenous_goulburn/murray",
                     "jobactive_indigenous_inner metropolitan melbourne",
                     "jobactive_indigenous_north eastern melbourne",
                     "jobactive_indigenous_north western melbourne",
                     "jobactive_indigenous_south coast of victoria",
                     "jobactive_indigenous_south eastern melbourne and peninsula",
                     "jobactive_indigenous_western melbourne",
                     "jobactive_indigenous_wimmera mallee"
                   ),
                   df = dash_data
                   ),
                   plt_change = plt_change,
                   date_slider = FALSE,
                   download_button = FALSE,
                   width_percent = 75
  )

}
