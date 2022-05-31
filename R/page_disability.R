page_disabilityUI <- function(...) {
  fluidRow(

    djpr_h2_box("People with disabilities: jobactive caseload"),
    box(
      width = 12,
      uiOutput("table_jobactive_pwd") %>%
      djpr_with_spinner()
      ),
    fluidRow(
    djpr_async_ui(
      width = 6,
      id = "gr_pwd_jobact_sincecovid_line",
      date_slider("gr_pwd_jobact_sincecovid_line", table_no = "jobactive")
      ),

    djpr_async_ui(width = 6, "gr_pwd_jobactive_bar"),

    htmlOutput("disability_footnote"),
  ))
}


page_disability <- function(input, output, session, plt_change, series_latestdates, footnote) {
  output$table_jobactive_pwd <- renderUI({
    table_jobactive_pwd() %>%
      flextable::htmltools_value()
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
    )
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31"))
  )

  djpr_async_server(
    id= "gr_pwd_jobactive_bar",
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
    )
    )
  )
}
