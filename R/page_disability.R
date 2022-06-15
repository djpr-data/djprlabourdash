page_disabilityUI <- function(...) {
  fluidRow(
    djprshiny::djpr_h1_box("People with Disabilities"),
    shinydashboard::box(
      width = 12,
      style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
      "This page contains the number of people with a disability assisted through the jobactive program. ",
      "More detail by location of residence is available on this page."
    ),
    box(
      width = 12,
      uiOutput("table_jobactive_pwd") %>%
        djpr_with_spinner()
    ),
    djpr_box_ui(
      width = 6,
      id = "gr_pwd_jobact_sincecovid_line",
      date_slider("gr_pwd_jobact_sincecovid_line", table_no = "jobactive")
    ),
    djpr_box_ui(
        width = 6,
        id = "gr_pwd_jobactive_bar"
    ),

    height_sync("gr_pwd_jobact_sincecovid_line", "gr_pwd_jobactive_bar"),


    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        # "This dashboard is produced by the <b>Strategy and Priority ",
        # "Projects - Data + Analytics</b> team at the Victorian Department ",
        # "of Jobs, Precincts and Regions.",
        "The latest data in this ",
        "dashboard is for ", format(data_dates$`6202012`$max, "%B %Y"), '.',
        "We are committed to making our websites accessible to all users.",
        "We are aware that parts of these dashboards are not fully accessible.",
        "If you require this information in an alternative format or would",
        "like to provide feedback please ",
        "<a href='mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard'>email us</a>.",
        "</br>"
      ),
      div(
        style = "text-align: center;",
        tags$a(
          class = "legalLink",
          href = "#shiny-tab-legal",
          "Copyright | Disclaimer"
        )
      )
    )
  )
}


page_disability <- function(input, output, session) {
  output$table_jobactive_pwd <- renderUI({
    table_jobactive_pwd() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  })

  djpr_box_server(
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

  djpr_box_server(
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
}
