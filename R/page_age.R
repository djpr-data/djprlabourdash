
page_ageUI <- function(...) {
  shiny::fluidRow(
    br(),
    h2(br(), "Overview"),
    "Labour force data disaggregated by age can be volatile, and most of this data",
    " is not seasonally adjusted by the ABS. DJPR smooths the data ",
    "by using 12-month rolling averages. While this assists in removing noise",
    " to focus on the underlying trends, it makes large month-to-month changes ",
    "in underlying conditions less apparent.",
    uiOutput("table_gr_youth_summary"),
    fluidRow(
        djpr_async_ui("gr_yth_emp_sincecovid_line", width = 6),
        djpr_async_ui("gr_yth_lfpartrate_vicaus_line", width = 6)
    ),


    focus_box(
      h2("Labour force status of Victorian youth"),
      shiny::selectInput("youth_focus",
        "Select an indicator",
        choices = c(
          "Unemployment rate" = "unemp_rate",
          "Participation rate" = "part_rate",
          "Employment-to-population ratio" = "emp_pop"
        ),
        width = "100%"
      ),
      fluidRow(
        column(6,
               djpr_async_ui("gr_youth_states_dot",
                             height = "640px",
                             width = 12
               )),
        column(6,
               fluidRow(
                 djpr_async_ui("gr_ages_line",
                                height = "200px",
                                width = 12
               )),
               fluidRow(
                 djpr_async_ui("gr_yth_melbvrest_line",
                               height = "200px",
                               width = 12
                 )
               ))),
      fluidRow(
        djpr_async_ui(
          "gr_youth_vicaus_line",
          width = 12)
      )
    ),


    djpr_h2_box("Detailed labour force status of Victorian youth"),
    djpr_async_ui("gr_youth_full_part_line"),
    djpr_async_ui("gr_youth_eduemp_waterfall",
      interactive = FALSE
    ),
    djpr_async_ui("gr_yth_mostvuln_line"),


    djpr_h2_box("Youth unemployment rate by region"),
    uiOutput("table_gr_youth_unemp_region") %>% djpr_with_spinner(),



    focus_box(
      selectInput("youth_region_focus",
        "Select an indicator",
        choices = c(
          "Unemployment rate" = "unemp_rate",
          "Participation rate" = "part_rate",
          "Employment-to-population ratio" = "emp_pop"
        ),
        width = "100%"
      ),
      uiOutput("title_youth_unemp_emppop_partrate_vic"),
      column(
        6,
        leaflet::leafletOutput("map_youth_unemp_emppop_partrate_vic") %>% djpr_with_spinner()
      ),
      column(
        6,
        plotOutput("gr_youth_unemp_emppop_partrate_bar")
      )
    ),



    djpr_h2_box("Jobactive caseload by age"),

    uiOutput("table_jobactive_youth") %>% djpr_with_spinner(),

    djpr_async_ui("gr_youth_jobactive_bar"),

    djpr_async_ui("gr_age_jobactive_since_covid_line"),

    uiOutput("table_jobactive_mature_age") %>% djpr_with_spinner(),

    djpr_async_ui("gr_mature_age_jobactive_bar")

  )
}

page_age <- function(input, output, session, plt_change, series_latestdates, footnote) {
  output$table_gr_youth_summary <- renderUI({
    table_gr_youth_summary() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Line chart indexed to COVID: employment by age
  djpr_async_server(
    id       = "gr_yth_emp_sincecovid_line",
    plot_fun = viz_gr_yth_emp_sincecovid_line,
    data     = dash_data %>%
      dplyr::filter(series_id %in% c(
      "15-24_greater melbourne_employed",
      "25-54_greater melbourne_employed",
      "55+_greater melbourne_employed",
      "15-24_rest of vic._employed",
      "25-54_rest of vic._employed",
      "55+_rest of vic._employed"
    )) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value,
                                               before = 11,
                                               complete = TRUE)) %>%
      dplyr::filter(.data$date >= as.Date("2020-01-01"))
  )

  djpr_async_server(
    id         =  "gr_yth_lfpartrate_vicaus_line",
    plot_fun   =  viz_gr_yth_lfpartrate_vicaus_line,
    data       = dash_data %>%
      dplyr::filter(series_id %in%
                      c("A84424622R",
                        "A84424692W")) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value,
        before = 11, complete = TRUE
      ))
  )

  # Age: youth focus box -----

  djpr_async_server(
    id       = "gr_youth_states_dot",
    plot_fun = viz_gr_youth_states_dot,
    data     = dash_data %>%
      filter(series_id %in%
                c("A84433601W",
                  "A84433602X",
                  "A84433603A",
                  "A84433505W",
                  "A84433503T",
                  "A84433504V",
                  "A84433519K",
                  "A84433517F",
                  "A84433518J",
                  "A84433533F",
                  "A84433531A",
                  "A84433532C",
                  "A84433617R",
                  "A84433615K",
                  "A84433616L",
                  "A84433575C",
                  "A84433573X",
                  "A84433574A",
                  "A84433547V",
                  "A84433545R",
                  "A84433546T",
                  "A84433589T",
                  "A84433587L",
                  "A84433588R",
                  "A84433561R",
                  "A84433559C",
                  "A84433560L"
                )),
    # width_percent = 90,
    # height_percent = 160,
    download_button = TRUE,
    selected_indicator = reactive({
      input$youth_focus
    })
  )

  djpr_async_server(
    id        =  "gr_ages_line",
    plot_fun  =  viz_gr_ages_line,
    data      =  youth_focus_box_data(),
    # width_percent = 90,
    # height_percent = 50,
    date_slider = TRUE,
    date_slider_value_min = as.Date("2014-11-01"),
    download_button = TRUE,
    selected_indicator = reactive({
      input$youth_focus
    })
  )

  djpr_async_server(
    id        =  "gr_yth_melbvrest_line",
    plot_fun  =  viz_gr_yth_melbvrest_line,
    data      = dash_data %>%
      filter(series_id %in%
                c("15-24_greater melbourne_employed",
                  "15-24_rest of vic._employed",
                  "15-24_greater melbourne_nilf",
                  "15-24_rest of vic._nilf",
                  "15-24_greater melbourne_unemployed",
                  "15-24_rest of vic._unemployed"
                )),
    # width_percent = 90,
    # height_percent = 50,
    date_slider = TRUE,
    date_slider_value_min = as.Date("2014-11-01"),
    download_button = T,
    selected_indicator = reactive({
      input$youth_focus
    })
  )

  djpr_async_server("gr_youth_vicaus_line",
    viz_gr_youth_vicaus_line,
    data = filter_dash_data(c(
      "A84433601W",
      "A84433602X",
      "A84433603A",
      "A84433505W",
      "A84433503T",
      "A84433504V",
      "A84433519K",
      "A84433517F",
      "A84433518J",
      "A84433533F",
      "A84433531A",
      "A84433532C",
      "A84433617R",
      "A84433615K",
      "A84433616L",
      "A84433575C",
      "A84433573X",
      "A84433574A",
      "A84433547V",
      "A84433545R",
      "A84433546T",
      "A84433589T",
      "A84433587L",
      "A84433588R",
      "A84433561R",
      "A84433559C",
      "A84433560L"
    ),
    df = dash_data
    ) %>%
      dplyr::mutate(
        state = dplyr::if_else(.data$state == "",
          "Aus",
          .data$state
        ),
        state = strayr::clean_state(.data$state)
      ),
    check_box_options = c(
      "Aus",
      "NSW",
      "Qld",
      "Tas",
      "ACT",
      "WA",
      "NT",
      "Vic",
      "SA"
    ),
    check_box_var = .data$state,
    check_box_selected = c("Aus", "Vic"),
    selected_indicator = reactive(input$youth_focus),
    width_percent = 90,
    date_slider = FALSE,
    plt_change = plt_change
  )

  djpr_plot_server("gr_youth_full_part_line",
    plot_function = viz_gr_youth_full_part_line,
    data = filter_dash_data(c(
      "A84424687C",
      "A84424695C",
      "A84424696F"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    width_percent = 90,
    date_slider = TRUE
  )

  djpr_plot_server("gr_youth_eduemp_waterfall",
    plot_function = viz_gr_youth_eduemp_waterfall,
    data = filter_dash_data(c(
      "A84424598A",
      "A84424778K",
      "A84424597X",
      "A84424777J",
      "A84424600A",
      "A84424780W",
      "A84424694A"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    interactive = FALSE,
    width_percent = 90,
    date_slider = FALSE
  )

  djpr_plot_server("gr_yth_mostvuln_line",
    plot_function = viz_gr_yth_mostvuln_line,
    data = filter_dash_data(
      c(
        "A84424601C",
        "A84424781X"
      ),
      df = dash_data
    ),
    plt_change = plt_change,
    width_percent = 90,
    date_slider_value_min = Sys.Date() - (365.25 * 10),
    date_slider = TRUE
  )

  output$table_gr_youth_unemp_region <- renderUI({
    table_gr_youth_unemp_region() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Youth LF status by region focus box ----
  data_youth_map_bar_title <- reactive({
    data_youth_unemp_emppop_partrate_vic(selected_indicator = input$youth_region_focus)
  })

  output$title_youth_unemp_emppop_partrate_vic <- renderUI({
    title_youth_unemp_emppop_partrate_vic(
      data = data_youth_map_bar_title(),
      selected_indicator = input$youth_region_focus
    ) %>%
      djpr_plot_title()
  })

  output$map_youth_unemp_emppop_partrate_vic <- leaflet::renderLeaflet({
    map_youth_unemp_emppop_partrate_vic(
      data = data_youth_map_bar_title(),
      selected_indicator = input$youth_region_focus
    )
  })

  output$gr_youth_unemp_emppop_partrate_bar <- renderPlot({
    viz_gr_youth_unemp_emppop_partrate_bar(
      data = data_youth_map_bar_title(),
      selected_indicator = input$youth_region_focus
    )
  })


  # jobactive data by age

  output$table_jobactive_youth <- renderUI({
    table_jobactive_youth() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_youth_jobactive_bar",
    viz_gr_youth_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_youth (15-24)_ballarat",
      "jobactive_youth (15-24)_bendigo",
      "jobactive_youth (15-24)_barwon",
      "jobactive_youth (15-24)_gippsland",
      "jobactive_youth (15-24)_goulburn/murray",
      "jobactive_youth (15-24)_inner metropolitan melbourne",
      "jobactive_youth (15-24)_north eastern melbourne",
      "jobactive_youth (15-24)_north western melbourne",
      "jobactive_youth (15-24)_south coast of victoria",
      "jobactive_youth (15-24)_south eastern melbourne and peninsula",
      "jobactive_youth (15-24)_western melbourne",
      "jobactive_youth (15-24)_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  djpr_plot_server("gr_age_jobactive_since_covid_line",
    viz_gr_age_jobactive_since_covid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_youth (15-24)_ballarat",
      "jobactive_youth (15-24)_bendigo",
      "jobactive_youth (15-24)_barwon",
      "jobactive_youth (15-24)_gippsland",
      "jobactive_youth (15-24)_goulburn/murray",
      "jobactive_youth (15-24)_inner metropolitan melbourne",
      "jobactive_youth (15-24)_north eastern melbourne",
      "jobactive_youth (15-24)_north western melbourne",
      "jobactive_youth (15-24)_south coast of victoria",
      "jobactive_youth (15-24)_south eastern melbourne and peninsula",
      "jobactive_youth (15-24)_western melbourne",
      "jobactive_youth (15-24)_wimmera mallee",
      "jobactive_mature age (50+)_ballarat",
      "jobactive_mature age (50+)_bendigo",
      "jobactive_mature age (50+)_barwon",
      "jobactive_mature age (50+)_gippsland",
      "jobactive_mature age (50+)_goulburn/murray",
      "jobactive_mature age (50+)_inner metropolitan melbourne",
      "jobactive_mature age (50+)_north eastern melbourne",
      "jobactive_mature age (50+)_north western melbourne",
      "jobactive_mature age (50+)_south coast of victoria",
      "jobactive_mature age (50+)_south eastern melbourne and peninsula",
      "jobactive_mature age (50+)_western melbourne",
      "jobactive_mature age (50+)_wimmera mallee",
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
    width_percent = 90,
    date_slider = FALSE
  )

  output$table_jobactive_mature_age <- renderUI({
    table_jobactive_mature_age() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_mature_age_jobactive_bar",
    viz_gr_mature_age_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_mature age (50+)_ballarat",
      "jobactive_mature age (50+)_bendigo",
      "jobactive_mature age (50+)_barwon",
      "jobactive_mature age (50+)_gippsland",
      "jobactive_mature age (50+)_goulburn/murray",
      "jobactive_mature age (50+)_inner metropolitan melbourne",
      "jobactive_mature age (50+)_north eastern melbourne",
      "jobactive_mature age (50+)_north western melbourne",
      "jobactive_mature age (50+)_south coast of victoria",
      "jobactive_mature age (50+)_south eastern melbourne and peninsula",
      "jobactive_mature age (50+)_western melbourne",
      "jobactive_mature age (50+)_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  observeEvent(input$link_age, {
    updateNavbarPage(session, "navbarpage", "tab-age")
  })
}
