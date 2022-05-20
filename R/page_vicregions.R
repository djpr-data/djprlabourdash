page_vicregionsUI <- function(...) {
  fluidRow(
    h1("Regions of Victoria"),
    # Unemployment by region -----
    br(),
    "Victoria contains a range of diverse regions, both within Greater Melbourne",
    " and outside the metropolitan area.",
    br(),
    "Below we explore the regional differences in historical and current labour force status ",
    "within Victoria.",
    br(),
    h2(br(), "Labour force status by region"),
    selectInput("lf_status_region",
      label = "Choose an indicator",
      choices = c(
        "Unemployment rate" = "unemp_rate",
        "Participation rate" = "part_rate",
        "Employment to population ratio" = "emp_pop"
      ),
      selected = "unemp_rate"
    ),
    djpr_plot_title(textOutput("title_reg_unemp_emppop_partrate_vic")),
    djpr_plot_subtitle(textOutput("subtitle_reg_unemp_emppop_partrate_vic")),
    fluidRow(
      column(
        6,
        leaflet::leafletOutput("map_reg_unemp_emppop_partrate_vic") %>%
          djpr_with_spinner()
      ),
      column(
        6,
        plotOutput("reg_unemp_emppop_partrate_bar") %>%
          djpr_with_spinner()
      )
    ),
    djpr_plot_caption("Source: ABS Labour Force, Detailed (monthly). Note: data is not seasonally adjusted; smoothed using a 3 month rolling average."),
    br(),
    br(),
    selectInput("lf_status_multiline",
      label = "Choose an indicator",
      choices = c(
        "Unemployment rate" = "unemp_rate",
        "Participation rate" = "part_rate",
        "Employment to population ratio" = "emp_pop"
      ),
      selected = "unemp_rate"
    ),
    djpr_plot_ui("reg_unemp_emppop_partrate_multiline",
      height = "500px",
      interactive = FALSE
    ),
    br(),
    br(),
    "The unemployment rate always varies substantially across Victoria. The amount of ",
    "variation across the regions of Victoria changes over time - the gap between the ",
    "highest and lowest unemployment rate in the state grows and shrinks. ",
    "The graphs below explore the level of that dispersion (i.e. the ",
    "difference between minimum and maximum) of unemployment rates over time", "
    in different regions in Victoria. The breakdown of regions is by ",
    shiny::a("SA4.", href = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/main-structure-and-greater-capital-city-statistical-areas/statistical-area-level-4"),
    br(),
    br(),
    selectInput(
      "sa4_type_dispersion",
      label = "Choose regions",
      choices = c(
        "All Victorian SA4s" = "all",
        "Metropolitan Melbourne SA4s" = "metropolitan",
        "Rural and regional SA4s" = "regional"
      ),
      selected = "all"
    ),
    djpr_plot_ui("reg_unemprate_dispersion",
      interactive = FALSE
    ),
    br(),

    # Regional Vic vs Greater Melb -----
    h2(br(), "Regional Victoria and Greater Melbourne"),
    djpr_plot_ui("reg_melvic_line"),
    # htmlOutput("text_emp_regions"),
    br(),
    djpr_plot_ui("reg_emp_regions_sincecovid_line"),

    # Victorian regions focus box ------
    h2(br(), "Regional Victoria"),
    # Box for regional focus
    focus_box(
      h4("Compare regions of Victoria"),
      selectInput("focus_region",
        label = "Choose a region of Victoria",
        selected = "Ballarat",
        width = "100%",
        choices = c(
          "Ballarat",
          "Bendigo",
          "Geelong",
          "Hume",
          "Latrobe - Gippsland",
          "Melbourne - Inner",
          "Melbourne - Inner East",
          "Melbourne - Inner South",
          "Melbourne - North East",
          "Melbourne - North West",
          "Melbourne - Outer East",
          "Melbourne - South East",
          "Melbourne - West",
          "Mornington Peninsula",
          "North West",
          "Shepparton",
          "Warrnambool and South West"
        )
      ),
      column(
        6,
        plotOutput("reg_sa4", height = 280) %>%
          djpr_with_spinner(),
        br(),
        htmlOutput("reg_sa4unemp_cf_broadregion_title", inline = FALSE) %>%
          djpr_with_spinner(),
        plotOutput("reg_sa4unemp_cf_broadregion", height = 300) %>%
          djpr_with_spinner(),
      ),
      column(
        6,
        br(),
        uiOutput("table_region_focus") %>%
          djpr_with_spinner()
      )
    ),
    br(),
    h2(br(), "Victorian jobactive caseload by employment region"),
    uiOutput("title_reg_jobactive_vic"),
    fluidRow(
      column(
        6,
        leaflet::leafletOutput("map_reg_jobactive_vic") %>%
          djpr_with_spinner()
      ),
      column(
        6,
        plotOutput("reg_jobactive_vic_bar")
      )
    ),
    br(),
    br(),
    br(),
    uiOutput("table_jobactive_regions") %>%
      djpr_with_spinner(),
    br(),
    htmlOutput("vicregions_footnote")
  )
}


page_vicregions <- function(input, output, session, plt_change, series_latestdates, footnote) {
  djpr_plot_server("reg_melvic_line",
    viz_reg_melvic_line,
    plt_change = plt_change,
    date_slider_value_min = as.Date("2014-11-01"),
    data = filter_dash_data(c(
      "A84600144J",
      "A84600078W",
      "A84595516F",
      "A84595471L"
    ),
    df = dash_data
    ) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
      dplyr::filter(!is.na(.data$value))
  )


  output$caption_regions_data2 <- output$caption_regions_data1 <- renderUI({
    djpr_plot_caption(paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average."))
  })

  data_reg_map_bar_title <- reactive({
    data_reg_unemp_emppop_partrate_vic(selected_indicator = input$lf_status_region)
  })

  output$title_reg_unemp_emppop_partrate_vic <- renderText({
    title_reg_unemp_emppop_partrate_vic(data_reg_map_bar_title(),
      selected_indicator = input$lf_status_region
    )
  })

  output$subtitle_reg_unemp_emppop_partrate_vic <- renderText({
    indic_long <- dplyr::case_when(
      input$lf_status_region == "unemp_rate" ~
        "Unemployment rate",
      input$lf_status_region == "part_rate" ~
        "Participation rate",
      input$lf_status_region == "emp_pop" ~
        "Employment to population"
    )

    paste0(indic_long, " by region (SA4), per cent")
  })

  output$map_reg_unemp_emppop_partrate_vic <-
    leaflet::renderLeaflet({
      map_reg_unemp_emppop_partrate_vic(
        data = data_reg_map_bar_title(),
        selected_indicator = input$lf_status_region
      )
    })

  output$reg_unemp_emppop_partrate_bar <- renderPlot({
    viz_reg_unemp_emppop_partrate_bar(
      data = data_reg_map_bar_title(),
      selected_indicator = input$lf_status_region
    )
  })

  djpr_plot_server("reg_unemp_emppop_partrate_multiline",
    viz_reg_unemp_emppop_partrate_multiline,
    date_slider = TRUE,
    interactive = FALSE,
    height_percent = 125,
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
      "A84600037A",
      "A84599658K",
      "A84599660W",
      "A84600018V",
      "A84600020F",
      "A84600186F",
      "A84600188K",
      "A84599556W",
      "A84599558A",
      "A84600114V",
      "A84600116X",
      "A84599850K",
      "A84599852R",
      "A84599922K",
      "A84599924R",
      "A84600024R",
      "A84600026V",
      "A84600192A",
      "A84600194F",
      "A84599664F",
      "A84599666K",
      "A84600030K",
      "A84600032R",
      "A84599670A",
      "A84599672F",
      "A84599676R",
      "A84599678V",
      "A84599682K",
      "A84599684R",
      "A84599928X",
      "A84599930K",
      "A84600120R",
      "A84600122V",
      "A84600036X",
      "A84600038C",
      "A84600252T",
      "A84600254W"
    ),
    df = dash_data
    ),
    selected_indicator = reactive(input$lf_status_multiline),
    date_slider_value_min = as.Date("2018-01-01"),
    plt_change = plt_change
  )

  # output$text_emp_regions <- renderUI({
  #   text_reg_regions_sincecovid()
  # })

  djpr_plot_server("reg_emp_regions_sincecovid_line",
    viz_reg_emp_regions_sincecovid_line,
    date_slider = FALSE,
    data = filter_dash_data(c(
      "A84600141A",
      "A84600075R"
    )) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
      dplyr::filter(.data$date >= as.Date("2020-01-01")),
    plt_change = plt_change
  )

  djpr_plot_server("reg_unemprate_dispersion",
    viz_reg_unemprate_dispersion,
    interactive = FALSE,
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
    ),
    date_slider_value_min = as.Date("2014-11-01"),
    plt_change = plt_change,
    selected_indicator = reactive(input$sa4_type_dispersion)
  )

  # Regions: Focus box -----
  output$reg_sa4 <- renderPlot(
    {
      map_reg_sa4(sa4 = input$focus_region)
    },
    height = 350
  ) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  output$table_region_focus <- renderUI({
    table_region_focus(sa4 = input$focus_region) %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  reg_sa4unemp_cf_broadregion_withtitle <- reactive({
    viz_reg_sa4unemp_cf_broadregion(sa4 = input$focus_region)
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  output$reg_sa4unemp_cf_broadregion_title <- renderUI({
    djpr_plot_title(extract_labs(reg_sa4unemp_cf_broadregion_withtitle()))
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  output$reg_sa4unemp_cf_broadregion <- renderPlot({
    plot <- reg_sa4unemp_cf_broadregion_withtitle()
    plot$labels$title <- NULL
    plot
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  # Regional JobActive caseload --------
  # data_reg_jobactive_map_bar_title <- data_reg_jobactive_vic()

  output$title_reg_jobactive_vic <- renderUI({
    title_reg_jobactive_vic(data = data_reg_jobactive_vic()) %>%
      djpr_plot_title()
  })

  output$map_reg_jobactive_vic <- leaflet::renderLeaflet({
    map_reg_jobactive_vic(data = data_reg_jobactive_vic())
  })

  output$reg_jobactive_vic_bar <- renderPlot({
    viz_reg_jobactive_vic_bar(data = data_reg_jobactive_vic())
  })

  output$table_jobactive_regions <- renderUI({
    table_jobactive_regions() %>%
      flextable::htmltools_value()
  })

  observeEvent(input$link_vicregions, {
    updateNavbarPage(session, "navbarpage", "tab-vicregions")
  })
}
