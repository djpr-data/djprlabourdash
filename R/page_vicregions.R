page_vicregionsUI <- function(...) {
  fluidPage(

    # Unemployment by region -----
    djprshiny::djpr_h1_box("Victorian regions") %>% fluidRow(),
    shinydashboard::box(
      width = 12,
      style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
      "Victoria contains a range of diverse regions, both within Greater Melbourne and outside the metropolitan area. Below we explore the regional differences in historical and current labour force status within Victoria. "
    ) %>% fluidRow(),
    djpr_h2_box("Labour force status by region") %>% fluidRow(),
    focus_box(
      title = tagList(
        h3(textOutput("title_reg_unemp_emppop_partrate_vic"), style = "color:#FFFFFF"),
        h4(textOutput("subtitle_reg_unemp_emppop_partrate_vic"), style = "color:#FFFFFF")
      ),
      inputs = tagList(
        br(),
        selectInput(
          inputId = "lf_status_region",
          label = span("Choose an indicator", style = "color:#FFFFFF;"),
          selected = "unemp_rate",
          choices = c(
            "Unemployment rate" = "unemp_rate",
            "Participation rate" = "part_rate",
            "Employment to population ratio" = "emp_pop"
          )
        )
      ),
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
      ) %>% fluidRow(),
      div(
        class = "djpr-caption",
        "Based on Australian Bureau of Statistics data: ABS Labour Force, Detailed (monthly). Note: data is not seasonally adjusted; smoothed using a 3 month rolling average."
      )
    ),
    djpr_box_ui(
      id = "reg_unemp_emppop_partrate_multiline",
      height = "600px",
      width = 12,
      fluidRow(
        style = "padding-top:15px;",
        column(
          6,
          date_slider(
            id = "reg_unemp_emppop_partrate_multiline",
            table_no = "6291016",
            value = c(as.Date("2018-01-01"), data_dates$`6202016`$max)
          )
        ),
        column(
          6,
          selectInput(
            NS("reg_unemp_emppop_partrate_multiline", "indicator"),
            label = "Choose an indicator",
            choices = c(
              "Unemployment rate" = "unemp_rate",
              "Participation rate" = "part_rate",
              "Employment to population ratio" = "emp_pop"
            ),
            selected = "unemp_rate"
          )
        )
      )
    ) %>% fluidRow(),
    djpr_h3_box("Unemployment rate variations across Victoria") %>% fluidRow(),
    box(
      width = 12,
      style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
      "The unemployment rate always varies substantially across Victoria. The amount of ",
      "variation across the regions of Victoria changes over time - the gap between the ",
      "highest and lowest unemployment rate in the state grows and shrinks. ",
      "The graphs below explore the level of that dispersion (i.e. the ",
      "difference between minimum and maximum) of unemployment rates over time",
      "in different regions in Victoria. The breakdown of regions is by ",
      shiny::a("SA4.", href = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/main-structure-and-greater-capital-city-statistical-areas/statistical-area-level-4")
    ) %>% fluidRow(),
    djpr_box_ui(
      id = "reg_unemprate_dispersion",
      width = 12,
      fluidRow(
        column(
          6,
          date_slider(
            id = "reg_unemprate_dispersion",
            table_no = "6291016",
            value = c(as.Date("2014-11-01"), data_dates$`6202016`$max)
          )
        ),
        column(
          6,
          selectInput(
            "reg_unemprate_dispersion-sa4_type_dispersion",
            label = "Choose regions",
            choices = c(
              "All Victorian SA4s" = "all",
              "Metropolitan Melbourne SA4s" = "metropolitan",
              "Rural and regional SA4s" = "regional"
            ),
            selected = "all"
          )
        )
      )
    ) %>% fluidRow(),


    # Regional Vic vs Greater Melb -----
    djpr_h2_box("Regional Victoria and Greater Melbourne") %>% fluidRow(),
    djpr_box_ui(
      id = "reg_melvic_line",
      width = 12,
      date_slider(
        id = "reg_melvic_line",
        table_no = "6291002",
        value = c(as.Date("2014-11-01"), data_dates$`6291002`$max)
      )
    ) %>% fluidRow(),
    djpr_box_ui("reg_emp_regions_sincecovid_line", width = 12) %>% fluidRow(),



    # Victorian regions focus box ------
    djpr_h2_box("Regional Victoria") %>% fluidRow(),
    # Box for regional focus
    focus_box(
      title = h3("Compare regions of Victoria"),
      inputs =
        tagList(
          selectInput(
            "focus_region",
            label = span("Choose a region of Victoria", style = "color:#FFFFFF;"),
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
          )
        ),
      fluidRow(
        column(
          6,
          plotOutput("reg_sa4", height = 280) %>%
            djpr_with_spinner()
        ),
        column(
          6,
          htmlOutput("reg_sa4unemp_cf_broadregion_title", inline = FALSE) %>%
            djpr_with_spinner(proxy.height = "30px"),
          plotOutput("reg_sa4unemp_cf_broadregion", height = 300) %>%
            djpr_with_spinner()
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput("table_region_focus") %>%
            djpr_with_spinner()
        )
      )
    ),
    djpr_h2_box("Victorian jobactive caseload by employment region") %>% fluidRow(),
    focus_box(
      title = h3(uiOutput("title_reg_jobactive_vic")),
      inputs = NULL,
      title_width = 11,
      fluidRow(
        column(
          6,
          leaflet::leafletOutput("map_reg_jobactive_vic") %>%
            djpr_with_spinner()
        ),
        column(
          6,
          plotOutput("reg_jobactive_vic_bar") %>%
            djpr_with_spinner()
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput("table_jobactive_regions") %>%
            djpr_with_spinner()
        )
      )
    ),
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
    ) %>% fluidRow()
  )
}


page_vicregions <- function(input, output, session) {
  djpr_box_server(
    id       = "reg_melvic_line",
    plot_fun = viz_reg_melvic_line,
    dates    = input$dates
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

  djpr_box_server(
    "reg_unemp_emppop_partrate_multiline",
    viz_reg_unemp_emppop_partrate_multiline,
    dates = input$dates,
    selected_indicator = input$indicator,
    n_col = ceiling(req(input$sizing$width) / 200)
  )


  djpr_box_server(
    id = "reg_emp_regions_sincecovid_line",
    plot_fun = viz_reg_emp_regions_sincecovid_line
  )

  djpr_box_server(
    id                 = "reg_unemprate_dispersion",
    plot_fun           = viz_reg_unemprate_dispersion,
    selected_indicator = input$sa4_type_dispersion,
    dates              = input$dates
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
      sa42016
    )

  output$table_region_focus <- renderUI({
    table_region_focus(sa4 = input$focus_region) %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  }) %>%
    bindCache(
      input$focus_region,
      data_dates$`6291016`$max
    )

  reg_sa4unemp_cf_broadregion_withtitle <- reactive({
    viz_reg_sa4unemp_cf_broadregion(sa4 = input$focus_region)
  }) %>%
    bindCache(
      input$focus_region,
      data_dates$`6291002`$max,
      data_dates$`6291016`$max
    )

  output$reg_sa4unemp_cf_broadregion_title <- renderUI({
    djpr_plot_title(extract_labs(reg_sa4unemp_cf_broadregion_withtitle()))
  }) %>%
    bindCache(
      input$focus_region,
      data_dates$`6291002`$max,
      data_dates$`6291016`$max
    )

  output$reg_sa4unemp_cf_broadregion <- renderPlot({
    plot <- reg_sa4unemp_cf_broadregion_withtitle()
    plot$labels$title <- NULL
    plot
  }) %>%
    bindCache(
      input$focus_region,
      data_dates$`6291002`$max,
      data_dates$`6291016`$max
    )

  # Regional JobActive caseload --------
  # data_reg_jobactive_map_bar_title <- data_reg_jobactive_vic()

  output$title_reg_jobactive_vic <- renderUI({
    title_reg_jobactive_vic(data = data_reg_jobactive_vic())
  })

  output$map_reg_jobactive_vic <- leaflet::renderLeaflet({
    map_reg_jobactive_vic(data = data_reg_jobactive_vic())
  })

  output$reg_jobactive_vic_bar <- renderPlot({
    viz_reg_jobactive_vic_bar(data = data_reg_jobactive_vic())
  })

  output$table_jobactive_regions <- renderUI({
    table_jobactive_regions() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  })

  observeEvent(input$link_vicregions, {
    updateNavbarPage(session, "navbarpage", "tab-vicregions")
  })
}
