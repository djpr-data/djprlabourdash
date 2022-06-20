
page_ausregionsUI <- function(...) {
  fluidPage(

    # Unemployment by Australian Regions -------------
    djprshiny::djpr_h1_box("Australian regions") %>% fluidRow(),
    shinydashboard::box(
      width = 12,
      style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
      "This section explores labour force indicators in regional and metropolitan areas of Australia."
    ) %>% fluidRow(),
    djpr_h2_box("Unemployment rate in Australian regional areas") %>% fluidRow(),
    box(
      width = 12,
      uiOutput("table_reg_nonmetro_states_unemprate") %>%
        djpr_with_spinner()
    ) %>% fluidRow(),
    focus_box(
      title = h3("Compare regional areas of Australian states"),
      inputs = selectInput(
        inputId = "aus_regions_indicator",
        label = span("Select indicator", style = "color:#FFFFFF;"),
        choices = c(
          "Unemployment rate" = "unemp_rate",
          "Participation rate" = "part_rate",
          "Employment-to-population ratio" = "emp_pop"
        ),
        width = "100%",
        selected = "unemp_rate"
      ),
      fluidRow(
        djpr_box_ui("reg_regionstates_dot", height = "600px") %>%
          async_no_background(),
        djpr_box_ui("reg_regionstates_bar", height = "600px") %>%
          async_no_background()
      )
    )  %>% fluidRow(),
    djpr_box_ui(
      "reg_emp_regionstates_sincecovid_line",
      width = 12,
      state_checkbox(
        id = "reg_emp_regionstates_sincecovid_line",
        choices = c(
          "Reg. Vic",
          "Reg. NSW",
          "Reg. QLD",
          "Reg. NT",
          "Reg. WA",
          "Reg. SA",
          "Reg. Tas"
        ),
        selected = c("Reg. Vic", "Reg. NSW")
      )
    ) %>% fluidRow(),
    djpr_h2_box("Australian metropolitan areas") %>% fluidRow(),
    box(
      width = 12,
      title = "Unemployment rates in Australian major cities",
      uiOutput("table_reg_metro_states_unemprate") %>%
        djpr_with_spinner()
    ) %>% fluidRow(),
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


page_ausregions <- function(input, output, session) {
  output$table_reg_nonmetro_states_unemprate <- renderUI({
    table_reg_nonmetro_states_unemprate() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  }) %>%
    bindCache(data_dates$`6291016`$max)

  output$table_reg_metro_states_unemprate <- renderUI({
    table_reg_metro_states_unemprate() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  }) %>%
    bindCache(data_dates$`6291016`$max)

  # Regions: National focus box -----
  djpr_box_server(
    id = "reg_regionstates_dot",
    plot_fun = viz_reg_regionstates_dot,
    input_from_server = list(selected_indicator = reactive(input$aus_regions_indicator))
  )

  djpr_box_server(
    id = "reg_regionstates_bar",
    plot_fun = viz_reg_regionstates_bar,
    input_from_server = list(
      selected_indicator = reactive(input$aus_regions_indicator)
    )
  )

  djpr_box_server(
    "reg_emp_regionstates_sincecovid_line",
    viz_reg_emp_regionstates_sincecovid_line,
    states = input$states
  )
}
