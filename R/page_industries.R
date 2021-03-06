page_industriesUI <- function(...) {
  fluidPage(

    fluidRow(
      # No padding column with width = 4
      column(
        width = 4,
        djprshiny::djpr_h1_box("Victoria's industries") %>% fluidRow(),
        shinydashboard::box(
          width = 12,
          style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
          p(
            "On this page, we explore the employment trends of Victorian industries. ",
            "The ",
            shiny::a("ABS", href = "https://www.abs.gov.au/ausstats/abs@.nsf/mf/1292.0"),
            " classifies businesses into one of 19 broad industry divisions, each of which is further ",
            " divided into sub-divisions, groups, and classes.",
            "Note that industries data is not seasonally adjusted and is released quarterly."
          )
        ) %>% fluidRow()
      ),
      box(
        width = 8,
        title = h3("Number of people employed by industry"),
        uiOutput("table_industries_summary") %>%
          djpr_with_spinner()
      )
    ),
    djpr_box_ui("industries_empchange_sincecovid_bar", width = 12) %>% fluidRow(),
    focus_box(
      title = h3("Industry employment trends"),
      inputs = selectInput(
        "chosen_industry",
        label = span("Choose an industry", style = "color:#FFFFFF;"),
        choices = c(
          "Accommodation and Food Services",
          "Arts and Recreation Services",
          "Education and Training",
          "Transport, Postal and Warehousing",
          "Financial and Insurance Services",
          "Electricity, Gas, Water and Waste Services",
          "Professional, Scientific and Technical Services",
          "Health Care and Social Assistance",
          "Agriculture, Forestry and Fishing",
          "Mining",
          "Manufacturing",
          "Information Media and Telecommunications",
          "Administrative and Support Services",
          "Public Administration and Safety",
          "Construction",
          "Wholesale Trade",
          "Retail Trade",
          "Rental, Hiring and Real Estate Services",
          "Other Services"
        ),
        width = "100%"
      ),
      fluidRow(
        djpr_box_ui(
          id = "industries_emp_line",
          date_slider(
            id = "industries_emp_line",
            table_no = "6291005",
            value = c(Sys.Date() - (365 * 10), data_dates$`6291005`$max)
          )
        ) %>%
          async_no_background(),
        djpr_box_ui("industries_emp_bysex_bar") %>%
          async_no_background(),
        column(
          12,
          uiOutput("industries_employment") %>%
            djpr_with_spinner()
        )
      )
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

page_industries <- function(input, output, session) {
  output$table_industries_summary <- renderUI({
    table_industries_summary() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  })

  djpr_box_server(
    id = "industries_empchange_sincecovid_bar",
    plot_fun = viz_industries_empchange_sincecovid_bar
  )

  djpr_box_server(
    id = "industries_emp_line",
    plot_fun = viz_industries_emp_line,
    dates = input$dates,
    input_from_server = list(chosen_industry = reactive(input$chosen_industry))
  )

  djpr_box_server(
    id                = "industries_emp_bysex_bar",
    plot_fun          = viz_industries_emp_bysex_bar,
    input_from_server = list(chosen_industry = reactive(input$chosen_industry))
  )

  output$industries_employment <- renderUI({
    table_industries_employment(chosen_industry = input$chosen_industry) %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  }) %>%
    bindCache(
      data_dates$`6291005`$max,
      input$chosen_industry
    )
}
