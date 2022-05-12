page_ausregionsUI <- function(...) {
  djpr_tab_panel(
    title = "Australian Regions",
    # Unemployment by Australian Regions -------------
    br(),
    br(),
    "This section explores labour force indicators in regional and metropolitan areas of Australia. ",
    br(),
    h2("Unemployment rate in Australian regional areas"),
    uiOutput("table_reg_nonmetro_states_unemprate"),
    focus_box(
      h4("Compare regional areas of Australian states"),
      selectInput(
        inputId = "aus_regions_indicator",
        label = "Select indicator",
        choices = c(
          "Unemployment rate" = "unemp_rate",
          "Participation rate" = "part_rate",
          "Employment-to-population ratio" = "emp_pop"
        ),
        width = "100%",
        selected = "unemp_rate"
      ),
      column(6,
        djpr_plot_ui("reg_regionstates_dot"),
        height = "600px"
      ),
      column(6, djpr_plot_ui("reg_regionstates_bar",
        height = "600px",
        interactive = FALSE
      ))
    ),
    br(),
    djpr_plot_ui("reg_emp_regionstates_sincecovid_line"),
    br(),
    h2(br(), "Australian metropolitan areas"),
    h4("Unemployment rates in Australian major cities"),
    uiOutput("table_reg_metro_states_unemprate"),
    htmlOutput("regions_footnote"),
    br()
  )
}


page_ausregions <- function(input, output, session, plt_change) {

  output$table_reg_nonmetro_states_unemprate <- renderUI({
    table_reg_nonmetro_states_unemprate() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  output$table_reg_metro_states_unemprate <- renderUI({
    table_reg_metro_states_unemprate() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Regions: National focus box -----
  djpr_plot_server("reg_regionstates_dot",
                   plot_function = viz_reg_regionstates_dot,
                   data = filter_dash_data(c(
                     "A84599628W",
                     "A84599629X",
                     "A84599630J",
                     "A84600078W",
                     "A84600079X",
                     "A84600080J",
                     "A84599784X",
                     "A84599785A",
                     "A84599786C",
                     "A84599718A",
                     "A84599719C",
                     "A84599720L",
                     "A84600246W",
                     "A84600247X",
                     "A84600248A",
                     "A84599634T",
                     "A84599635V",
                     "A84599636W",
                     "A84599610X",
                     "A84599611A",
                     "A84599612C"
                   ),
                   df = dash_data
                   ),
                   selected_indicator = reactive(input$aus_regions_indicator),
                   plt_change = plt_change,
                   width_percent = 46,
                   height_percent = 150,
                   date_slider = FALSE
  )

  djpr_plot_server("reg_regionstates_bar",
                   viz_reg_regionstates_bar,
                   data = filter_dash_data(c(
                     "15-24_employed_rest of nsw",
                     "15-24_employed_rest of nt",
                     "15-24_employed_rest of qld",
                     "15-24_employed_rest of sa",
                     "15-24_employed_rest of tas.",
                     "15-24_employed_rest of vic.",
                     "15-24_employed_rest of wa",
                     "15-24_nilf_rest of nsw",
                     "15-24_nilf_rest of nt",
                     "15-24_nilf_rest of qld",
                     "15-24_nilf_rest of sa",
                     "15-24_nilf_rest of tas.",
                     "15-24_nilf_rest of vic.",
                     "15-24_nilf_rest of wa",
                     "15-24_unemployed_rest of nsw",
                     "15-24_unemployed_rest of nt",
                     "15-24_unemployed_rest of qld",
                     "15-24_unemployed_rest of sa",
                     "15-24_unemployed_rest of tas.",
                     "15-24_unemployed_rest of vic.",
                     "15-24_unemployed_rest of wa",
                     "25-54_employed_rest of nsw",
                     "25-54_employed_rest of nt",
                     "25-54_employed_rest of qld",
                     "25-54_employed_rest of sa",
                     "25-54_employed_rest of tas.",
                     "25-54_employed_rest of vic.",
                     "25-54_employed_rest of wa",
                     "25-54_nilf_rest of nsw",
                     "25-54_nilf_rest of nt",
                     "25-54_nilf_rest of qld",
                     "25-54_nilf_rest of sa",
                     "25-54_nilf_rest of tas.",
                     "25-54_nilf_rest of vic.",
                     "25-54_nilf_rest of wa",
                     "25-54_unemployed_rest of nsw",
                     "25-54_unemployed_rest of nt",
                     "25-54_unemployed_rest of qld",
                     "25-54_unemployed_rest of sa",
                     "25-54_unemployed_rest of tas.",
                     "25-54_unemployed_rest of vic.",
                     "25-54_unemployed_rest of wa",
                     "55+_employed_rest of nsw",
                     "55+_employed_rest of nt",
                     "55+_employed_rest of qld",
                     "55+_employed_rest of sa",
                     "55+_employed_rest of tas.",
                     "55+_employed_rest of vic.",
                     "55+_employed_rest of wa",
                     "55+_nilf_rest of nsw",
                     "55+_nilf_rest of nt",
                     "55+_nilf_rest of qld",
                     "55+_nilf_rest of sa",
                     "55+_nilf_rest of tas.",
                     "55+_nilf_rest of vic.",
                     "55+_nilf_rest of wa",
                     "55+_unemployed_rest of nsw",
                     "55+_unemployed_rest of nt",
                     "55+_unemployed_rest of qld",
                     "55+_unemployed_rest of sa",
                     "55+_unemployed_rest of tas.",
                     "55+_unemployed_rest of vic.",
                     "55+_unemployed_rest of wa"
                   ),
                   df = dash_data
                   ),
                   selected_indicator = req(reactive(input$aus_regions_indicator)),
                   plt_change = plt_change,
                   height_percent = 150,
                   width_percent = 46,
                   interactive = FALSE,
                   date_slider = FALSE
  )

  djpr_plot_server(
    "reg_emp_regionstates_sincecovid_line",
    viz_reg_emp_regionstates_sincecovid_line,
    data = filter_dash_data(c(
      "A84600075R",
      "A84599625R",
      "A84599781T",
      "A84599607K",
      "A84600243R",
      "A84599715V",
      "A84599631K"
    ),
    df = dash_data
    ) %>%
      dplyr::mutate(
        state = dplyr::case_when(
          .data$series == ">> Rest of Vic. ;  Employed total ;  Persons ;" ~
            "Reg. Vic",
          .data$series == ">> Rest of NSW ;  Employed total ;  Persons ;" ~
            "Reg. NSW",
          .data$series == ">> Rest of Qld ;  Employed total ;  Persons ;" ~
            "Reg. QLD",
          .data$series == ">>> Northern Territory - Outback ;  Employed total ;  Persons ;" ~
            "Reg. NT",
          .data$series == ">> Rest of WA ;  Employed total ;  Persons ;" ~
            "Reg. WA",
          .data$series == ">> Rest of SA ;  Employed total ;  Persons ;" ~
            "Reg. SA",
          .data$series == ">> Rest of Tas. ;  Employed total ;  Persons ;" ~
            "Reg. Tas",
          TRUE ~ .data$state
        )
      ),
    check_box_options = c(
      "Reg. Vic",
      "Reg. NSW",
      "Reg. QLD",
      "Reg. Tas",
      "Reg. WA",
      "Reg. NT",
      "Reg. SA"
    ),
    check_box_var = .data$state,
    check_box_selected = c("Reg. NSW", "Reg. Vic"),
    date_slider = FALSE,
    plt_change = plt_change
  )

  observeEvent(input$link_ausregions, {
    updateNavbarPage(session, "navbarpage", "tab-ausregions")
  })

}
