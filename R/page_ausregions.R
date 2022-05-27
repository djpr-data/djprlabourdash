
page_ausregionsUI <- function(...) {
  fluidRow(

    # Unemployment by Australian Regions -------------

    djpr_h2_box("Unemployment rate in Australian regional areas"),
    box(
      width = 12,
      uiOutput("table_reg_nonmetro_states_unemprate") %>%
      djpr_with_spinner()
      ),

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
        djpr_async_ui("reg_regionstates_dot", height = "600px") %>%
          async_no_background(),
        djpr_async_ui("reg_regionstates_bar", height = "600px") %>%
          async_no_background()
      )

    ),
    djpr_async_ui(
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
      ),
    djpr_h2_box("Australian metropolitan areas"),
    box(
      width = 12,
      title = "Unemployment rates in Australian major cities",
      uiOutput("table_reg_metro_states_unemprate") %>%
        djpr_with_spinner()
    ),
    htmlOutput("regions_footnote")
  )
}


page_ausregions <- function(input, output, session, plt_change, series_latestdates, footnote) {
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
  djpr_async_server(
    id = "reg_regionstates_dot",
    plot_fun = viz_reg_regionstates_dot,
    input_from_server = list(selected_indicator = reactive(input$aus_regions_indicator))
  )

  djpr_async_server(
    id = "reg_regionstates_bar",
    plot_fun = viz_reg_regionstates_bar,
    input_from_server = list(
      selected_indicator = reactive(input$aus_regions_indicator)
      )
  )

  djpr_async_server(
    "reg_emp_regionstates_sincecovid_line",
    viz_reg_emp_regionstates_sincecovid_line,
    states = input$states
  )

}
