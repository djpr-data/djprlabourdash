page_ausregions <- function(...) {
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
          )
        )
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
