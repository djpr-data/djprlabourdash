page_age <- function(...) {
  djpr_tab_panel(
    title = "Age",
    br(),
    h2(br(), "Overview"),
    "Labour force data disaggregated by age can be volatile, and most of this data",
    " is not seasonally adjusted by the ABS. DJPR smooths the data ",
    "by using 12-month rolling averages. While this assists in removing noise",
    " to focus on the underlying trends, it makes large month-to-month changes ",
    "in underlying conditions less apparent.",
    uiOutput("table_gr_youth_summary"),
    fluidRow(
      column(
        6,
        djpr_plot_ui("gr_yth_emp_sincecovid_line")
      ),
      column(
        6,
        djpr_plot_ui("gr_yth_lfpartrate_vicaus_line")
      )
    ),
    br(),
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
      column(
        6,
        djpr_plot_ui("gr_youth_states_dot",
          height = "640px"
        )
      ),
      column(
        6,
        djpr_plot_ui("gr_ages_line",
          height = "200px"
        ),
        djpr_plot_ui("gr_yth_melbvrest_line",
          height = "200px"
        )
      ),
      column(
        12,
        djpr_plot_ui("gr_youth_vicaus_line")
      )
    ),
    br(),
    h2(br(), "Detailed labour force status of Victorian youth"),
    djpr_plot_ui("gr_youth_full_part_line"),
    djpr_plot_ui("gr_youth_eduemp_waterfall",
      interactive = FALSE
    ),
    djpr_plot_ui("gr_yth_mostvuln_line"),
    h2(br(), "Youth unemployment rate by region"),
    uiOutput("table_gr_youth_unemp_region"),
    br(),
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
        leaflet::leafletOutput("map_youth_unemp_emppop_partrate_vic") %>%
          djpr_with_spinner()
      ),
      column(
        6,
        djpr_plot_ui("gr_youth_unemp_emppop_partrate_bar")
      )
    ),
    h2(br(), "Victorian jobactive caseload by age"),
    uiOutput("table_jobactive_mature_age") %>%
      djpr_with_spinner(),
    br()
  )
}
