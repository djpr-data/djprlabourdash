page_age <- function(...) {
  shiny::fluidRow(
    column(
      width = 3,
      djpr_h2_box("Overview"),
      shinydashboard::box(
        width = 12,
        height = "100%",
        p(
          "Labour force data disaggregated by age can be volatile, and most of this data is not seasonally adjusted by the ABS. DJPR smooths the data by using 12-month rolling averages. While this assists in removing noise to focus on the underlying trends, it makes large month-to-month changes in underlying conditions less apparent.",
          style = "font-size:13px;"
        )
      )
    ),
    uiOutput("table_gr_youth_summary") %>%
      djpr_with_spinner() %>%
      shinydashboard::box(width = 9),
    djpr_plot_box("gr_yth_emp_sincecovid_line"),
    djpr_plot_box("gr_yth_lfpartrate_vicaus_line"),
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
        djpr_plot_box("gr_youth_states_dot",
          height = "640px",
          width = 12
        )
      ),
      column(
        6,
        djpr_plot_box("gr_ages_line",
          height = "200px",
          width = 12
        ),
        djpr_plot_box("gr_yth_melbvrest_line",
          height = "200px",
          width = 12
        )
      ),
      column(
        12,
        djpr_plot_box(
          "gr_youth_vicaus_line",
                      width = 12)
      )
    ),
    djpr_h2_box("Detailed labour force status of Victorian youth"),
    djpr_plot_box("gr_youth_full_part_line"),
    djpr_plot_box("gr_youth_eduemp_waterfall",
      interactive = FALSE
    ),
    djpr_plot_box("gr_yth_mostvuln_line"),
    djpr_h2_box("Youth unemployment rate by region"),
    uiOutput("table_gr_youth_unemp_region") %>%
      djpr_with_spinner(),

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
        plotOutput("gr_youth_unemp_emppop_partrate_bar")
      )
    ),
    djpr_h2_box("Jobactive caseload by age"),
    uiOutput("table_jobactive_youth") %>%
      djpr_with_spinner(),

    djpr_plot_box("gr_youth_jobactive_bar"),

    djpr_plot_box("gr_age_jobactive_since_covid_line"),

    uiOutput("table_jobactive_mature_age") %>%
      djpr_with_spinner(),
    djpr_plot_box("gr_mature_age_jobactive_bar")
  )
}
