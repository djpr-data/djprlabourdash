page_inclusion <- function(...) {
  djpr_tab_panel(
    title = "Inclusion",
    h1("Key groups"),
    # tagList(
    #   "This page contains information about the labour force status of key groups of ",
    #   "Victorians, such as women, and young people. ",
    #   shiny::tags$b("More information will be included with future releases. "),
    #   "For more information about overall labour force indicators ",
    #   "see the ",
    #   actionLink("link_indicators", "indicators page"),
    #   ". For information about how employment and unemployment varies across Victoria, see the ",
    #   actionLink("link_regions", "regions page"), "."
    # ),
    br(),
    h2(br(), "Women and men"),
    uiOutput("table_gr_sex"),
    djpr_plot_ui("gr_gen_emp_bar",
      interactive = FALSE
    ),
    djpr_plot_ui("gr_gen_emppopratio_line"),
    djpr_plot_ui("gr_gen_unemp_line"),
    djpr_plot_ui("gr_gen_partrate_line"),
    djpr_plot_ui("gr_full_part_line"),
    h2(br(), "Young people"),
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
      h3("Labour force status of Victorian youth"),
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
    h3(br(), "Detailed labour force status of Victorian youth"),
    djpr_plot_ui("gr_youth_full_part_line"),
    djpr_plot_ui("gr_youth_eduemp_waterfall",
      interactive = FALSE
    ),
    # djpr_plot_ui("gr_yth_mostvuln_line"),
    h3(br(), "Youth unemployment rate by region"),
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
      column(6,
             leaflet::leafletOutput("map_youth_unemp_emppop_partrate_vic") %>%
               djpr_with_spinner()),
      column(6,
             djpr_plot_ui("gr_youth_unemp_emppop_partrate_bar"))
    ),
    h2(br(), "Long-term unemployed"),
    djpr_plot_ui("gr_ltunemp_line"),
    djpr_plot_ui("gr_ltunvic_bar",
      interactive = FALSE
    ),
    djpr_plot_ui("gr_ltunvic_area",
      interactive = FALSE
    ),
    br(),
    htmlOutput("inclusion_footnote"),
    br()
  )
}
