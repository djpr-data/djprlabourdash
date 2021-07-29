page_regions <- function(...) {
  djpr_tab_panel(
    title = "Regions",
    h1("Regions of Victoria"),
    # tagList(
    #   "This page contains information about employment and unemployment across",
    #   "the different regions of Victoria.",
    #   shiny::tags$b("More information will be included with future releases. "),
    #   "For more information about overall labour force indicators ",
    #   "see the ",
    #   actionLink("link_indicators", "indicators page"),
    #   ". For information about the labour force status of key groups of Victorians, see the ",
    #   actionLink("link_inclusion", "inclusion page"), "."
    # ),
    # Unemployment by region -----
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
    djpr_plot_title(textOutput("title_unemp_emppop_partrate_vic")),
    djpr_plot_subtitle(textOutput("subtitle_unemp_emppop_partrate_vic")),
    fluidRow(
      column(
        6,
        leaflet::leafletOutput("map_unemp_emppop_partrate_vic") %>%
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
    djpr_plot_ui("reg_unemprate_multiline",
      height = "500px"
    ),
    djpr_plot_ui("reg_unemprate_dispersion"),
    br(),

    # Regional Vic vs Greater Melb -----
    h2(br(), "Regional Victoria and Greater Melbourne"),
    djpr_plot_ui("reg_melvic_line"),
    htmlOutput("text_emp_regions"),
    djpr_plot_ui("reg_emp_regions_sincecovid_line"),

    # Victorian regions focus box ------
    h2(br(), "Victorian regions"),
    # Box for regional focus
    focus_box(
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
          djpr_with_spinner(),
        djpr_plot_caption("Source: ABS Labour Force, Detailed (monthly). Note: data is not seasonally adjusted; smoothed using a 3 month rolling average.")
      )
    ),
    br(),
    h2(br(), "Australian regions"),
    focus_box(
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
      column(
        6,
        djpr_plot_ui("reg_regionstates_bar", height = "600px")
      )
    ),
    br(),
    djpr_plot_ui("reg_emp_regionstates_sincecovid_line"),
    br(),
    htmlOutput("regions_footnote"),
    br()
  )
}
