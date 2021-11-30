page_vicregions <- function(...) {
  djpr_tab_panel(
    title = "Victorian Regions",
    h1("Regions of Victoria"),
    # Unemployment by region -----
    br(),
    "Victoria contains a range of diverse regions, both within Greater Melbourne",
    " and outside the metropolitan area.",
    br(),
    "Below we explore the regional differences in historical and current labour force status ",
    "within Victoria.",
    br(),
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
    djpr_plot_title(textOutput("title_reg_unemp_emppop_partrate_vic")),
    djpr_plot_subtitle(textOutput("subtitle_reg_unemp_emppop_partrate_vic")),
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
    ),
    djpr_plot_caption("Source: ABS Labour Force, Detailed (monthly). Note: data is not seasonally adjusted; smoothed using a 3 month rolling average."),
    br(),
    br(),
    selectInput("lf_status_multiline",
      label = "Choose an indicator",
      choices = c(
        "Unemployment rate" = "unemp_rate",
        "Participation rate" = "part_rate",
        "Employment to population ratio" = "emp_pop"
      ),
      selected = "unemp_rate"
    ),
    djpr_plot_ui("reg_unemp_emppop_partrate_multiline",
      height = "500px",
      interactive = FALSE
    ),
    br(),
    br(),
    "The unemployment rate always varies substantially across Victoria. The amount of ",
    "variation across the regions of Victoria changes over time - the gap between the ",
    "highest and lowest unemployment rate in the state grows and shrinks. ",
    "The graphs below explore the level of that dispersion (i.e. the ",
    "difference between minimum and maximum) of unemployment rates over time", "
    in different regions in Victoria. The breakdown of regions is by ",
    shiny::a("SA4.", href = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/main-structure-and-greater-capital-city-statistical-areas/statistical-area-level-4"),
    br(),
    br(),
    selectInput(
      "sa4_type_dispersion",
      label = "Choose regions",
      choices = c(
        "All Victorian SA4s" = "all",
        "Metropolitan Melbourne SA4s" = "metropolitan",
        "Rural and regional SA4s" = "regional"
      ),
      selected = "all"
    ),
    djpr_plot_ui("reg_unemprate_dispersion",
      interactive = FALSE
    ),
    br(),

    # Regional Vic vs Greater Melb -----
    h2(br(), "Regional Victoria and Greater Melbourne"),
    djpr_plot_ui("reg_melvic_line"),
    # htmlOutput("text_emp_regions"),
    br(),
    djpr_plot_ui("reg_emp_regions_sincecovid_line"),

    # Victorian regions focus box ------
    h2(br(), "Regional Victoria"),
    # Box for regional focus
    focus_box(
      h4("Compare regions of Victoria"),
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
          djpr_with_spinner()
      )
    ),
    br(),
    h2(br(), "Victorian jobactive caseload by employment region"),
    uiOutput("title_reg_jobactive_vic"),
    fluidRow(
      column(
        6,
        leaflet::leafletOutput("map_reg_jobactive_vic") %>%
          djpr_with_spinner()
      ),
      column(
        6,
        plotOutput("reg_jobactive_vic_bar")
      )
    ),
    br(),
    br(),
    br(),
    uiOutput("table_jobactive_regions") %>%
      djpr_with_spinner(),
    br(),
    htmlOutput("vicregions_footnote")
  )
}
