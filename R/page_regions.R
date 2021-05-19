page_regions <- function(...) {
  djpr_tab_panel(
    title = "Regions",
    h1("Regions of Victoria"),
    tagList(
      "This page contains information about employment and unemployment across",
      "the different regions of Victoria.",
      "For more information about overall labour force indicators ",
      "see the ",
      actionLink("link_indicators", "indicators page"),
      ". For information about the labour force status of key groups of Victorians, see the ",
      actionLink("link_groups", "groups page"), "."
    ),
    h2("Greater Melbourne and the rest of Victoria"),
    djpr_plot_ui("reg_melvic_line"),
    htmlOutput("text_emp_regions"),
    djpr_plot_ui("reg_emp_regions_sincecovid_line"),
    h2("Regional focus"),
    selectInput("focus_region",
      label = "Choose a region of Victoria to examine in greater detail",
      selected = "Ballarat",
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
    # Box for regional focus
    fluidRow(
      style = "padding-left: 15px;border: 1px solid #53565A; box-shadow: 3px 3px 2px #AEAEAE;",
      fluidRow(),
      column(
        6,
        column(1),
        column(
          11,
          plotOutput("reg_sa4", height = 280) %>%
            djpr_with_spinner(),
          br(),
          htmlOutput("reg_sa4unemp_cf_broadregion_title", inline = FALSE) %>%
            djpr_with_spinner(),
          plotOutput("reg_sa4unemp_cf_broadregion", height = 300) %>%
            djpr_with_spinner()
        )
      ),
      column(
        6,
        br(),
        reactable::reactableOutput("table_region_focus") %>%
          djpr_with_spinner(),
        djpr_plot_caption("Source: ABS Labour Force, Detailed (monthly). Note: data is not seasonally adjusted; smoothed using a 3 month rolling average."),
        br(),
      )
    ),
    h2("Unemployment by region"),
    djpr_plot_title(textOutput("title_unemprate_vic")),
    djpr_plot_subtitle("Unemployment rate by region (SA4), per cent"),
    fluidRow(
      column(
        6,
        leaflet::leafletOutput("reg_unemprate_map") %>%
          djpr_with_spinner()
      ),
      column(
        6,
        plotOutput("reg_unemprate_bar") %>%
          djpr_with_spinner()
      )
    ),
    djpr_plot_caption("Source: ABS Labour Force, Detailed (monthly). Note: data is not seasonally adjusted; smoothed using a 3 month rolling average."),
    br(),
    djpr_plot_ui("reg_unemprate_multiline"),
    djpr_plot_ui("reg_unemprate_dispersion"),
    br(),
    h2("Employment by region"),
    br(),
    htmlOutput("regions_footnote"),
    br()
  )
}
