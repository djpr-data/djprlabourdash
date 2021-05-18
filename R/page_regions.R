page_regions <- function(...) {
  djpr_tab_panel(
    title = "Regions",
    h1("Regions of Victoria"),
    HTML(
      paste0(
        "Employment and unemployment is not distributed evenly across Victoria. ",
        "This page breaks down the data by region - focusing particularly on ",
        "the 17 different regions at the ",
        htmltools::a("'statistical area level 4', or SA4 level",
          href = "https://www.abs.gov.au/websitedbs/D3310114.nsf/home/Australian+Statistical+Geography+Standard+(ASGS)"
        ),
        ", as defined by the ABS. Eight of Victoria's 17 SA4 regions are in Greater Melbourne,",
        " with the balance covering the full breadth of the state."
      )
    ),
    h2("Unemployment by region"),
    djpr_plot_title("The unemployment rate varies widely across Victoria"),
    djpr_plot_subtitle("Unemployment rate by region (SA4), per cent"),
    fluidRow(
      column(
        6,
        leaflet::leafletOutput("reg_unemprate_map")
      ),
      column(
        6,
        plotOutput("reg_unemprate_bar")
      )
    ),
    djpr_plot_caption("Source: ABS Labour Force. Note: regional data is smoothed using a 3-month rolling average."),
    br(),
    "TEXT HERE",
    djpr_plot_ui("reg_unemprate_multiline"),
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
    fluidRow(
      style = "padding-left: 15px;border: 1px solid #53565A; box-shadow: 3px 3px 2px #AEAEAE;",
      fluidRow(),
      column(
        6,
        plotOutput("reg_sa4", height = 280),
        br(),
        htmlOutput("reg_sa4unemp_cf_broadregion_title", inline = FALSE),
        plotOutput("reg_sa4unemp_cf_broadregion", height = 300)
      ),
      column(
        6,
        br(),
        reactable::reactableOutput("table_region_focus"),
        br(),
      )
    ),
    h2("Employment by region"),
    htmlOutput("text_emp_regions"),
    djpr_plot_ui("reg_emp_regions_sincecovid_line"),
    br(),
    djpr_plot_ui("reg_unemprate_dispersion"),
    br()
  )
}
