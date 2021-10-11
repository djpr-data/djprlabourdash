page_industries <- function(...) {
  djpr_tab_panel(
    title = "Industries",
    h1("Victoria's industries"),
    br(),
    "On this page, we explore the economic and financial performance of Victorian industries. ",
    "The ",
    shiny::a("ABS", href = "https://www.abs.gov.au/ausstats/abs@.nsf/mf/1292.0"),
    " classifies businesses into one of 19 broad industry divisions, each of which is further ",
    " divided into sub-divisions, groups, and classes.",
    br(),
    br(),
    "Note that industries data is not seasonally adjusted and is released quarterly.",
    uiOutput("table_industries_summary"),
    djpr_plot_ui("industries_empchange_sincecovid_bar"),
    br(),
    focus_box(
      selectInput(
        "chosen_industry",
        label = "Choose an industry",
        choices = c(
          "Accommodation and Food Services",
          "Arts and Recreation Services",
          "Education and Training",
          "Transport, Postal and Warehousing",
          "Financial and Insurance Services",
          "Electricity, Gas, Water and Waste Services",
          "Professional, Scientific and Technical Services",
          "Health Care and Social Assistance",
          "Agriculture, Forestry and Fishing",
          "Mining",
          "Manufacturing",
          "Information Media and Telecommunications",
          "Administrative and Support Services",
          "Public Administration and Safety",
          "Construction",
          "Wholesale Trade",
          "Retail Trade",
          "Rental, Hiring and Real Estate Services",
          "Other Services"
        ),
        width = "100%"
      ),
      column(
        6,
        djpr_plot_ui("industries_emp_line")
      ),
      column(
        6,
        uiOutput("industries_employment")
      ),
      br(),
      djpr_plot_ui("industries_emp_bysex_bar")
    ),
    br(),
    htmlOutput("industries_footnote"),
    br()
  )
}
