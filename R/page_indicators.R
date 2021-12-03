page_indicators <- function(...) {
  djpr_tab_panel(
    title = "Indicators",
    h1("Key indicators"),
    paste0(
      "This page contains key labour force indicators, focusing on Victoria as a whole."
    ),
    h2(br(), "Employment"),
    # htmlOutput("ind_empgrowth_sincecovid_text"),
    uiOutput("ind_emp_table") %>%
      djpr_with_spinner(hide.ui = TRUE),
    br(),
    fluidRow(
      column(6, djpr_plot_ui("ind_emppop_state_slope")),
      column(6, djpr_plot_ui("ind_empgro_line"))
    ),
    br(),
    fluidRow(
      column(6, djpr_plot_ui("ind_gen_full_part_line")),
      column(6, djpr_plot_ui("ind_emp_sincecovid_line"))
    ),
    h2(br(), "Unemployment & underemployment"),
    uiOutput("ind_unemp_summary") %>%
      djpr_with_spinner(hide.ui = TRUE),
    djpr_plot_ui("ind_unemprate_line"),
    h4(br(), "Effective unemployment rate"),
    paste0(
      "People who are employed but have not been able to work any hours do not count ",
      "towards the 'headline' unemployment rate. Effective unemployment rate includes these people ",
      "and is defined as the sum of unemployed persons and persons who are employed but worked zero ",
      "hours due to economic conditions or 'other reasons', divided by the labour force. ",
      "The unemployment rate is seasonally adjusted, while the effective unemployment rate includes ",
      "a three month average of persons working zero hours for economic or unstated reasons."
    ),
    br(),
    djpr_plot_ui("ind_effective_unemprate_line"),
    br(),
    h4(br(), "Unemployment rates by state"),
    uiOutput("table_ind_unemp_state"),
    br(),
    djpr_plot_ui("ind_unemp_states_dot"),
    br(),
    djpr_plot_ui("ind_underut_area",
      interactive = FALSE
    ),
    br(),
    h2(br(), "Hours worked"),
    djpr_plot_ui("ind_hoursworked_line"),
    br(),
    h2(br(), "Participation"),
    br(),
    djpr_plot_ui("ind_partrate_line"),
    djpr_plot_ui("ind_partrate_bar",
      interactive = FALSE
    ),
    djpr_plot_ui("ind_partrate_un_line"),
    br(),
    paste0(
      "A fall in the unemployment rate can mean very different things depending on whether the participation rate is rising - more people have joined the labour force - or falling. ",
      "The chart below shows how the unemployment and participation rates changed over the last month or year, and how that compares to past changes in the Victorian labour market. ",
      "Choose whether you would like to examine monthly, or yearly, changes in the unemployment and participation rates."
    ),
    br(),
    br(),
    shiny::selectInput("ind_partrate_un_scatter_selected_period",
      label = "Compare monthly or yearly change",
      selected = "year",
      choices = c(
        "Monthly" = "month",
        "Yearly" = "year"
      )
    ),
    djpr_plot_ui("ind_partrate_un_scatter"),
    br(),
    htmlOutput("indicators_footnote")
  )
}
