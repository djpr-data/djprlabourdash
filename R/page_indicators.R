page_indicators <- function(...) {
  djpr_tab_panel(
    title = "Indicators",
    h1("Key indicators"),
    # tagList(
    #   "This page contains key labour force indicators, focusing on Victoria as a whole. ",
    #   shiny::tags$b("More information will be included with future releases. "),
    #   "For more information about particular sub-groups of Victorians - like young people, or women - ",
    #   "see the ",
    #   actionLink("link_inclusion", "inclusion page"),
    #   ". For information about how employment and unemployment varies across Victoria, see the ",
    #   actionLink("link_regions", "regions page"), "."
    # ),
    h2(br(), "Employment"),
    htmlOutput("ind_empgrowth_sincecovid_text"),
    reactable::reactableOutput("ind_emp_table") %>%
      djpr_with_spinner(hide.ui = TRUE),
    caption_reactable(),
    djpr_plot_ui("ind_emp_sincecovid_line"),
    djpr_plot_ui("ind_emppop_state_slope"),
    djpr_plot_ui("ind_empgro_line"),
    # htmlOutput("ind_emp_dotpoints"),
    # djpr_plot_ui("ind_empgro_line"),
    h2(br(), "Unemployment & underemployment"),
    reactable::reactableOutput(
      "ind_unemp_summary"
    ) %>%
      djpr_with_spinner(hide.ui = TRUE),
    caption_reactable("Youth unemployment rate is not seasonally adjusted. It is smoothed using a 3 month rolling average."),
    djpr_plot_ui("ind_unemprate_line"),
    djpr_plot_ui("ind_unemp_states_dot"),
    br(),
    djpr_plot_ui("ind_underut_area"),
    br(),
    h2(br(), "Hours worked"),
    djpr_plot_ui("ind_hoursworked_line"),
    br(),
    h2(br(), "Participation"),
    br(),
    djpr_plot_ui("ind_partrate_line"),
    djpr_plot_ui("ind_partrate_bar"),
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
