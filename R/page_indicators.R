page_indicators <- function(...) {
  djpr_tab_panel(
    title = "Indicators",
    h1("Key indicators"),
    tagList(
      "This page contains key labour force indicators, focusing on Victoria as a whole. ",
      htmltools::tags$b("More information will be included with future releases. "),
      "For more information about particular sub-groups of Victorians - like young people, or women - ",
      "see the ",
      actionLink("link_inclusion", "inclusion page"),
      ". For information about how employment and unemployment varies across Victoria, see the ",
      actionLink("link_regions", "regions page"), "."
    ),
    h2("Employment"),
    htmlOutput("ind_empgrowth_sincecovid_text"),
    reactable::reactableOutput("ind_emp_table") %>%
      djpr_with_spinner(hide.ui = TRUE),
    reactable_caption(),
    djpr_plot_ui("ind_emp_sincecovid_line"),
    djpr_plot_ui("ind_emppop_state_slope"),
    # htmlOutput("ind_emp_dotpoints"),
    # djpr_plot_ui("ind_empgro_line"),
    h2("Unemployment & underemployment"),
    reactable::reactableOutput(
      "ind_unemp_summary"
    ) %>%
      djpr_with_spinner(hide.ui = TRUE),
    reactable_caption("Youth unemployment rate is not seasonally adjusted. It is smoothed using a 3 month rolling average."),
    djpr_plot_ui("ind_unemprate_line"),
    djpr_plot_ui("ind_unemp_states_dot"),
    br(),
    djpr_plot_ui("ind_underut_area"),
    br(),
    h2("Hours worked"),
    djpr_plot_ui("ind_hoursworked_line"),
    br(),
    h2("Participation"),
    br(),
    djpr_plot_ui("ind_partrate_bar"),
    br(),
    htmlOutput("indicators_footnote")
  )
}
