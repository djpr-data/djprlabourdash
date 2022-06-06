page_indicatorsUI <- function(...) {
  fluidRow(

    # No padding column with width = 4
    column_nopad(
      width = 4,
      djprshiny::djpr_h1_box("Indicators"),
      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "This page contains key labour force indicators, focusing on Victoria as a whole."
      )
    ),

    # htmlOutput("ind_empgrowth_sincecovid_text"),

    box(
      width = 8,
      shiny::uiOutput("ind_emp_table") %>%
        djpr_with_spinner(hide.ui = TRUE)
    ),
    djpr_h2_box("Employment"),
    djpr_async_ui(
      "ind_emppop_state_line",
      width = 12,
      fluidRow(
        column(
          width = 6,
          date_slider(
            id = "ind_emppop_state_line",
            table_no = "6202023",
            value = c(Sys.Date() - years(5), data_dates$`6202023`$max)
          )
        ),
        column(
          width = 6,
          state_checkbox(
            id = "ind_emppop_state_line"
          )
        )
      )
    ),
    djpr_async_ui(
      "ind_empgro_line",
      width = 12,
      fluidRow(
        column(
          width = 12,
          date_slider(
            id = "ind_empgro_line",
            table_no = "6202012",
            value = c(Sys.Date() - years(5), data_dates$`6202012`$max)
          )
        )
      )
    ),
    djpr_async_ui("ind_gen_full_part_line", width = 6),
    djpr_async_ui("ind_emp_sincecovid_line", width = 6),
    djpr_h2_box("Unemployment & underemployment"),
    box(
      width = 12,
      uiOutput("ind_unemp_summary") %>%
        djpr_with_spinner(hide.ui = TRUE)
    ),
    djpr_async_ui(
      "ind_unemprate_line",
      width = 12,
      date_slider(
        id = "ind_unemprate_line",
        table_no = "6202012",
        value = c(Sys.Date() - years(5), data_dates$`6202012`$max)
      )
    ),
    djpr_h2_box("Effective unemployment rate"),
    shinydashboard::box(
      width = 12,
      "People who are employed but have not been able to work any hours do not count ",
      "towards the 'headline' unemployment rate. Effective unemployment rate includes these people ",
      "and is defined as the sum of unemployed persons and persons who are employed but worked zero ",
      "hours due to economic conditions or 'other reasons', divided by the labour force. ",
      "The unemployment rate is seasonally adjusted, while the effective unemployment rate includes ",
      "a three month average of persons working zero hours for economic or unstated reasons."
    ),
    djpr_async_ui("ind_effective_unemprate_line", width = 12),
    djpr_h2_box("Unemployment rates by state"),
    box(
      width = 12,
      uiOutput("table_ind_unemp_state") %>%
        djpr_with_spinner()
    ),
    djpr_async_ui("ind_unemp_states_dot", width = 12),
    djpr_async_ui(
      "ind_underut_area",
      width = 12,
      date_slider(
        id = "ind_underut_area",
        table_no = "6202023",
        value = c(Sys.Date() - years(10), data_dates$`6202023`$max)
      )
    ),
    djpr_h2_box("Hours worked"),
    djpr_async_ui(
      "ind_hoursworked_line",
      width = 12,
      date_slider(
        id = "ind_hoursworked_line",
        table_no = "6202019",
        value = c(as.Date("2000-01-01"), data_dates$`6202019`$max)
      )
    ),
    djpr_h2_box("Participation"),
    djpr_async_ui(
      id = "ind_partrate_line",
      width = 6,
      date_slider(
        id = "ind_partrate_line",
        table_no = "6202012",
        value = c(as.Date("2000-01-01"), data_dates$`6202012`$max)
      )
    ),
    djpr_async_ui("ind_partrate_bar", width = 6),
    djpr_async_ui(
      id = "ind_partrate_un_line",
      width = 12,
      date_slider(
        id = "ind_partrate_un_line",
        table_no = "6202012",
        value = c(Sys.Date() - (10 * 365), data_dates$`6202012`$max)
      )
    ),
    shinydashboard::box(
      width = 12,
      "A fall in the unemployment rate can mean very different things depending on whether the participation rate is rising - more people have joined the labour force - or falling. ",
      "The chart below shows how the unemployment and participation rates changed over the last month or year, and how that compares to past changes in the Victorian labour market. ",
      "Choose whether you would like to examine monthly, or yearly, changes in the unemployment and participation rates."
    ),
    djpr_async_ui(
      "ind_partrate_un_scatter",
      width = 12,
      shiny::selectInput(
        shiny::NS("ind_partrate_un_scatter", "selected_period"),
        label = "Compare monthly or yearly change",
        selected = "year",
        choices = c(
          "Monthly" = "month",
          "Yearly" = "year"
        )
      )
    ),
    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        "This dashboard is produced by the <b>Strategy and Priority ",
        "Projects - Data + Analytics</b> team at the Victorian Department ",
        "of Jobs, Precincts and Regions. The <b>latest data in this ",
        "dashboard is for ", format(data_dates$`6202012`$max, "%B %Y"),
        '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'
      )
    )
  )
}


page_indicators <- function(input, output, session) {

  # output$ind_empgrowth_sincecovid_text <- renderUI({
  #   text_active(
  #     paste(
  #       "There were XX million Victorians employed in XX, compared to XX million in XX.",
  #       "Employment changed by XX per cent over the year to XX,",
  #       "a",
  #       dplyr::case_when(
  #         get_summ("A84423349V", .data$ptile_d_year_abs) < 0.33 ~
  #           "relatively sluggish",
  #         get_summ("A84423349V", .data$ptile_d_year_abs) > 0.67 ~
  #           "relatively rapid",
  #         TRUE ~ "normal"
  #       ),
  #       "pace of growth for Victoria compared to historical trends.",
  #       "Over the past year, employment across Australia grew by XX per cent.",
  #       "Employment in Victoria is XX per cent",
  #       dplyr::if_else(sign(get_summ("A84423349V", .data$d_year_perc)) > 0,
  #         "above",
  #         "below"
  #       ),
  #       "its level from a year earlier."
  #     ),
  #     c(
  #       round2(get_summ("A84423349V", .data$latest_value) / 1000000, 3),
  #       get_summ("A84423349V", .data$latest_period),
  #       round2(get_summ("A84423349V", .data$prev_value) / 1000000, 3),
  #       format(get_summ("A84423349V", .data$prev_date), "%B"),
  #       round2(get_summ("A84423349V", .data$d_year_perc), 1),
  #       format(get_summ("A84423349V", .data$latest_date), "%B"),
  #       round2(get_summ("A84423043C", .data$d_year_perc), 1),
  #       round2(get_summ("A84423349V", .data$d_year_perc), 1)
  #     )
  #   )
  # })

  djpr_async_server(
    id = "ind_emp_sincecovid_line",
    plot_fun = viz_ind_emp_sincecovid_line # ,
    # data = dash_data %>%
    #   dplyr::filter(series_id %in% c("A84423043C", "A84423349V"),
    #                 date >= as.Date("2020-01-01"))
  )

  # Indicators: table of employment indicators
  output$ind_emp_table <- renderUI({
    table_ind_employment() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(data_dates$`6202012`$max)

  # Indicators: line graph of emp-pop ratios in states, with state selector boxes
  djpr_async_server(
    id = "ind_emppop_state_line",
    plot_fun = viz_ind_emppop_state_line,
    date_filter = input$dates,
    state_filter = input$states
  )

  # Indicators: line chart of annual employment growth in Vic & Aus
  djpr_async_server(
    id = "ind_empgro_line",
    plot_fun = viz_ind_empgro_line,
    dates = input$dates
  )

  # Indicators: cumulative change in PT / FT since COVID
  djpr_async_server(
    id = "ind_gen_full_part_line",
    plot_fun = viz_ind_gen_full_part_line
  )

  # Indicators: unemployment ------
  output$ind_unemp_summary <- renderUI({
    table_ind_unemp_summary() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(data_dates$`6202012`$max)

  # Indicators: line chart of Aus v Vic
  djpr_async_server(
    id = "ind_unemprate_line",
    plot_fun = viz_ind_unemprate_line,
    dates = input$dates
  )

  # Indicators: effective unemployment rate
  djpr_async_server(
    id = "ind_effective_unemprate_line",
    plot_fun = viz_ind_effective_unemprate_line
  )

  # Indicators: table of unemployment rates by state
  output$table_ind_unemp_state <- renderUI({
    table_ind_unemp_state() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(data_dates$`6202012`$max)

  # Indicators: dot plot of unemp rate by state
  djpr_async_server(
    id = "ind_unemp_states_dot",
    plot_fun = viz_ind_unemp_states_dot
  )

  djpr_async_server(
    id = "ind_underut_area",
    plot_fun = viz_ind_underut_area,
    date_filter = input$dates
  )

  # Indicators: hours worked ----
  djpr_async_server(
    id = "ind_hoursworked_line",
    plot_fun = viz_ind_hoursworked_line,
    dates = input$dates
  )

  # Indicators: participation ----
  djpr_async_server(
    id = "ind_partrate_bar",
    plot_fun = viz_ind_partrate_bar
  )

  djpr_async_server(
    id = "ind_partrate_un_line",
    plot_fun = viz_ind_partrate_un_line,
    dates = input$dates
  )

  djpr_async_server(
    id = "ind_partrate_un_scatter",
    plot_fun = viz_ind_partrate_un_scatter,
    selected_period = input$selected_period
  )

  djpr_async_server(
    id = "ind_partrate_line",
    plot_fun = viz_ind_partrate_line,
    dates = input$dates
  )
}
