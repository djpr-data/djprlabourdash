page_indicatorsUI <- function(...) {

  fluidRow(
    djpr_h2_box("Key indicators"),
    shinydashboard::box(
      width = 12,
      "This page contains key labour force indicators, focusing on Victoria as a whole."
    ),

    djpr_h2_box("Employment"),

    # htmlOutput("ind_empgrowth_sincecovid_text"),

    box(
      uiOutput("ind_emp_table") %>%
        djpr_with_spinner(hide.ui = TRUE)
    ),

    fluidRow(
      djpr_async_ui("ind_emppop_state_line", width = 12)

    ),

    fluidRow(
      column(width = 6,
             date_slider(
               id = "ind_emppop_state_line_date-date_filter",
               table_no = "6202023")
      ),
      column(width = 6,
             state_checkbox(
               id = "ind_emppop_state_line_state"
             ))
    ),

    fluidRow(
      djpr_async_ui("ind_empgro_line", width = 12)
    ),

    fluidRow(
      djpr_async_ui("ind_gen_full_part_line", width = 6),
      djpr_async_ui("ind_emp_sincecovid_line", width = 6)
    ),

    djpr_h2_box("Unemployment & underemployment"),

    box(
      uiOutput("ind_unemp_summary") %>%
        djpr_with_spinner(hide.ui = TRUE)),

    fluidRow(
      djpr_async_ui("ind_unemprate_line", width = 12)
      ),

    djpr_h3_box("Effective unemployment rate"),

    shinydashboard::box(
      width = 12,
      "People who are employed but have not been able to work any hours do not count ",
      "towards the 'headline' unemployment rate. Effective unemployment rate includes these people ",
      "and is defined as the sum of unemployed persons and persons who are employed but worked zero ",
      "hours due to economic conditions or 'other reasons', divided by the labour force. ",
      "The unemployment rate is seasonally adjusted, while the effective unemployment rate includes ",
      "a three month average of persons working zero hours for economic or unstated reasons."
    ),

    fluidRow(
      djpr_async_ui("ind_effective_unemprate_line", width = 12)
      ),

    djpr_h3_box("Unemployment rates by state"),

    box(
      uiOutput("table_ind_unemp_state")
      ),

    fluidRow(
      djpr_async_ui("ind_unemp_states_dot", width = 12)
      ),

    fluidRow(
      djpr_async_ui("ind_underut_area", width = 12)
      ),

    djpr_h2_box("Hours worked"),

    fluidRow(
      djpr_async_ui("ind_hoursworked_line", width = 12)
      ),

    djpr_h2_box("Participation"),

    fluidRow(
      djpr_async_ui("ind_partrate_line", width = 6),
      djpr_async_ui("ind_partrate_bar", width = 6)
      ),

    fluidRow(
      djpr_async_ui("ind_partrate_un_line", width = 12)
    ),

    shinydashboard::box(
      width = 12,
      "A fall in the unemployment rate can mean very different things depending on whether the participation rate is rising - more people have joined the labour force - or falling. ",
      "The chart below shows how the unemployment and participation rates changed over the last month or year, and how that compares to past changes in the Victorian labour market. ",
      "Choose whether you would like to examine monthly, or yearly, changes in the unemployment and participation rates."
    ),

    shiny::selectInput("ind_partrate_un_scatter_selected_period",
      label = "Compare monthly or yearly change",
      selected = "year",
      choices = c(
        "Monthly" = "month",
        "Yearly" = "year"
      )
    ),
    djpr_async_ui("ind_partrate_un_scatter", width = 12),

    htmlOutput("indicators_footnote")
  )
}


page_indicators <- function(input, output, session, plt_change, series_latestdates, footnote) {

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
    plot_fun = viz_ind_emp_sincecovid_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c("A84423043C", "A84423349V"),
                    date >= as.Date("2020-01-01"))
  )


  # Indicators: table of employment indicators
  output$ind_emp_table <- renderUI({
    table_ind_employment() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Indicators: line graph of emp-pop ratios in states, with state selector boxes
  djpr_async_server(
    id = "ind_emppop_state_line",
    plot_fun = viz_ind_emppop_state_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A84423272J",
      "A84423356T",
      "A84423286W",
      "A84423370L",
      "A84423328J",
      "A84423300F",
      "A84423314V",
      "A84423342C"
    )) %>%
      dplyr::mutate(
        state = dplyr::case_when(
          .data$series == "Employment to population ratio ;  Persons ;  > Victoria ;" ~
            "Vic",
          .data$series == "Employment to population ratio ;  Persons ;  > New South Wales ;" ~
            "NSW",
          .data$series == "Employment to population ratio ;  Persons ;  > Queensland ;" ~
            "QLD",
          .data$series == "Employment to population ratio ;  Persons ;  > Northern Territory ;" ~
            "NT",
          .data$series == "Employment to population ratio ;  Persons ;  > Western Australia ;" ~
            "WA",
          .data$series == "Employment to population ratio ;  Persons ;  > South Australia ;" ~
            "SA",
          .data$series == "Employment to population ratio ;  Persons ;  > Tasmania ;" ~
            "Tas",
          .data$series == "Employment to population ratio ;  Persons ;  > Australian Capital Territory ;" ~
            "ACT",
          TRUE ~ .data$state
        )
      ),
    date_filter = input$date_filter,
    state_filter = input$ind_emppop_state_line_state
  )

  # Indicators: line chart of annual employment growth in Vic & Aus
  djpr_async_server(
    id = "ind_empgro_line",
    plot_fun = viz_ind_empgro_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
        "A84423349V",
        "A84423043C"
        ))
  )

  # Indicators: cumulative change in PT / FT since COVID
  djpr_async_server(
    id = "ind_gen_full_part_line",
    plot_fun = viz_ind_gen_full_part_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "pt_emp_vic",
      "A84423357V"),
      date >= as.Date("2020-01-01")
      ))

  # Indicators: unemployment ------
  output$ind_unemp_summary <- renderUI({
    table_ind_unemp_summary() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Indicators: line chart of Aus v Vic
  djpr_async_server(
    id = "ind_unemprate_line",
    plot_fun = viz_ind_unemprate_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A84423354L",
      "A84423050A")),
    date >= as.Date("2000-01-01")
  )

  # Indicators: effective unemployment rate
  djpr_async_server(
    id = "ind_effective_unemprate_line",
    plot_fun = viz_ind_effective_unemprate_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A84423350C",
      "A84423351F",
      "A84423354L",
      "employed full-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
      "employed part-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
      "employed full-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria",
      "employed part-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria"
    )),
    date >= as.Date("2019-06-01")
  )

  # Indicators: table of unemployment rates by state
  output$table_ind_unemp_state <- renderUI({
    table_ind_unemp_state() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Indicators: dot plot of unemp rate by state
  djpr_async_server(
    id = "ind_unemp_states_dot",
    plot_fun = viz_ind_unemp_states_dot,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
        "A84423354L",
        "A84423270C",
        "A84423368A",
        "A84423340X",
        "A84423326C",
        "A84423284T",
        "A84423312R",
        "A84423298F",
        "A84423050A"
      )
    )
  )

  djpr_async_server(
    id = "ind_underut_area",
    plot_func = viz_ind_underut_area,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A85223450L",
      "A85223451R",
      "A84423354L"
    )),
    date = Sys.Date() - (10 * 365)
  )

  # Indicators: hours worked ----
  djpr_async_server(
    id = "ind_hoursworked_line",
    plot_fun = viz_ind_hoursworked_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A84426256L",
      "A84426277X",
      "A84423689R",
      "A84423091W"
    )
    ),
    date >= as.Date("2000-01-01")
  )

  # Indicators: participation ----
  djpr_async_server(
    id = "ind_partrate_bar",
    plot_fun = viz_ind_partrate_bar,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A84423355R",
      "A84423271F",
      "A84423369C",
      "A84423341A",
      "A84423327F",
      "A84423285V",
      "A84423313T",
      "A84423299J",
      "A84423051C"
    )
    )
  )

  djpr_pasync_server(
    id = "ind_partrate_un_line",
    plot_fun = viz_ind_partrate_un_line,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A84423355R",
      "A84423354L"
    )
    )
    #date_slider_value_min = Sys.Date() - (10 * 365)
  )

  djpr_async_server(
    id = "ind_partrate_un_scatter",
    plot_fun = viz_ind_partrate_un_scatter,
    data = dash_data %>%
      dplyr::filter(series_id %in% c(
      "A84423355R",
      "A84423354L"
    )
    )
#    selected_period = reactive(input$ind_partrate_un_scatter_selected_period),
  )

  djpr_async_server(
    id = "ind_partrate_line",
    plot_func = viz_ind_partrate_line,
    data = dash_data %>%
      dplyr::filter(series_id = c(
      "A84423355R",
      "A84423051C"
    )
    )
#    date_slider_value_min = as.Date("2000-01-01"),
  )

  observeEvent(input$link_indicators, {
    updateNavbarPage(session, "navbarpage", "tab-indicators")
  })
}
