page_indicatorsUI <- function(...) {

  shiny::fluidRow(
    br(),
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
      djpr_async_ui("ind_emppop_state_line", width = 6),
      djpr_async_ui("ind_empgro_line", width = 6),
    ),

    fluidRow(
      djpr_async_ui("ind_gen_full_part_line", width = 6),
      djpr_async_ui("ind_emp_sincecovid_line", width = 6)
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


page_indicators <- function(input, output, session, plt_change, series_latestdates, footnote) {
  output$ind_empgrowth_sincecovid_text <- renderUI({
    text_active(
      paste(
        "There were XX million Victorians employed in XX, compared to XX million in XX.",
        "Employment changed by XX per cent over the year to XX,",
        "a",
        dplyr::case_when(
          get_summ("A84423349V", .data$ptile_d_year_abs) < 0.33 ~
            "relatively sluggish",
          get_summ("A84423349V", .data$ptile_d_year_abs) > 0.67 ~
            "relatively rapid",
          TRUE ~ "normal"
        ),
        "pace of growth for Victoria compared to historical trends.",
        "Over the past year, employment across Australia grew by XX per cent.",
        "Employment in Victoria is XX per cent",
        dplyr::if_else(sign(get_summ("A84423349V", .data$d_year_perc)) > 0,
          "above",
          "below"
        ),
        "its level from a year earlier."
      ),
      c(
        round2(get_summ("A84423349V", .data$latest_value) / 1000000, 3),
        get_summ("A84423349V", .data$latest_period),
        round2(get_summ("A84423349V", .data$prev_value) / 1000000, 3),
        format(get_summ("A84423349V", .data$prev_date), "%B"),
        round2(get_summ("A84423349V", .data$d_year_perc), 1),
        format(get_summ("A84423349V", .data$latest_date), "%B"),
        round2(get_summ("A84423043C", .data$d_year_perc), 1),
        round2(get_summ("A84423349V", .data$d_year_perc), 1)
      )
    )
  })


  djpr_plot_server(
    id = "ind_emp_sincecovid_line",
    plot_function = viz_ind_emp_sincecovid_line,
    date_slider = FALSE,
    width_percent = 45,
    height_percent = 70,
    data = filter_dash_data(c("A84423043C", "A84423349V")) %>%
      dplyr::filter(date >= as.Date("2020-01-01")),
    plt_change = plt_change
  )


  # Indicators: table of employment indicators
  output$ind_emp_table <- renderUI({
    table_ind_employment() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Indicators: line graph of emp-pop ratios in states, with state selector boxes
  djpr_plot_server("ind_emppop_state_line",
    viz_ind_emppop_state_line,
    data = filter_dash_data(c(
      "A84423272J",
      "A84423356T",
      "A84423286W",
      "A84423370L",
      "A84423328J",
      "A84423300F",
      "A84423314V",
      "A84423342C"
    ),
    df = dash_data
    ) %>%
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
    check_box_options = c(
      "Vic",
      "NSW",
      "SA",
      "QLD",
      "WA",
      "NT",
      "ACT",
      "Tas"
    ),
    check_box_var = .data$state,
    check_box_selected = c("NSW", "Vic"),
    date_slider = TRUE,
    date_slider_value_min = Sys.Date() - (365 * 5),
    width_percent = 100,
    height_percent = 70,
    plt_change = plt_change,
    non_filtered_latest = filter_dash_data(
      df = dash_data,
      series_ids = c(
        "A84423272J",
        "A84423356T",
        "A84423286W",
        "A84423370L",
        "A84423328J",
        "A84423300F",
        "A84423314V",
        "A84423342C"
      )
    ) %>%
      dplyr::filter(
        .data$date == max(.data$date),
        !(.data$state %in% c(
          "Northern Territory",
          "Australian Capital Territory"
        )
        )
      ) %>%
      dplyr::arrange(-.data$value)
  )

  # Indicators: line chart of annual employment growth in Vic & Aus

  djpr_plot_server("ind_empgro_line",
    viz_ind_empgro_line,
    data = filter_dash_data(c(
      "A84423349V",
      "A84423043C"
    )),
    date_slider_value_min = Sys.Date() - (365 * 5),
    plt_change = plt_change,
    height_percent = 70
  )

  # Indicators: cumulative change in PT / FT since COVID
  djpr_plot_server("ind_gen_full_part_line",
    plot_function = viz_ind_gen_full_part_line,
    data = filter_dash_data(c(
      "pt_emp_vic",
      "A84423357V"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2020-01-01")),
    plt_change = plt_change,
    width_percent = 45,
    height_percent = 70,
    date_slider = FALSE
  )

  # Indicators: unemployment ------
  output$ind_unemp_summary <- renderUI({
    table_ind_unemp_summary() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Indicators: line chart of Aus v Vic
  djpr_plot_server("ind_unemprate_line",
    viz_ind_unemprate_line,
    data = filter_dash_data(c(
      "A84423354L",
      "A84423050A"
    ),
    df = dash_data
    ),
    date_slider_value_min = as.Date("2000-01-01"),
    plt_change = plt_change
  )

  # Indicators: effective unemployment rate
  djpr_plot_server("ind_effective_unemprate_line",
    viz_ind_effective_unemprate_line,
    data = filter_dash_data(c(
      "A84423350C",
      "A84423351F",
      "A84423354L",
      "employed full-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
      "employed part-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
      "employed full-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria",
      "employed part-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2019-06-01")),
    plt_change = plt_change,
    width_percent = 100,
    height_percent = 70,
    date_slider = FALSE
  )

  # Indicators: table of unemployment rates by state
  output$table_ind_unemp_state <- renderUI({
    table_ind_unemp_state() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Indicators: dot plot of unemp rate by state
  djpr_plot_server("ind_unemp_states_dot",
    viz_ind_unemp_states_dot,
    data = filter_dash_data(
      c(
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
    ),
    date_slider = FALSE,
    plt_change = plt_change
  )

  djpr_plot_server("ind_underut_area",
    viz_ind_underut_area,
    data = filter_dash_data(c(
      "A85223450L",
      "A85223451R",
      "A84423354L"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (10 * 365),
    plt_change = plt_change,
    interactive = FALSE
  )

  # Indicators: hours worked ----

  djpr_plot_server("ind_hoursworked_line",
    viz_ind_hoursworked_line,
    data = filter_dash_data(c(
      "A84426256L",
      "A84426277X",
      "A84423689R",
      "A84423091W"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider_value_min = as.Date("2000-01-01")
  )

  # Indicators: participation ----
  djpr_plot_server("ind_partrate_bar",
    viz_ind_partrate_bar,
    data = filter_dash_data(c(
      "A84423355R",
      "A84423271F",
      "A84423369C",
      "A84423341A",
      "A84423327F",
      "A84423285V",
      "A84423313T",
      "A84423299J",
      "A84423051C"
    ),
    df = dash_data
    ),
    height_percent = 75,
    plt_change = plt_change,
    date_slider = FALSE,
    interactive = FALSE
  )

  djpr_plot_server("ind_partrate_un_line",
    viz_ind_partrate_un_line,
    data = filter_dash_data(c(
      "A84423355R",
      "A84423354L"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider_value_min = Sys.Date() - (10 * 365)
  )

  djpr_plot_server("ind_partrate_un_scatter",
    viz_ind_partrate_un_scatter,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84423355R",
      "A84423354L"
    ),
    df = dash_data
    ),
    selected_period = reactive(input$ind_partrate_un_scatter_selected_period),
    date_slider = FALSE
  )

  djpr_plot_server("ind_partrate_line",
    plot_function = viz_ind_partrate_line,
    data = filter_dash_data(c(
      "A84423355R",
      "A84423051C"
    ),
    df = dash_data
    ),
    date_slider_value_min = as.Date("2000-01-01"),
    plt_change = plt_change
  )

  observeEvent(input$link_indicators, {
    updateNavbarPage(session, "navbarpage", "tab-indicators")
  })
}
