#' @import djprshiny
#' @import djprdashdata
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import djprtheme
#' @importFrom rlang .data .env

labour_server <- function(input, output, session) {
  # Load data and create persistent objects ----

  Sys.setenv("R_DJPRLABOURDASH_TABLEDEST" = "dashboard")
  dash_data <<- get_dash_data()
  dash_data_updated <<- attr(dash_data, "date_updated")
  if (shiny::isRunning()) {
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
  }

  series_latestdates <- dash_data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$date)

  plt_change <- reactive(input$plt_change) %>%
    debounce(2)

  # Overview ------

  # Overview: footnote and main table ----
  footnote <- reactive({
    req(dash_data)
    latest <- max(series_latestdates)
    div(
      shiny::HTML(
        paste0(
          "This dashboard is produced by the <b>Strategy and Priority ",
          "Projects - Data + Analytics</b> team at the Victorian Department ",
          "of Jobs, Precincts and Regions. The <b>latest data in this ",
          "dashboard is for ",
          format(latest, "%B %Y"),
          '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'
        )
      ),
      style = "color: #828282; font-size: 0.75rem"
    )
  })

  output$aboriginal_footnote <- output$vicregions_footnote <- output$disability_footnote <- output$migration_footnote <- output$overview_footnote <- output$indicators_footnote <- output$inclusion_footnote <- output$regions_footnote <- output$industries_footnote <- renderUI({
    footnote()
  })

  output$main_table <- renderUI({
    req(dash_data)
    table_overview() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Indicators -----

  # Indicators: Employment ----
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

  # Sex -----

  output$table_gr_sex <- renderUI({
    table_gr_sex() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Groups: line chart of emp-pop by sex
  djpr_plot_server("gr_gen_emppopratio_line",
    plot_function = viz_gr_gen_emppopratio_line,
    data = filter_dash_data(c(
      "A84423244X",
      "A84423468K"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 10),
    plt_change = plt_change
  )

  # Bar chart: LF status by sex, latest month

  djpr_plot_server("gr_gen_emp_bar",
    viz_gr_gen_emp_bar,
    date_slider = F,
    plt_change = plt_change,
    interactive = FALSE,
    data = filter_dash_data(c(
      "A84423469L",
      "A84423245A",
      "A84423801C",
      "A84423577W",
      "A84423461V",
      "A84423237A",
      "A84423463X",
      "A84423239F",
      "A84423462W",
      "A84423238C"
    ), df = dash_data) %>%
      dplyr::group_by(.data$series) %>%
      dplyr::filter(.data$date == max(.data$date))
  )

  # Line chart: participation by sex over time
  djpr_plot_server("gr_gen_partrate_line",
    viz_gr_gen_partrate_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84423355R",
      "A84423243W",
      "A84423467J"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 5)
  )

  # Line chart: unemployment rate by sex
  djpr_plot_server("gr_gen_unemp_line",
    viz_gr_gen_unemp_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84423354L",
      "A84423242V",
      "A84423466F"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 10)
  )

  djpr_plot_server("gr_full_part_line",
    viz_gr_full_part_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84423237A",
      "A84423461V",
      "A84423245A",
      "A84423469L"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 5)
  )

  # Jobactive by sex
  output$table_jobactive_female <- renderUI({
    table_jobactive_female() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_female_jobact_sincecovid_line",
    viz_gr_female_jobact_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_female_ballarat",
      "jobactive_female_bendigo",
      "jobactive_female_barwon",
      "jobactive_female_gippsland",
      "jobactive_female_goulburn/murray",
      "jobactive_female_inner metropolitan melbourne",
      "jobactive_female_north eastern melbourne",
      "jobactive_female_north western melbourne",
      "jobactive_female_south coast of victoria",
      "jobactive_female_south eastern melbourne and peninsula",
      "jobactive_female_western melbourne",
      "jobactive_female_wimmera mallee",
      "jobactive_total_ballarat",
      "jobactive_total_bendigo",
      "jobactive_total_barwon",
      "jobactive_total_gippsland",
      "jobactive_total_goulburn/murray",
      "jobactive_total_inner metropolitan melbourne",
      "jobactive_total_north eastern melbourne",
      "jobactive_total_north western melbourne",
      "jobactive_total_south coast of victoria",
      "jobactive_total_south eastern melbourne and peninsula",
      "jobactive_total_western melbourne",
      "jobactive_total_wimmera mallee"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31")),
    date_slider = FALSE
  )

  djpr_plot_server("gr_female_jobactive_bar",
    viz_gr_female_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_female_ballarat",
      "jobactive_female_bendigo",
      "jobactive_female_barwon",
      "jobactive_female_gippsland",
      "jobactive_female_goulburn/murray",
      "jobactive_female_inner metropolitan melbourne",
      "jobactive_female_north eastern melbourne",
      "jobactive_female_north western melbourne",
      "jobactive_female_south coast of victoria",
      "jobactive_female_south eastern melbourne and peninsula",
      "jobactive_female_western melbourne",
      "jobactive_female_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  # Age ----

  output$table_gr_youth_summary <- renderUI({
    table_gr_youth_summary() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Line chart indexed to COVID: employment by age
  djpr_plot_server("gr_yth_emp_sincecovid_line",
    viz_gr_yth_emp_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "15-24_greater melbourne_employed",
      "25-54_greater melbourne_employed",
      "55+_greater melbourne_employed",
      "15-24_rest of vic._employed",
      "25-54_rest of vic._employed",
      "55+_rest of vic._employed"
    ),
    df = dash_data
    ) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value, before = 11, complete = TRUE)) %>%
      dplyr::filter(.data$date >= as.Date("2020-01-01")),
    date_slider = FALSE,
    width_percent = 45
  )

  djpr_plot_server("gr_yth_lfpartrate_vicaus_line",
    viz_gr_yth_lfpartrate_vicaus_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84424622R",
      "A84424692W"
    ), df = dash_data) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value,
        before = 11, complete = TRUE
      )),
    width_percent = 45
  )

  # Age: youth focus box -----

  djpr_plot_server("gr_youth_states_dot",
    viz_gr_youth_states_dot,
    data = filter_dash_data(c(
      "A84433601W",
      "A84433602X",
      "A84433603A",
      "A84433505W",
      "A84433503T",
      "A84433504V",
      "A84433519K",
      "A84433517F",
      "A84433518J",
      "A84433533F",
      "A84433531A",
      "A84433532C",
      "A84433617R",
      "A84433615K",
      "A84433616L",
      "A84433575C",
      "A84433573X",
      "A84433574A",
      "A84433547V",
      "A84433545R",
      "A84433546T",
      "A84433589T",
      "A84433587L",
      "A84433588R",
      "A84433561R",
      "A84433559C",
      "A84433560L"
    ), df = dash_data),
    plt_change = plt_change,
    width_percent = 45,
    height_percent = 160,
    date_slider = FALSE,
    download_button = T,
    selected_indicator = reactive({
      input$youth_focus
    })
  )

  djpr_plot_server("gr_ages_line",
    viz_gr_ages_line,
    data = youth_focus_box_data(),
    plt_change = plt_change,
    width_percent = 47,
    height_percent = 50,
    date_slider = TRUE,
    date_slider_value_min = as.Date("2014-11-01"),
    download_button = T,
    selected_indicator = reactive({
      input$youth_focus
    })
  )

  djpr_plot_server("gr_yth_melbvrest_line",
    viz_gr_yth_melbvrest_line,
    data = filter_dash_data(
      c(
        "15-24_greater melbourne_employed",
        "15-24_rest of vic._employed",
        "15-24_greater melbourne_nilf",
        "15-24_rest of vic._nilf",
        "15-24_greater melbourne_unemployed",
        "15-24_rest of vic._unemployed"
      ),
      df = dash_data
    ),
    plt_change = plt_change,
    width_percent = 47,
    height_percent = 50,
    date_slider = TRUE,
    date_slider_value_min = as.Date("2014-11-01"),
    download_button = T,
    selected_indicator = reactive({
      input$youth_focus
    })
  )

  djpr_plot_server("gr_youth_vicaus_line",
    viz_gr_youth_vicaus_line,
    data = filter_dash_data(c(
      "A84433601W",
      "A84433602X",
      "A84433603A",
      "A84433505W",
      "A84433503T",
      "A84433504V",
      "A84433519K",
      "A84433517F",
      "A84433518J",
      "A84433533F",
      "A84433531A",
      "A84433532C",
      "A84433617R",
      "A84433615K",
      "A84433616L",
      "A84433575C",
      "A84433573X",
      "A84433574A",
      "A84433547V",
      "A84433545R",
      "A84433546T",
      "A84433589T",
      "A84433587L",
      "A84433588R",
      "A84433561R",
      "A84433559C",
      "A84433560L"
    ),
    df = dash_data
    ) %>%
      dplyr::mutate(
        state = dplyr::if_else(.data$state == "",
          "Aus",
          .data$state
        ),
        state = strayr::clean_state(.data$state)
      ),
    check_box_options = c(
      "Aus",
      "NSW",
      "Qld",
      "Tas",
      "ACT",
      "WA",
      "NT",
      "Vic",
      "SA"
    ),
    check_box_var = .data$state,
    check_box_selected = c("Aus", "Vic"),
    selected_indicator = reactive(input$youth_focus),
    plt_change = plt_change
  )

  djpr_plot_server("gr_youth_full_part_line",
    plot_function = viz_gr_youth_full_part_line,
    data = filter_dash_data(c(
      "A84424687C",
      "A84424695C",
      "A84424696F"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = TRUE
  )

  djpr_plot_server("gr_youth_eduemp_waterfall",
    plot_function = viz_gr_youth_eduemp_waterfall,
    data = filter_dash_data(c(
      "A84424598A",
      "A84424778K",
      "A84424597X",
      "A84424777J",
      "A84424600A",
      "A84424780W",
      "A84424694A"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    interactive = FALSE,
    date_slider = FALSE
  )

  djpr_plot_server("gr_yth_mostvuln_line",
    plot_function = viz_gr_yth_mostvuln_line,
    data = filter_dash_data(
      c(
        "A84424601C",
        "A84424781X"
      ),
      df = dash_data
    ),
    plt_change = plt_change,
    date_slider_value_min = Sys.Date() - (365.25 * 10),
    date_slider = TRUE
  )

  output$table_gr_youth_unemp_region <- renderUI({
    table_gr_youth_unemp_region() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Youth LF status by region focus box ----
  data_youth_map_bar_title <- reactive({
    data_youth_unemp_emppop_partrate_vic(selected_indicator = input$youth_region_focus)
  })

  output$title_youth_unemp_emppop_partrate_vic <- renderUI({
    title_youth_unemp_emppop_partrate_vic(
      data = data_youth_map_bar_title(),
      selected_indicator = input$youth_region_focus
    ) %>%
      djpr_plot_title()
  })

  output$map_youth_unemp_emppop_partrate_vic <- leaflet::renderLeaflet({
    map_youth_unemp_emppop_partrate_vic(
      data = data_youth_map_bar_title(),
      selected_indicator = input$youth_region_focus
    )
  })

  output$gr_youth_unemp_emppop_partrate_bar <- renderPlot({
    viz_gr_youth_unemp_emppop_partrate_bar(
      data = data_youth_map_bar_title(),
      selected_indicator = input$youth_region_focus
    )
  })


  # jobactive data by age

  output$table_jobactive_youth <- renderUI({
    table_jobactive_youth() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_youth_jobactive_bar",
    viz_gr_youth_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_youth (15-24)_ballarat",
      "jobactive_youth (15-24)_bendigo",
      "jobactive_youth (15-24)_barwon",
      "jobactive_youth (15-24)_gippsland",
      "jobactive_youth (15-24)_goulburn/murray",
      "jobactive_youth (15-24)_inner metropolitan melbourne",
      "jobactive_youth (15-24)_north eastern melbourne",
      "jobactive_youth (15-24)_north western melbourne",
      "jobactive_youth (15-24)_south coast of victoria",
      "jobactive_youth (15-24)_south eastern melbourne and peninsula",
      "jobactive_youth (15-24)_western melbourne",
      "jobactive_youth (15-24)_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  djpr_plot_server("gr_age_jobactive_since_covid_line",
    viz_gr_age_jobactive_since_covid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_youth (15-24)_ballarat",
      "jobactive_youth (15-24)_bendigo",
      "jobactive_youth (15-24)_barwon",
      "jobactive_youth (15-24)_gippsland",
      "jobactive_youth (15-24)_goulburn/murray",
      "jobactive_youth (15-24)_inner metropolitan melbourne",
      "jobactive_youth (15-24)_north eastern melbourne",
      "jobactive_youth (15-24)_north western melbourne",
      "jobactive_youth (15-24)_south coast of victoria",
      "jobactive_youth (15-24)_south eastern melbourne and peninsula",
      "jobactive_youth (15-24)_western melbourne",
      "jobactive_youth (15-24)_wimmera mallee",
      "jobactive_mature age (50+)_ballarat",
      "jobactive_mature age (50+)_bendigo",
      "jobactive_mature age (50+)_barwon",
      "jobactive_mature age (50+)_gippsland",
      "jobactive_mature age (50+)_goulburn/murray",
      "jobactive_mature age (50+)_inner metropolitan melbourne",
      "jobactive_mature age (50+)_north eastern melbourne",
      "jobactive_mature age (50+)_north western melbourne",
      "jobactive_mature age (50+)_south coast of victoria",
      "jobactive_mature age (50+)_south eastern melbourne and peninsula",
      "jobactive_mature age (50+)_western melbourne",
      "jobactive_mature age (50+)_wimmera mallee",
      "jobactive_total_ballarat",
      "jobactive_total_bendigo",
      "jobactive_total_barwon",
      "jobactive_total_gippsland",
      "jobactive_total_goulburn/murray",
      "jobactive_total_inner metropolitan melbourne",
      "jobactive_total_north eastern melbourne",
      "jobactive_total_north western melbourne",
      "jobactive_total_south coast of victoria",
      "jobactive_total_south eastern melbourne and peninsula",
      "jobactive_total_western melbourne",
      "jobactive_total_wimmera mallee"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31")),
    date_slider = FALSE
  )

  output$table_jobactive_mature_age <- renderUI({
    table_jobactive_mature_age() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_mature_age_jobactive_bar",
    viz_gr_mature_age_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_mature age (50+)_ballarat",
      "jobactive_mature age (50+)_bendigo",
      "jobactive_mature age (50+)_barwon",
      "jobactive_mature age (50+)_gippsland",
      "jobactive_mature age (50+)_goulburn/murray",
      "jobactive_mature age (50+)_inner metropolitan melbourne",
      "jobactive_mature age (50+)_north eastern melbourne",
      "jobactive_mature age (50+)_north western melbourne",
      "jobactive_mature age (50+)_south coast of victoria",
      "jobactive_mature age (50+)_south eastern melbourne and peninsula",
      "jobactive_mature age (50+)_western melbourne",
      "jobactive_mature age (50+)_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  # Long-term unemployment ------

  djpr_plot_server("gr_ltunemp_line",
    viz_gr_ltunemp_line,
    data = filter_dash_data(c(
      "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
      "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
      "A84423687K",
      "A84423089K",
      "A84597681W"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider_value_min = as.Date("2000-01-01")
  )

  djpr_plot_server("gr_ltunvic_bar",
    viz_gr_ltunvic_bar,
    data = filter_dash_data(c(
      "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
      "unemployed total ('000)_victoria_13 weeks and under 26 weeks (3-6 months)",
      "unemployed total ('000)_victoria_26 weeks and under 52 weeks (6-12 months)",
      "unemployed total ('000)_victoria_4 weeks and under 13 weeks (1-3 months)",
      "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
      "unemployed total ('000)_victoria_under 4 weeks (under 1 month)"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    interactive = FALSE,
    date_slider = FALSE
  )

  djpr_plot_server("gr_ltunvic_area",
    viz_gr_ltunvic_area,
    data = filter_dash_data(c(
      "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
      "unemployed total ('000)_victoria_13 weeks and under 26 weeks (3-6 months)",
      "unemployed total ('000)_victoria_26 weeks and under 52 weeks (6-12 months)",
      "unemployed total ('000)_victoria_4 weeks and under 13 weeks (1-3 months)",
      "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
      "unemployed total ('000)_victoria_under 4 weeks (under 1 month)"
    ),
    df = dash_data
    ),
    interactive = FALSE,
    plt_change = plt_change
  )

  # Aboriginal ------

  output$table_jobactive_aboriginal <- renderUI({
    table_jobactive_aboriginal() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_abor_jobactive_sincecovid_line",
    viz_gr_abor_jobactive_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_indigenous_ballarat",
      "jobactive_indigenous_bendigo",
      "jobactive_indigenous_barwon",
      "jobactive_indigenous_gippsland",
      "jobactive_indigenous_goulburn/murray",
      "jobactive_indigenous_inner metropolitan melbourne",
      "jobactive_indigenous_north eastern melbourne",
      "jobactive_indigenous_north western melbourne",
      "jobactive_indigenous_south coast of victoria",
      "jobactive_indigenous_south eastern melbourne and peninsula",
      "jobactive_indigenous_western melbourne",
      "jobactive_indigenous_wimmera mallee",
      "jobactive_total_ballarat",
      "jobactive_total_bendigo",
      "jobactive_total_barwon",
      "jobactive_total_gippsland",
      "jobactive_total_goulburn/murray",
      "jobactive_total_inner metropolitan melbourne",
      "jobactive_total_north eastern melbourne",
      "jobactive_total_north western melbourne",
      "jobactive_total_south coast of victoria",
      "jobactive_total_south eastern melbourne and peninsula",
      "jobactive_total_western melbourne",
      "jobactive_total_wimmera mallee"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31")),
    date_slider = FALSE
  )

  djpr_plot_server("gr_abor_jobactive_bar",
    viz_gr_abor_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_indigenous_ballarat",
      "jobactive_indigenous_bendigo",
      "jobactive_indigenous_barwon",
      "jobactive_indigenous_gippsland",
      "jobactive_indigenous_goulburn/murray",
      "jobactive_indigenous_inner metropolitan melbourne",
      "jobactive_indigenous_north eastern melbourne",
      "jobactive_indigenous_north western melbourne",
      "jobactive_indigenous_south coast of victoria",
      "jobactive_indigenous_south eastern melbourne and peninsula",
      "jobactive_indigenous_western melbourne",
      "jobactive_indigenous_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  # Disability ------

  output$table_jobactive_pwd <- renderUI({
    table_jobactive_pwd() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_pwd_jobact_sincecovid_line",
    viz_gr_pwd_jobact_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_pwd_ballarat",
      "jobactive_pwd_bendigo",
      "jobactive_pwd_barwon",
      "jobactive_pwd_gippsland",
      "jobactive_pwd_goulburn/murray",
      "jobactive_pwd_inner metropolitan melbourne",
      "jobactive_pwd_north eastern melbourne",
      "jobactive_pwd_north western melbourne",
      "jobactive_pwd_south coast of victoria",
      "jobactive_pwd_south eastern melbourne and peninsula",
      "jobactive_pwd_western melbourne",
      "jobactive_pwd_wimmera mallee",
      "jobactive_total_ballarat",
      "jobactive_total_bendigo",
      "jobactive_total_barwon",
      "jobactive_total_gippsland",
      "jobactive_total_goulburn/murray",
      "jobactive_total_inner metropolitan melbourne",
      "jobactive_total_north eastern melbourne",
      "jobactive_total_north western melbourne",
      "jobactive_total_south coast of victoria",
      "jobactive_total_south eastern melbourne and peninsula",
      "jobactive_total_western melbourne",
      "jobactive_total_wimmera mallee"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31")),
    date_slider = FALSE
  )

  djpr_plot_server("gr_pwd_jobactive_bar",
    viz_gr_pwd_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_pwd_ballarat",
      "jobactive_pwd_bendigo",
      "jobactive_pwd_barwon",
      "jobactive_pwd_gippsland",
      "jobactive_pwd_goulburn/murray",
      "jobactive_pwd_inner metropolitan melbourne",
      "jobactive_pwd_north eastern melbourne",
      "jobactive_pwd_north western melbourne",
      "jobactive_pwd_south coast of victoria",
      "jobactive_pwd_south eastern melbourne and peninsula",
      "jobactive_pwd_western melbourne",
      "jobactive_pwd_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  # Migration ------

  output$table_jobactive_refugees <- renderUI({
    table_jobactive_refugees() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_refugee_jobact_sincecovid_line",
    viz_gr_refugee_jobact_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_refugee_ballarat",
      "jobactive_refugee_bendigo",
      "jobactive_refugee_barwon",
      "jobactive_refugee_gippsland",
      "jobactive_refugee_goulburn/murray",
      "jobactive_refugee_inner metropolitan melbourne",
      "jobactive_refugee_north eastern melbourne",
      "jobactive_refugee_north western melbourne",
      "jobactive_refugee_south coast of victoria",
      "jobactive_refugee_south eastern melbourne and peninsula",
      "jobactive_refugee_western melbourne",
      "jobactive_refugee_wimmera mallee",
      "jobactive_total_ballarat",
      "jobactive_total_bendigo",
      "jobactive_total_barwon",
      "jobactive_total_gippsland",
      "jobactive_total_goulburn/murray",
      "jobactive_total_inner metropolitan melbourne",
      "jobactive_total_north eastern melbourne",
      "jobactive_total_north western melbourne",
      "jobactive_total_south coast of victoria",
      "jobactive_total_south eastern melbourne and peninsula",
      "jobactive_total_western melbourne",
      "jobactive_total_wimmera mallee"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31")),
    date_slider = FALSE
  )

  djpr_plot_server("gr_refugee_jobactive_bar",
    viz_gr_refugee_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_refugee_ballarat",
      "jobactive_refugee_bendigo",
      "jobactive_refugee_barwon",
      "jobactive_refugee_gippsland",
      "jobactive_refugee_goulburn/murray",
      "jobactive_refugee_inner metropolitan melbourne",
      "jobactive_refugee_north eastern melbourne",
      "jobactive_refugee_north western melbourne",
      "jobactive_refugee_south coast of victoria",
      "jobactive_refugee_south eastern melbourne and peninsula",
      "jobactive_refugee_western melbourne",
      "jobactive_refugee_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  # Regions ------

  # Victorian Regions -------

  djpr_plot_server("reg_melvic_line",
    viz_reg_melvic_line,
    plt_change = plt_change,
    date_slider_value_min = as.Date("2014-11-01"),
    data = filter_dash_data(c(
      "A84600144J",
      "A84600078W",
      "A84595516F",
      "A84595471L"
    ),
    df = dash_data
    ) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
      dplyr::filter(!is.na(.data$value))
  )


  output$caption_regions_data2 <- output$caption_regions_data1 <- renderUI({
    djpr_plot_caption(paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average."))
  })

  data_reg_map_bar_title <- reactive({
    data_reg_unemp_emppop_partrate_vic(selected_indicator = input$lf_status_region)
  })

  output$title_reg_unemp_emppop_partrate_vic <- renderText({
    title_reg_unemp_emppop_partrate_vic(data_reg_map_bar_title(),
      selected_indicator = input$lf_status_region
    )
  })

  output$subtitle_reg_unemp_emppop_partrate_vic <- renderText({
    indic_long <- dplyr::case_when(
      input$lf_status_region == "unemp_rate" ~
        "Unemployment rate",
      input$lf_status_region == "part_rate" ~
        "Participation rate",
      input$lf_status_region == "emp_pop" ~
        "Employment to population"
    )

    paste0(indic_long, " by region (SA4), per cent")
  })

  output$map_reg_unemp_emppop_partrate_vic <-
    leaflet::renderLeaflet({
      map_reg_unemp_emppop_partrate_vic(
        data = data_reg_map_bar_title(),
        selected_indicator = input$lf_status_region
      )
    })

  output$reg_unemp_emppop_partrate_bar <- renderPlot({
    viz_reg_unemp_emppop_partrate_bar(
      data = data_reg_map_bar_title(),
      selected_indicator = input$lf_status_region
    )
  })

  djpr_plot_server("reg_unemp_emppop_partrate_multiline",
    viz_reg_unemp_emppop_partrate_multiline,
    date_slider = TRUE,
    interactive = FALSE,
    height_percent = 125,
    data = filter_dash_data(c(
      "A84600253V",
      "A84599659L",
      "A84600019W",
      "A84600187J",
      "A84599557X",
      "A84600115W",
      "A84599851L",
      "A84599923L",
      "A84600025T",
      "A84600193C",
      "A84599665J",
      "A84600031L",
      "A84599671C",
      "A84599677T",
      "A84599683L",
      "A84599929A",
      "A84600121T",
      "A84600037A",
      "A84599658K",
      "A84599660W",
      "A84600018V",
      "A84600020F",
      "A84600186F",
      "A84600188K",
      "A84599556W",
      "A84599558A",
      "A84600114V",
      "A84600116X",
      "A84599850K",
      "A84599852R",
      "A84599922K",
      "A84599924R",
      "A84600024R",
      "A84600026V",
      "A84600192A",
      "A84600194F",
      "A84599664F",
      "A84599666K",
      "A84600030K",
      "A84600032R",
      "A84599670A",
      "A84599672F",
      "A84599676R",
      "A84599678V",
      "A84599682K",
      "A84599684R",
      "A84599928X",
      "A84599930K",
      "A84600120R",
      "A84600122V",
      "A84600036X",
      "A84600038C",
      "A84600252T",
      "A84600254W"
    ),
    df = dash_data
    ),
    selected_indicator = reactive(input$lf_status_multiline),
    date_slider_value_min = as.Date("2018-01-01"),
    plt_change = plt_change
  )

  # output$text_emp_regions <- renderUI({
  #   text_reg_regions_sincecovid()
  # })

  djpr_plot_server("reg_emp_regions_sincecovid_line",
    viz_reg_emp_regions_sincecovid_line,
    date_slider = FALSE,
    data = filter_dash_data(c(
      "A84600141A",
      "A84600075R"
    )) %>%
      dplyr::group_by(.data$series_id) %>%
      dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
      dplyr::filter(.data$date >= as.Date("2020-01-01")),
    plt_change = plt_change
  )

  djpr_plot_server("reg_unemprate_dispersion",
    viz_reg_unemprate_dispersion,
    interactive = FALSE,
    data = filter_dash_data(c(
      "A84600253V",
      "A84599659L",
      "A84600019W",
      "A84600187J",
      "A84599557X",
      "A84600115W",
      "A84599851L",
      "A84599923L",
      "A84600025T",
      "A84600193C",
      "A84599665J",
      "A84600031L",
      "A84599671C",
      "A84599677T",
      "A84599683L",
      "A84599929A",
      "A84600121T",
      "A84600037A"
    ),
    df = dash_data
    ),
    date_slider_value_min = as.Date("2014-11-01"),
    plt_change = plt_change,
    selected_indicator = reactive(input$sa4_type_dispersion)
  )

  # Regions: Focus box -----
  output$reg_sa4 <- renderPlot(
    {
      map_reg_sa4(sa4 = input$focus_region)
    },
    height = 350
  ) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  output$table_region_focus <- renderUI({
    table_region_focus(sa4 = input$focus_region) %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  reg_sa4unemp_cf_broadregion_withtitle <- reactive({
    viz_reg_sa4unemp_cf_broadregion(sa4 = input$focus_region)
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  output$reg_sa4unemp_cf_broadregion_title <- renderUI({
    djpr_plot_title(extract_labs(reg_sa4unemp_cf_broadregion_withtitle()))
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  output$reg_sa4unemp_cf_broadregion <- renderPlot({
    plot <- reg_sa4unemp_cf_broadregion_withtitle()
    plot$labels$title <- NULL
    plot
  }) %>%
    bindCache(
      input$focus_region,
      series_latestdates
    )

  # Regional JobActive caseload --------
  # data_reg_jobactive_map_bar_title <- data_reg_jobactive_vic()

  output$title_reg_jobactive_vic <- renderUI({
    title_reg_jobactive_vic(data = data_reg_jobactive_vic()) %>%
      djpr_plot_title()
  })

  output$map_reg_jobactive_vic <- leaflet::renderLeaflet({
    map_reg_jobactive_vic(data = data_reg_jobactive_vic())
  })

  output$reg_jobactive_vic_bar <- renderPlot({
    viz_reg_jobactive_vic_bar(data = data_reg_jobactive_vic())
  })

  output$table_jobactive_regions <- renderUI({
    table_jobactive_regions() %>%
      flextable::htmltools_value()
  })



  # Australian Regions -----------

  output$table_reg_nonmetro_states_unemprate <- renderUI({
    table_reg_nonmetro_states_unemprate() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  output$table_reg_metro_states_unemprate <- renderUI({
    table_reg_metro_states_unemprate() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Regions: National focus box -----
  djpr_plot_server("reg_regionstates_dot",
    plot_function = viz_reg_regionstates_dot,
    data = filter_dash_data(c(
      "A84599628W",
      "A84599629X",
      "A84599630J",
      "A84600078W",
      "A84600079X",
      "A84600080J",
      "A84599784X",
      "A84599785A",
      "A84599786C",
      "A84599718A",
      "A84599719C",
      "A84599720L",
      "A84600246W",
      "A84600247X",
      "A84600248A",
      "A84599634T",
      "A84599635V",
      "A84599636W",
      "A84599610X",
      "A84599611A",
      "A84599612C"
    ),
    df = dash_data
    ),
    selected_indicator = reactive(input$aus_regions_indicator),
    plt_change = plt_change,
    width_percent = 46,
    height_percent = 150,
    date_slider = FALSE
  )

  djpr_plot_server("reg_regionstates_bar",
    viz_reg_regionstates_bar,
    data = filter_dash_data(c(
      "15-24_employed_rest of nsw",
      "15-24_employed_rest of nt",
      "15-24_employed_rest of qld",
      "15-24_employed_rest of sa",
      "15-24_employed_rest of tas.",
      "15-24_employed_rest of vic.",
      "15-24_employed_rest of wa",
      "15-24_nilf_rest of nsw",
      "15-24_nilf_rest of nt",
      "15-24_nilf_rest of qld",
      "15-24_nilf_rest of sa",
      "15-24_nilf_rest of tas.",
      "15-24_nilf_rest of vic.",
      "15-24_nilf_rest of wa",
      "15-24_unemployed_rest of nsw",
      "15-24_unemployed_rest of nt",
      "15-24_unemployed_rest of qld",
      "15-24_unemployed_rest of sa",
      "15-24_unemployed_rest of tas.",
      "15-24_unemployed_rest of vic.",
      "15-24_unemployed_rest of wa",
      "25-54_employed_rest of nsw",
      "25-54_employed_rest of nt",
      "25-54_employed_rest of qld",
      "25-54_employed_rest of sa",
      "25-54_employed_rest of tas.",
      "25-54_employed_rest of vic.",
      "25-54_employed_rest of wa",
      "25-54_nilf_rest of nsw",
      "25-54_nilf_rest of nt",
      "25-54_nilf_rest of qld",
      "25-54_nilf_rest of sa",
      "25-54_nilf_rest of tas.",
      "25-54_nilf_rest of vic.",
      "25-54_nilf_rest of wa",
      "25-54_unemployed_rest of nsw",
      "25-54_unemployed_rest of nt",
      "25-54_unemployed_rest of qld",
      "25-54_unemployed_rest of sa",
      "25-54_unemployed_rest of tas.",
      "25-54_unemployed_rest of vic.",
      "25-54_unemployed_rest of wa",
      "55+_employed_rest of nsw",
      "55+_employed_rest of nt",
      "55+_employed_rest of qld",
      "55+_employed_rest of sa",
      "55+_employed_rest of tas.",
      "55+_employed_rest of vic.",
      "55+_employed_rest of wa",
      "55+_nilf_rest of nsw",
      "55+_nilf_rest of nt",
      "55+_nilf_rest of qld",
      "55+_nilf_rest of sa",
      "55+_nilf_rest of tas.",
      "55+_nilf_rest of vic.",
      "55+_nilf_rest of wa",
      "55+_unemployed_rest of nsw",
      "55+_unemployed_rest of nt",
      "55+_unemployed_rest of qld",
      "55+_unemployed_rest of sa",
      "55+_unemployed_rest of tas.",
      "55+_unemployed_rest of vic.",
      "55+_unemployed_rest of wa"
    ),
    df = dash_data
    ),
    selected_indicator = req(reactive(input$aus_regions_indicator)),
    plt_change = plt_change,
    height_percent = 150,
    width_percent = 46,
    interactive = FALSE,
    date_slider = FALSE
  )

  djpr_plot_server(
    "reg_emp_regionstates_sincecovid_line",
    viz_reg_emp_regionstates_sincecovid_line,
    data = filter_dash_data(c(
      "A84600075R",
      "A84599625R",
      "A84599781T",
      "A84599607K",
      "A84600243R",
      "A84599715V",
      "A84599631K"
    ),
    df = dash_data
    ) %>%
      dplyr::mutate(
        state = dplyr::case_when(
          .data$series == ">> Rest of Vic. ;  Employed total ;  Persons ;" ~
            "Reg. Vic",
          .data$series == ">> Rest of NSW ;  Employed total ;  Persons ;" ~
            "Reg. NSW",
          .data$series == ">> Rest of Qld ;  Employed total ;  Persons ;" ~
            "Reg. QLD",
          .data$series == ">>> Northern Territory - Outback ;  Employed total ;  Persons ;" ~
            "Reg. NT",
          .data$series == ">> Rest of WA ;  Employed total ;  Persons ;" ~
            "Reg. WA",
          .data$series == ">> Rest of SA ;  Employed total ;  Persons ;" ~
            "Reg. SA",
          .data$series == ">> Rest of Tas. ;  Employed total ;  Persons ;" ~
            "Reg. Tas",
          TRUE ~ .data$state
        )
      ),
    check_box_options = c(
      "Reg. Vic",
      "Reg. NSW",
      "Reg. QLD",
      "Reg. Tas",
      "Reg. WA",
      "Reg. NT",
      "Reg. SA"
    ),
    check_box_var = .data$state,
    check_box_selected = c("Reg. NSW", "Reg. Vic"),
    date_slider = FALSE,
    plt_change = plt_change
  )

  # Industries ------

  output$table_industries_summary <- renderUI({
    table_industries_summary() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("industries_empchange_sincecovid_bar",
    plot_function = viz_industries_empchange_sincecovid_bar,
    data = filter_dash_data(c(
      "A84601680F",
      "A84601683L",
      "A84601686V",
      "A84601665J",
      "A84601704L",
      "A84601707V",
      "A84601710J",
      "A84601638A",
      "A84601653X",
      "A84601689A",
      "A84601656F",
      "A84601713R",
      "A84601668R",
      "A84601695W",
      "A84601698C",
      "A84601650T",
      "A84601671C",
      "A84601641R",
      "A84601716W",
      "A84601662A"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE
  )

  djpr_plot_server("industries_emp_line",
    plot_function = viz_industries_emp_line,
    data = filter_dash_data(c(
      "A84601680F",
      "A84601683L",
      "A84601686V",
      "A84601665J",
      "A84601704L",
      "A84601707V",
      "A84601710J",
      "A84601638A",
      "A84601653X",
      "A84601689A",
      "A84601656F",
      "A84601713R",
      "A84601668R",
      "A84601695W",
      "A84601698C",
      "A84601650T",
      "A84601671C",
      "A84601641R",
      "A84601716W",
      "A84601662A"
    ),
    df = dash_data
    ),
    chosen_industry = reactive({
      input$chosen_industry
    }),
    plt_change = plt_change,
    date_slider = TRUE,
    date_slider_value_min = Sys.Date() - (365 * 10),
    width_percent = 45,
    height_percent = 75
  )

  djpr_plot_server("industries_emp_bysex_bar",
    plot_function = viz_industries_emp_bysex_bar,
    data = filter_dash_data(c(
      "females_greater melbourne_accommodation and food services_employed full-time",
      "females_greater melbourne_administrative and support services_employed full-time",
      "females_greater melbourne_agriculture, forestry and fishing_employed full-time",
      "females_greater melbourne_arts and recreation services_employed full-time",
      "females_greater melbourne_construction_employed full-time",
      "females_greater melbourne_education and training_employed full-time",
      "females_greater melbourne_electricity, gas, water and waste services_employed full-time",
      "females_greater melbourne_financial and insurance services_employed full-time",
      "females_greater melbourne_health care and social assistance_employed full-time",
      "females_greater melbourne_information media and telecommunications_employed full-time",
      "females_greater melbourne_manufacturing_employed full-time",
      "females_greater melbourne_mining_employed full-time",
      "females_greater melbourne_other services_employed full-time",
      "females_greater melbourne_professional, scientific and technical services_employed full-time",
      "females_greater melbourne_public administration and safety_employed full-time",
      "females_greater melbourne_rental, hiring and real estate services_employed full-time",
      "females_greater melbourne_retail trade_employed full-time",
      "females_greater melbourne_transport, postal and warehousing_employed full-time",
      "females_greater melbourne_wholesale trade_employed full-time",
      "males_greater melbourne_accommodation and food services_employed full-time",
      "males_greater melbourne_administrative and support services_employed full-time",
      "males_greater melbourne_agriculture, forestry and fishing_employed full-time",
      "males_greater melbourne_arts and recreation services_employed full-time",
      "males_greater melbourne_construction_employed full-time",
      "males_greater melbourne_education and training_employed full-time",
      "males_greater melbourne_electricity, gas, water and waste services_employed full-time",
      "males_greater melbourne_financial and insurance services_employed full-time",
      "males_greater melbourne_health care and social assistance_employed full-time",
      "males_greater melbourne_information media and telecommunications_employed full-time",
      "males_greater melbourne_manufacturing_employed full-time",
      "males_greater melbourne_mining_employed full-time",
      "males_greater melbourne_other services_employed full-time",
      "males_greater melbourne_professional, scientific and technical services_employed full-time",
      "males_greater melbourne_public administration and safety_employed full-time",
      "males_greater melbourne_rental, hiring and real estate services_employed full-time",
      "males_greater melbourne_retail trade_employed full-time",
      "males_greater melbourne_transport, postal and warehousing_employed full-time",
      "males_greater melbourne_wholesale trade_employed full-time",
      "females_rest of vic._accommodation and food services_employed full-time",
      "females_rest of vic._administrative and support services_employed full-time",
      "females_rest of vic._agriculture, forestry and fishing_employed full-time",
      "females_rest of vic._arts and recreation services_employed full-time",
      "females_rest of vic._construction_employed full-time",
      "females_rest of vic._education and training_employed full-time",
      "females_rest of vic._electricity, gas, water and waste services_employed full-time",
      "females_rest of vic._financial and insurance services_employed full-time",
      "females_rest of vic._health care and social assistance_employed full-time",
      "females_rest of vic._information media and telecommunications_employed full-time",
      "females_rest of vic._manufacturing_employed full-time",
      "females_rest of vic._mining_employed full-time",
      "females_rest of vic._other services_employed full-time",
      "females_rest of vic._professional, scientific and technical services_employed full-time",
      "females_rest of vic._public administration and safety_employed full-time",
      "females_rest of vic._rental, hiring and real estate services_employed full-time",
      "females_rest of vic._retail trade_employed full-time",
      "females_rest of vic._transport, postal and warehousing_employed full-time",
      "females_rest of vic._wholesale trade_employed full-time",
      "males_rest of vic._accommodation and food services_employed full-time",
      "males_rest of vic._administrative and support services_employed full-time",
      "males_rest of vic._agriculture, forestry and fishing_employed full-time",
      "males_rest of vic._arts and recreation services_employed full-time",
      "males_rest of vic._construction_employed full-time",
      "males_rest of vic._education and training_employed full-time",
      "males_rest of vic._electricity, gas, water and waste services_employed full-time",
      "males_rest of vic._financial and insurance services_employed full-time",
      "males_rest of vic._health care and social assistance_employed full-time",
      "males_rest of vic._information media and telecommunications_employed full-time",
      "males_rest of vic._manufacturing_employed full-time",
      "males_rest of vic._mining_employed full-time",
      "males_rest of vic._other services_employed full-time",
      "males_rest of vic._professional, scientific and technical services_employed full-time",
      "males_rest of vic._public administration and safety_employed full-time",
      "males_rest of vic._rental, hiring and real estate services_employed full-time",
      "males_rest of vic._retail trade_employed full-time",
      "males_rest of vic._transport, postal and warehousing_employed full-time",
      "males_rest of vic._wholesale trade_employed full-time",
      "females_greater melbourne_accommodation and food services_employed part-time",
      "females_greater melbourne_administrative and support services_employed part-time",
      "females_greater melbourne_agriculture, forestry and fishing_employed part-time",
      "females_greater melbourne_arts and recreation services_employed part-time",
      "females_greater melbourne_construction_employed part-time",
      "females_greater melbourne_education and training_employed part-time",
      "females_greater melbourne_electricity, gas, water and waste services_employed part-time",
      "females_greater melbourne_financial and insurance services_employed part-time",
      "females_greater melbourne_health care and social assistance_employed part-time",
      "females_greater melbourne_information media and telecommunications_employed part-time",
      "females_greater melbourne_manufacturing_employed part-time",
      "females_greater melbourne_mining_employed part-time",
      "females_greater melbourne_other services_employed part-time",
      "females_greater melbourne_professional, scientific and technical services_employed part-time",
      "females_greater melbourne_public administration and safety_employed part-time",
      "females_greater melbourne_rental, hiring and real estate services_employed part-time",
      "females_greater melbourne_retail trade_employed part-time",
      "females_greater melbourne_transport, postal and warehousing_employed part-time",
      "females_greater melbourne_wholesale trade_employed part-time",
      "males_greater melbourne_accommodation and food services_employed part-time",
      "males_greater melbourne_administrative and support services_employed part-time",
      "males_greater melbourne_agriculture, forestry and fishing_employed part-time",
      "males_greater melbourne_arts and recreation services_employed part-time",
      "males_greater melbourne_construction_employed part-time",
      "males_greater melbourne_education and training_employed part-time",
      "males_greater melbourne_electricity, gas, water and waste services_employed part-time",
      "males_greater melbourne_financial and insurance services_employed part-time",
      "males_greater melbourne_health care and social assistance_employed part-time",
      "males_greater melbourne_information media and telecommunications_employed part-time",
      "males_greater melbourne_manufacturing_employed part-time",
      "males_greater melbourne_mining_employed part-time",
      "males_greater melbourne_other services_employed part-time",
      "males_greater melbourne_professional, scientific and technical services_employed part-time",
      "males_greater melbourne_public administration and safety_employed part-time",
      "males_greater melbourne_rental, hiring and real estate services_employed part-time",
      "males_greater melbourne_retail trade_employed part-time",
      "males_greater melbourne_transport, postal and warehousing_employed part-time",
      "males_greater melbourne_wholesale trade_employed part-time",
      "females_rest of vic._accommodation and food services_employed part-time",
      "females_rest of vic._administrative and support services_employed part-time",
      "females_rest of vic._agriculture, forestry and fishing_employed part-time",
      "females_rest of vic._arts and recreation services_employed part-time",
      "females_rest of vic._construction_employed part-time",
      "females_rest of vic._education and training_employed part-time",
      "females_rest of vic._electricity, gas, water and waste services_employed part-time",
      "females_rest of vic._financial and insurance services_employed part-time",
      "females_rest of vic._health care and social assistance_employed part-time",
      "females_rest of vic._information media and telecommunications_employed part-time",
      "females_rest of vic._manufacturing_employed part-time",
      "females_rest of vic._mining_employed part-time",
      "females_rest of vic._other services_employed part-time",
      "females_rest of vic._professional, scientific and technical services_employed part-time",
      "females_rest of vic._public administration and safety_employed part-time",
      "females_rest of vic._rental, hiring and real estate services_employed part-time",
      "females_rest of vic._retail trade_employed part-time",
      "females_rest of vic._transport, postal and warehousing_employed part-time",
      "females_rest of vic._wholesale trade_employed part-time",
      "males_rest of vic._accommodation and food services_employed part-time",
      "males_rest of vic._administrative and support services_employed part-time",
      "males_rest of vic._agriculture, forestry and fishing_employed part-time",
      "males_rest of vic._arts and recreation services_employed part-time",
      "males_rest of vic._construction_employed part-time",
      "males_rest of vic._education and training_employed part-time",
      "males_rest of vic._electricity, gas, water and waste services_employed part-time",
      "males_rest of vic._financial and insurance services_employed part-time",
      "males_rest of vic._health care and social assistance_employed part-time",
      "males_rest of vic._information media and telecommunications_employed part-time",
      "males_rest of vic._manufacturing_employed part-time",
      "males_rest of vic._mining_employed part-time",
      "males_rest of vic._other services_employed part-time",
      "males_rest of vic._professional, scientific and technical services_employed part-time",
      "males_rest of vic._public administration and safety_employed part-time",
      "males_rest of vic._rental, hiring and real estate services_employed part-time",
      "males_rest of vic._retail trade_employed part-time",
      "males_rest of vic._transport, postal and warehousing_employed part-time",
      "males_rest of vic._wholesale trade_employed part-time"
    ), df = dash_data) %>%
      dplyr::group_by(.data$series) %>%
      dplyr::filter(.data$date == max(.data$date)) %>%
      dplyr::ungroup(),
    plt_change = plt_change,
    chosen_industry = reactive({
      input$chosen_industry
    }),
    date_slider = FALSE,
    height_percent = 65,
    width_percent = 100
  )

  output$industries_employment <- renderUI({
    table_industries_employment(chosen_industry = input$chosen_industry) %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(
      series_latestdates,
      input$chosen_industry
    )

  # Links to pages -----
  observeEvent(input$link_indicators, {
    updateNavbarPage(session, "navbarpage", "tab-indicators")
  })

  observeEvent(input$link_overview, {
    updateNavbarPage(session, "navbarpage", "tab-overview")
  })

  observeEvent(input$link_sex, {
    updateNavbarPage(session, "navbarpage", "tab-sex")
  })

  observeEvent(input$link_age, {
    updateNavbarPage(session, "navbarpage", "tab-age")
  })

  observeEvent(input$link_ltunemp, {
    updateNavbarPage(session, "navbarpage", "tab-ltunemp")
  })

  observeEvent(input$link_vicregions, {
    updateNavbarPage(session, "navbarpage", "tab-vicregions")
  })

  observeEvent(input$link_ausregions, {
    updateNavbarPage(session, "navbarpage", "tab-ausregions")
  })

  observeEvent(input$link_industries, {
    updateNavbarPage(session, "navbarpage", "tab-industries")
  })
}

app <- function(...) {
  djprtheme::djpr_use_fonts()
  flextable::set_flextable_defaults(
    font.family = getOption("djprtheme.base_font_family")
  )

  jobs_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shinyOptions(
    cache = jobs_dash_cache
  )

  shiny::shinyApp(labour_ui(), labour_server)
}
