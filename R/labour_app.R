#' @import djprshiny
#' @import djprdashdata
#' @import shiny

labour_server <- function(input, output, session) {
  dash_data <<- load_and_hide()

  ts_summ <<- dash_data %>%
    tidyr::unnest(cols = data) %>%
    djprshiny::ts_summarise()

  # Overview ------

  output$overview_text <- renderUI({
    text_overview_summary(ts_summ)
  })

  output$main_table <- reactable::renderReactable({
    req(dash_data)
    ids <- c(
      "A84423349V",
      "A84423356T",
      "A84423355R",
      "A84423354L",
      "A84423350C"
    )

    df <- filter_dash_data(ids, dash_data)

    overview_table(data = df)
  })

  # Indicators -----

  output$ind_empgrowth_sincecovid_text <- renderUI({
    text_active(
      paste(
        "There were XX Victorians employed in XX, up from XX in XX.",
        "Employment grew by XX per cent over the year to XX,",
        "a",
        dplyr::case_when(
          get_summ("A84423349V", ptile_d_year_abs) < 0.33 ~
          "relatively sluggish",
          get_summ("A84423349V", ptile_d_year_abs) > 0.67 ~
          "relatively rapid",
          TRUE ~ "normal"
        ),
        "pace of growth for Victoria compared to historical trends.",
        "Over the past year, employment across Australia grew by XX per cent.",
        "Employment in Victoria is XX per cent",
        dplyr::if_else(sign(get_summ("A84423349V", d_year_perc)) > 0,
          "above",
          "below"
        ),
        "its pre-COVID level."
      ),
      c(
        scales::comma(get_summ("A84423349V", latest_value)),
        get_summ("A84423349V", latest_period),
        scales::comma(get_summ("A84423349V", prev_value)),
        format(get_summ("A84423349V", prev_date), "%B"),
        get_summ("A84423349V", d_year_perc),
        format(get_summ("A84423349V", latest_date), "%B"),
        get_summ("A84423043C", d_year_perc),
        get_summ("A84423349V", d_year_perc)
      )
    )
  })

  djpr_plot_server(
    id = "ind_emp_sincecovid_line",
    plot_function = viz_ind_emp_sincecovid_line,
    date_slider = FALSE,
    data = filter_dash_data(c("A84423043C", "A84423349V")) %>%
      dplyr::filter(date >= as.Date("2020-01-01")),
    date_slider_value_min = as.Date("2020-01-01"),
    plt_change = reactive(input$plt_change)
  )

  # Indicators: dot point text of employment figures
  output$ind_emp_dotpoints <- renderUI({
    dp1 <- text_active(
      paste(
        "There were XX Victorians employed,",
        "of whom XX were in full-time work."
      ),
      c(
        scales::comma(get_summ("A84423349V", latest_value)),
        scales::comma(get_summ("A84423357V", latest_value))
      ),
      colour = djprtheme::djpr_pal(1)
    )

    dp2 <- text_active(
      paste(
        "Employment ",
        dplyr::if_else(get_summ("A84423349V", d_period_abs) > 0,
          "rose",
          "fell"
        ),
        "by XX people (XX per cent) in the month to XX",
        "and by XX people (XX per cent) over the year."
      ),
      c(
        scales::comma(get_summ("A84423349V", d_period_abs)),
        get_summ("A84423349V", d_period_perc),
        get_summ("A84423349V", latest_period),
        scales::comma(get_summ("A84423349V", d_year_abs)),
        get_summ("A84423349V", d_period_perc)
      ),
      colour = djprtheme::djpr_pal(1)
    )

    tags$div(
      tags$ul(
        tags$li(dp1),
        tags$li(dp2)
      )
    )
  })

  # Indicators: table of employment inducators
  output$ind_emp_table <- reactable::renderReactable({
    table_ids <- c(
      "A84423349V",
      "A84423357V",
      "A84423356T",
      "A84423244X",
      "A84423468K",
      "pt_emp_vic"
    )

    table_data <- filter_dash_data(table_ids)

    table_data <- table_data %>%
      mutate(indicator = if_else(sex != "",
        paste0(indicator, " (", sex, ")"),
        indicator
      ))

    overview_table(table_data)
  })

  # Links to pages -----
  observeEvent(input$link_indicators, {
    updateNavbarPage(session, "navbarpage", "tab-indicators")
  })

  observeEvent(input$link_overview, {
    updateNavbarPage(session, "navbarpage", "tab-overview")
  })

  observeEvent(input$link_regions, {
    updateNavbarPage(session, "navbarpage", "tab-regions")
  })

  observeEvent(input$link_groups, {
    updateNavbarPage(session, "navbarpage", "tab-groups")
  })

  observeEvent(input$link_industries, {
    updateNavbarPage(session, "navbarpage", "tab-industries")
  })

}

app <- function(...) {
  shiny::shinyApp(labour_ui(), labour_server)
}
