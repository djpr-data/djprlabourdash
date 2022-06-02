
page_ageUI <- function(...) {
  shiny::fluidRow(

    column_nopad(
      width = 4,
      djpr_h1_box("Age"),
      box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "ABS Labour force data disaggregated by age can be volatile, and most of this data",
        " is not seasonally adjusted. DJPR smooths the data ",
        "by using 12-month rolling averages. While this assists in removing noise",
        " to focus on the underlying trends, it makes large month-to-month changes ",
        "in underlying conditions less apparent."
      )
    ),

    box(
      width = 8,
      uiOutput("table_gr_youth_summary") %>%
        djpr_with_spinner()
    ),

    djpr_async_ui("gr_yth_emp_sincecovid_line", width = 6, height = '385px'),

    djpr_async_ui(
      id = "gr_yth_lfpartrate_vicaus_line",
      width = 6,
      height = '385px',
      date_slider(
        id = "gr_yth_lfpartrate_vicaus_line",
        table_no = "6202016"
      )
    ),

    focus_box(
      title = h2("Labour force status of Victorian youth"),
      inputs = shiny::selectInput("youth_focus",
                         label = tags$span(style="color: white;","Select an indicator"),
                        choices = c(
                          "Unemployment rate" = "unemp_rate",
                          "Participation rate" = "part_rate",
                          "Employment-to-population ratio" = "emp_pop"
                        ),
                        width = "100%"
      ),
      fluidRow(
        column(6,
               djpr_async_ui("gr_youth_states_dot",
                             height = "735px",
                             width = 12) %>%
                 async_no_background()
               ),
        column(6,
               fluidRow(
                 djpr_async_ui(
                   "gr_ages_line",
                   height = "250px",
                   width = 12,
                   date_slider(
                     "gr_ages_line",
                     table_no = "LM1",
                     value = c(as.Date("2014-11-01"), data_dates$LM1$max)
                   )
                  ) %>%
                   async_no_background()
               ),
               fluidRow(
                 djpr_async_ui(
                   "gr_yth_melbvrest_line",
                   height = "200px",
                   width = 12,
                   date_slider(
                     "gr_yth_melbvrest_line",
                     table_no = "LM1",
                     value = c(as.Date("2014-11-01"), data_dates$LM1$max)
                   )
                  ) %>%
                   async_no_background()
               ))),

      fluidRow(
        djpr_async_ui(
          "gr_youth_vicaus_line",
          width = 12,
          fluidRow(
            column(6, date_slider("gr_youth_vicaus_line", table_no = "6291003")),
            column(6, state_checkbox(id = 'gr_youth_vicaus_line'))
          )
        ) %>%
          async_no_background()
      )
    ),


    djpr_h2_box("Detailed labour force status of Victorian youth"),

    djpr_async_ui("gr_youth_full_part_line",
                  date_slider('gr_youth_full_part_line', table_no = '6202016')),

    djpr_async_ui("gr_youth_eduemp_waterfall"),

    djpr_async_ui("gr_yth_mostvuln_line", width = 12,
                    date_slider('gr_yth_mostvuln_line', table_no = '6202016')),

    djpr_h2_box("Youth unemployment rate by region"),

    box(
      width = 12,
      uiOutput("table_gr_youth_unemp_region") %>% djpr_with_spinner()
    ),

    focus_box(
      h2(textOutput("title_youth_unemp_emppop_partrate_vic")),
      selectInput("youth_region_focus",
                  label = tags$span(style="color: white;","Select an indicator"),
        choices = c(
          "Unemployment rate" = "unemp_rate",
          "Participation rate" = "part_rate",
          "Employment-to-population ratio" = "emp_pop"
        ),
        width = "100%"
      ),
      fluidRow(
        column(6,
          leaflet::leafletOutput("map_youth_unemp_emppop_partrate_vic") %>% djpr_with_spinner()
        ),
        column(6,
          plotOutput("gr_youth_unemp_emppop_partrate_bar")
      ))
    ),

    djpr_h2_box("Jobactive caseload for 15-24 year olds (youth)"),

    box(
      width = 12,
      uiOutput("table_jobactive_youth") %>% djpr_with_spinner()
    ),

    djpr_async_ui("gr_youth_jobactive_bar", width = 12),

    djpr_h2_box("Jobactive caseload for 50+ (mature age)"),

    box(
      width = 12,
      uiOutput("table_jobactive_mature_age") %>% djpr_with_spinner()
    ),

    djpr_async_ui("gr_age_jobactive_since_covid_line"),
    djpr_async_ui("gr_mature_age_jobactive_bar"),

    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        "This dashboard is produced by the <b>Strategy and Priority ",
        "Projects - Data + Analytics</b> team at the Victorian Department ",
        "of Jobs, Precincts and Regions. The <b>latest data in this ",
        "dashboard is for ",  format(data_dates$`6202012`$max, "%B %Y"),
        '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'

      )
    )

  )
}

page_age <- function(input, output, session) {
  output$table_gr_youth_summary <- renderUI({
    table_gr_youth_summary() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(data_dates$`LM1`$max)

  # Line chart indexed to COVID: employment by age
  djpr_async_server(
    id       = "gr_yth_emp_sincecovid_line",
    plot_fun = viz_gr_yth_emp_sincecovid_line
  )

  djpr_async_server(
    id         =  "gr_yth_lfpartrate_vicaus_line",
    plot_fun   =  viz_gr_yth_lfpartrate_vicaus_line,
    date_range = input$dates
  )

  # Age: youth focus box -----

  djpr_async_server(
    id       = "gr_youth_states_dot",
    plot_fun = viz_gr_youth_states_dot,
    # download_button = TRUE,
    input_from_server = list(selected_indicator = reactive(input$youth_focus))
  )




  djpr_async_server(
    id        =  "gr_ages_line",
    plot_fun  =  viz_gr_ages_line,
    date_range = input$dates,
    input_from_server = list(selected_indicator = reactive(input$youth_focus))
  )




  djpr_async_server(
    id        =  "gr_yth_melbvrest_line",
    plot_fun  =  viz_gr_yth_melbvrest_line,
    date_range = input$dates,
    input_from_server = list(selected_indicator = reactive(input$youth_focus))
  )


  djpr_async_server(
    id        =  "gr_youth_vicaus_line",
    plot_fun  =  viz_gr_youth_vicaus_line,
    state = input$states,
    date_range = input$dates,
    input_from_server = list(selected_indicator = reactive(input$youth_focus))

  )





  djpr_async_server(
    id        =  "gr_youth_full_part_line",
    plot_fun  =  viz_gr_youth_full_part_line,
    date_range = input$dates
  )


  djpr_async_server(
    id        =  "gr_youth_eduemp_waterfall",
    plot_fun  =  viz_gr_youth_eduemp_waterfall
  )

  djpr_async_server(
    id        =  "gr_yth_mostvuln_line",
    plot_fun  =  viz_gr_yth_mostvuln_line,
    date_range = input$dates
  )

  output$table_gr_youth_unemp_region <- renderUI({
    table_gr_youth_unemp_region() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(data_dates$RM1$max)

  # Youth LF status by region focus box ----
  data_youth_map_bar_title <- reactive({
    data_youth_unemp_emppop_partrate_vic(selected_indicator = input$youth_region_focus)
  })

  output$title_youth_unemp_emppop_partrate_vic <- renderText({
    title_youth_unemp_emppop_partrate_vic(
      data = data_youth_map_bar_title(),
      selected_indicator = input$youth_region_focus
    )
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

  djpr_async_server(
    id        =  "gr_youth_jobactive_bar",
    plot_fun  =  viz_gr_youth_jobactive_bar
  )

  djpr_async_server(
    id    =  "gr_age_jobactive_since_covid_line",
    plot_fun  =  viz_gr_age_jobactive_since_covid_line
  )

  output$table_jobactive_mature_age <- renderUI({
    table_jobactive_mature_age() %>%
      flextable::htmltools_value()
  })

  djpr_async_server(
    id        =  "gr_mature_age_jobactive_bar",
    plot_fun  =  viz_gr_mature_age_jobactive_bar
  )

}
