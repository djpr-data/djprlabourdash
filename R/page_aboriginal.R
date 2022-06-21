page_aboriginalUI <- function(...) {
  shiny::fluidPage(

    # header row & table
    fluidRow(

      column(
        width = 4,
        djprshiny::djpr_h1_box("Aboriginal Victorians") %>% fluidRow(),
        shinydashboard::box(
          width = 12,
          style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
          "This page contains the number of Aboriginal people assisted through the jobactive program. ",
          "The jobactive program is a Commonwealth program designed to provide services to the majority of job seekers."
        ) %>% fluidRow()
      ),

      box(
        width = 8,
        uiOutput("table_jobactive_aboriginal") %>%
          djpr_with_spinner()
      )
    ),


    fluidRow(
      djpr_box_ui("gr_abor_jobactive_sincecovid_line", width = 6),
      djpr_box_ui("gr_abor_jobactive_bar", width = 6)
    ),
    height_sync("gr_abor_jobactive_sincecovid_line", "gr_abor_jobactive_bar"),

    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        "The latest data in this ",
        "dashboard is for ", format(data_dates$`6202012`$max, "%B %Y"), '.',
        "We are committed to making our websites accessible to all users.",
        "We are aware that parts of these dashboards are not fully accessible.",
        "If you require this information in an alternative format or would",
        "like to provide feedback please ",
        "<a href='mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard'>email us</a>.",
        "</br>"
      ),
      div(
        style = "text-align: center;",
        tags$a(
          class = "legalLink",
          href = "#shiny-tab-legal",
          "Copyright | Disclaimer"
        )
      )
    ) %>% fluidRow()
  )
}


page_aboriginal <- function(input, output, session) {
  output$table_jobactive_aboriginal <- renderUI({
    table_jobactive_aboriginal() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  })

  djpr_box_server(
    "gr_abor_jobactive_sincecovid_line",
    viz_gr_abor_jobactive_sincecovid_line
  )

  djpr_box_server(
    "gr_abor_jobactive_bar",
    viz_gr_abor_jobactive_bar
  )
}
