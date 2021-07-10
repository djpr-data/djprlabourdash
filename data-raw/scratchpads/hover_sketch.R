# devtools::load_all()
library(shiny)
library(ggiraph)
library(tidyverse)

# dash_data <- load_dash_data()

ur_bar_data <- filter_dash_data("A84423354L")

ur_bar_static <- ur_bar_data %>%
  dplyr::slice_tail(n = 12) %>%
  ggplot(aes(x = as.character(date), y = value, data_id = as.character(date))) +
  ggiraph::geom_col_interactive(fill = "#BCD3EF") +
  theme_void()

ui <- fluidPage(
  fluidRow(
    # column(
    #   8,
      ggiraph::girafeOutput("overview_ur_bar"),
    # ),
    # column(
    #   4,
      htmlOutput("overview_ur_text")
    # )
  )
)

server <- function(input, output, session) {
  output$overview_ur_bar <- ggiraph::renderGirafe({
    ggiraph::girafe(
      ggobj = ur_bar_static,
      # Only the relative heights here matter
      width = 3,
      height = 1,
      options = list(
        opts_hover(
          reactive = TRUE,
          css = girafe_css(
            css = "fill: #2A6FA2; transition: 0.2s;"
          )
        ),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })

  output$overview_ur_text <- renderUI({
    selected_date <- as.Date(hovered$last)
    prev_date <- seq.Date(
      from = selected_date,
      length = 2,
      by = "-1 month"
    )[2]

    selected_val <- round2(ur_bar_data$value[ur_bar_data$date == hovered$last], 1)
    prev_val <- round2(ur_bar_data$value[ur_bar_data$date == prev_date], 1)
    change <- round2(selected_val - prev_val, 1)
    dir_change <- sign(change)

    change_arrow <- dplyr::case_when(
      dir_change == 1 ~
      "arrow-circle-up",
      dir_change == -1 ~
      "arrow-circle-down",
      dir_change == 0 ~
      "arrow-circle-right"
    )

    tagList(
      HTML(
        paste0(
          span(
            style = "color: #2a6fa2; font-size: 2rem; line-height: 200%",
            format(selected_date, "%B %Y"),
            br(),
            span(
              style = "font-size: 6rem; font-weight: 700",
              paste0(format(
                selected_val,
                # Show first decimal even for integers
                nsmall = 1
              ), "%")
            ),

          )
        )
      ),
      br(),
      span(
        style = "color: #2a6fa2; font-size: 2rem; font-weight: 400",
        "Unemployment rate"
      ),
      br(),
      shiny::icon(change_arrow, style = "font-size: 2.5rem; color: #2a6fa2"),
      span(
        style = "color: #2a6fa2; font-size: 3rem; font-weight: 400",
        abs(change), " pts"
      ),
      # HTML(
      br(),

      br()
      # )
    )
  })

  hovered <- reactiveValues(
    last = as.character(max(ur_bar_data$date))
  )

  session$onFlushed(function() {
    session$sendCustomMessage(type = "overview_ur_bar_hovered_set", message = max(ur_bar_data$date))
  })

  observeEvent(input$overview_ur_bar_hovered,
    {
      if (!is.null(input$overview_ur_bar_hovered)) {
        hovered$last <<- input$overview_ur_bar_hovered
      } else {
        session$sendCustomMessage(type = "overview_ur_bar_hovered_set", message = hovered$last)
      }
    },
    ignoreNULL = F
  )

  output$hover_ur <- renderText({
    ur_bar_data$value[ur_bar_data$date == hovered$last]
  })
}

shinyApp(ui, server)
