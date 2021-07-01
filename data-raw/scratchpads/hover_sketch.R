# devtools::load_all()
library(shiny)
library(ggiraph)
library(tidyverse)

# dash_data <- load_dash_data()

data <- filter_dash_data("A84423354L")

df <- data %>%
    dplyr::slice_tail(n = 12)

p <- df %>%
  ggplot(aes(x = as.character(date), y = value, data_id = as.character(date))) +
  ggiraph::geom_col_interactive(fill = "#BCD3EF") +
  theme_void()

ui <- fluidPage(
  fluidRow(
    column(8,
           ggiraph::girafeOutput("overview_ur_bar")),
    column(4,
           htmlOutput("text"))
  )
)

server <- function(input, output, session) {

  output$overview_ur_bar <- ggiraph::renderGirafe({

    ggiraph::girafe(
      ggobj = p,
      # Only the relative heights here matter
      width = 3,
      height = 1,
      options = list(
        opts_hover(reactive = TRUE,
                   css = girafe_css(
                     css = "fill:#2A6FA2"
                   )),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })

  output$text <- renderUI({
    tagList(
      HTML(
        paste0(
          span(style = "color: #2a6fa2",
               format(as.Date(hovered$last), "%B %Y"),
               br(),
               span(style = "font-size: 5rem; font-weight: 700",
                    paste0(format(
                      round2(df$value[df$date == hovered$last], 1),
                      # Show first decimal even for integers
                      nsmall = 1
                      ), "%")
               )
               ))),
      shiny::icon("arrow-circle-up", style = "font-size: 2.5rem; color: #2a6fa2"),
      # HTML(
        br(),
        "Unemployment rate",
        br()
    # )
    )
  })

  hovered <- reactiveValues(
    last = as.character(max(df$date))
    )

  session$onFlushed(function(){
    session$sendCustomMessage(type = "overview_ur_bar_hovered_set", message = max(df$date))
  })

  observeEvent(input$overview_ur_bar_hovered, {
    if (!is.null(input$overview_ur_bar_hovered)) {
      hovered$last <<- input$overview_ur_bar_hovered
    } else {
      session$sendCustomMessage(type = "overview_ur_bar_hovered_set", message = hovered$last)
    }
  },
  ignoreNULL = F)

  output$hover_ur <- renderText({
    df$value[df$date == hovered$last]
  })
}

shinyApp(ui, server)
