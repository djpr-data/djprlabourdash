# devtools::load_all()
library(shiny)
library(ggiraph)
library(tidyverse)

# dash_data <- load_dash_data()

data <- filter_dash_data("A84423354L")

df <- data %>%
    dplyr::slice_tail(n = 6)

p <- df %>%
  ggplot(aes(x = date, y = value, data_id = date)) +
  ggiraph::geom_col_interactive(fill = "#BCD3EF") +
  theme_void()

ui <- fluidPage(
  ggiraph::girafeOutput("plot"),
  textOutput("text"),
  uiOutput("checkboxes")
)

server <- function(input, output, session) {
  # reactive(print(input$opts_hover))
  observeEvent(input$plot_opts_hover, {
    print(Sys.time())
  })

  output$plot <- ggiraph::renderGirafe({

    ggiraph::girafe(
      ggobj = p,
      options = list(
        opts_hover(reactive = TRUE),
        opts_selection(type = "none")
      )
    )
  })

  output$text <- renderText({
    (input$plot_hovered)
    # "foobar"
  })

  observeEvent(input$plot_hovered, {
    print(input$plot_hovered)
  })

  output$checkboxes <- renderUI({
    checkboxGroupInput("hovered",
                  "hovered",
                  choices = unique(df$date))
  })

}

shinyApp(ui, server)
