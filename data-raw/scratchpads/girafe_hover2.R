
# UI -----
library(ggiraph)

ui <- shinyUI(fluidPage(
  fluidRow(
      checkboxInput(
        "opt_hover_data",
        label = "Reactive data hovering",
        value = T
      ),
      checkboxInput(
        "hover_inv",
        label = "Hovering over one data element, makes the rest semi-transparent",
        value = FALSE
      )
  ),
  fluidRow(column(width = 8,
                  girafeOutput("plot")),
           column(width = 4,
                  uiOutput("checkboxes")))
))

# Module -----
library(shiny)

StateHandlerUI <- function(id, label, choices) {
  ns <- NS(id)

  checkboxGroupInput(inputId = ns("chk"),
                     label = label,
                     choices = choices)
}

StateHandler <- function(input,
                         output,
                         session,
                         plotInput,
                         messageId) {
  plot_data <- reactive({
    v <- do.call(plotInput, list())
    if (is.null(v)) {
      character(0)
    } else
      v
  })

  chk_data <- debounce(reactive({
    if (is.null(input$chk)) {
      character(0)
    } else
      input$chk
  }), 200)

  ignore_plot_data <- FALSE

  ignore_chk_data <- FALSE

  observeEvent(plot_data(), {
      # ignore_chk_data  <<- TRUE
      updateCheckboxGroupInput(session,
                               'chk',
                               selected = plot_data())
  })

  observeEvent(chk_data(), {
      session$sendCustomMessage(type = messageId, message = chk_data())
  })

  return(list(plot_data, chk_data))
}

# Server ----
library(ggplot2)
library(ggiraph)
library(dplyr)

options(shiny.trace = FALSE)

dat <- data.frame(
  name = c("Christine", "Ginette", "David", "Cedric", "Frederic"),
  gender = c("Female", "Female", "Male", "Male", "Male"),
  height = c(169, 160, 171, 172, 171)
)

p <- ggplot(dat,
            aes(
              x = name,
              y = height,
              fill = gender,
              data_id = name,
              tooltip = name
            )) +
  geom_bar_interactive(stat = "identity") +
  scale_fill_manual_interactive(
    name = label_interactive("gender", tooltip = "Gender levels", data_id =
                               "legend.title"),
    values = c(Male = "#0072B2", Female = "#009E73"),
    data_id = function(breaks) {
      as.character(breaks)
    },
    tooltip = function(breaks) {
      as.character(breaks)
    },
    labels = function(breaks) {
      lapply(breaks, function(br) {
        label_interactive(as.character(br),
                          data_id = as.character(br),
                          tooltip = as.character(br))
      })
    }
  ) +
  theme_minimal()


server <- shinyServer(function(input, output, session) {
  output$plot <- renderGirafe({
    x <- girafe(ggobj = p,
                width_svg = 6,
                height_svg = 8)
    x <- girafe_options(
      x,
      opts_hover(reactive = input$opt_hover_data),
      opts_hover_inv(css = if (input$hover_inv == TRUE) "opacity:0.3" else "")
    )
    x
  })

  output$checkboxes <- renderUI({
    result <- list()

    if (input$opt_hover_data == TRUE) {
      result[[length(result) + 1]] <-
        StateHandlerUI(
          "hovered_data",
          label = "Hovered data elements:",
          choices = list("Christine", "Ginette", "David", "Cedric", "Frederic")
        )
    }

    result
  })

  # link_selection_default <- FALSE
  link_hover_default <- FALSE

  callModule(StateHandler,
             id = 'hovered_data',
             plotInput = reactive(input$plot_hovered),
             messageId = 'plot_hovered_set')


  observeEvent(input$link_hover, {
    link_hover_default <<- input$link_hover
  })



})

shiny::shinyApp(ui, server)
