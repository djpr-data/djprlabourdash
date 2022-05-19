#' Edit SMS before Sending
#'
#' @param labour data.frame used for testing, not required by addin
#'
#' @importFrom shinyWidgets switchInput updateSwitchInput
#' @import glue
#' @import readxl
#' @import purrr
#' @return
#' @export
#'
#' @examples
editEMAIL <- function() {
  stopifnot(grepl("djprlabourdash", rstudioapi::getActiveProject()))

  email_lists_url <- file.path(Sys.getenv()[["R_USER_HOME"]],
    "VicGov",
    "Economic Modelling Team - Documents",
    "Modelling research projects",
    "Labour market dashboard",
    "Jobs dashboard circulation list.xlsx",
    fsep = .Platform$file.sep
  )

  email_lists_sheets <- excel_sheets(email_lists_url)

  email_lists <- email_lists_sheets |>
    set_names() |>
    map(~ read_excel(email_lists_url, sheet = .x, col_names = FALSE) |>
      dplyr::mutate(email = ifelse(grepl("<", `...1`),
        stringr::str_extract(`...1`, "(?<=<).*(?=>)"),
        `...1`
      )))


  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Edit EMAIL"),
    miniUI::miniContentPanel(
      shiny::textInput("subject", "Subject Line",
        value = "OFFICIAL: DJPR Jobs Briefing: A record 64.3% of working age Victorians are employed ",
        width = "100%"
      ),
      shiny::passwordInput('password', 'DJPR Password', placeholder = 'for authentication to email server'),
      fluidRow(
        column(
          6,
          shinyWidgets::pickerInput("all_addresses", "Selected Email Addresses",
            choices = email_lists$To$email,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          shiny::textOutput("selected", container = h2)
        ),
        column(
          6,
          shiny::radioButtons("addresses_type",
            "Email Group",
            choices = c("To", "BCC", "Test"),
            selected = "To"
          ),
          shiny::radioButtons(
            "attach",
            "Select Attachments",
            choices = list.files(here::here("inst"), ".docx")
          )
        ),
      ),
      miniUI::miniButtonBlock(
        # shiny::actionButton('draft', 'Send Draft', value = FALSE, icon = icon('microscope')),
        shinyWidgets::switchInput("live", "Enable", value = FALSE, onStatus = "success", offStatus = "danger"),
        shiny::actionButton("preview", "Preview Email", value = FALSE, icon = icon("microscope")),
        shiny::actionButton("send", "Send Email", value = FALSE, icon = icon("paper-plane"))
      )
    )
  )

  server <- function(input, output, session) {
    to <- reactiveValues(
      recipient = email_lists$Test$email
    )

    observeEvent(input$addresses_type, {
      shinyWidgets::updatePickerInput(session = session, "all_addresses", choices = email_lists[[input$addresses_type]]$email)
      isolate(to$recipient <- email_lists$Test$email)
    })

    observeEvent(input$all_addresses, {
      output$selected <- renderText({
        glue::glue("selected {length(input$all_addresses)} of {nrow(email_lists[[input$addresses_type]])}")
      })
      isolate(to$recipient <- input$all_addresses)
    })




    observeEvent(input$live, {
      if (input$live) {
        isolate(to$recipient <- input$all_addresses)
      } else {
        isolate(to$recipient <- email_lists$Test$email)
      }

      message("selected recipients: ", to$recipient)
    })


    to_send <- reactive({

      # build email
      msg <- htmltools::tags$html(
        tags$head(includeHTML(here::here("inst/css.html"))),
        tags$body(
          lang = "EN-AU",
          link = "#0563C1",
          vlink = "purple",
          style = "tab-interval:36.0pt;word-wrap:break-word",
          tags$div(
            class = "WordSection1",
            # insert markdown content
            includeMarkdown(here::here("inst/message.md")),
            # insert signature
            includeHTML(here::here("inst/signature.html"))
          )
        )
      )

      message("message defined")

      print(to$recipient)

      email <- emayili::envelope(
        from = "spp-data@ecodev.vic.gov.au"
      ) |>
        emayili::subject(input$subject) |>
        emayili::html(as.character(msg)) |>
        emayili::attachment(path = here::here("inst/image001.jpg"), cid = "image1") |>
        emayili::attachment(path = here::here("inst/image002.png"), cid = "image2") |>
        emayili:::header_set("Label", "Official (DJPR)") |>
        emayili::attachment(here::here(
          "inst",
          input$attach[1]
        ))

      if (input$addresses_type == "BCC") {
        email <- email |>
          emayili::bcc(to$recipient)
      } else {
        email <- email |>
          emayili::to(to$recipient)
      }

      message("email ready")

      email
    })

    observeEvent(input$preview, {
      to$recipient

      preview <- to_send()

      tryCatch(
        {
          print(preview)
        },
        error = function(e) {
          shiny::showNotification("Preview FAILED", type = "error", duration = 5)
        }
      )
    })


    observeEvent(input$send, {
      message("smtp setup")

      to$recipient

      smtp <- emayili::server(
        host = "smtp.office365.com",
        port = 587,
        username = Sys.getenv()[["USEREMAIL"]],
        password = input$password
      )

      if (input$live | input$addresses_type == "Test") {
        message("sending...")
        # shiny::showNotification("sending SMTP commented out", type = 'message', duration = 5)
        tryCatch(
          {
            smtp(to_send(), verbose = TRUE)
            shiny::showNotification("Email Has Been Sent", type = "message", duration = 5)
          },
          error = function(e) {
            shiny::showNotification("Email FAILED", type = "error", duration = 5)
          }
        )

        rm(smtp)
      } else {
        shiny::showNotification("You can't send when active is OFF unless testing", type = "error", duration = 5)
      }
    })


    # return geometry to file and object in console
    observeEvent(input$done, {
      stopApp({})
    })
  }

  viewer <- shiny::paneViewer(500)
  shiny::runGadget(ui, server, viewer = viewer)
}
