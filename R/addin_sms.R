#' Edit SMS before Sending
#'
#' @param labour data.frame used for testing, not required by addin
#'
#' @importFrom shinyWidgets switchInput updateSwitchInput
#' @import glue
#' @return
#' @export
#'
#' @examples
editSMS <- function(labour = NULL) {
  stopifnot(grepl("djprlabourdash", rstudioapi::getActiveProject()))
  # shell("git checkout main")


  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Edit SMS"),
    miniUI::miniContentPanel(
      shiny::textInput("to", "Send To", value = Sys.getenv()[["USEREMAIL"]], width = "100%"),
      shinyWidgets::switchInput("live", "Enable", value = FALSE, onStatus = "success", offStatus = "danger"),
      shiny::textAreaInput("sms_text", "SMS Content", rows = 8, width = "550px", height = "100%"),
      miniUI::miniButtonBlock(
        shiny::div(shiny::actionButton("send", "Send SMS", value = FALSE, icon = icon("paper-plane")))
      )
    )
  )

  server <- function(input, output, session) {
    if (is.null(labour)) {

      # 2 Get data
      req_series <- c(
        `vic total employed`             = "A84423349V",
        `vic unemployment rate`          = "A84423354L",
        `regional vic unemployment rate` = "A84600079X"
      )

      recode_series <- names(req_series)
      names(recode_series) <- req_series

      labour <- readabs::read_abs_series(req_series, show_progress_bars = F) %>%
        mutate(
          series = recode(series_id, !!!recode_series),
          value  = ifelse(unit == "000", value * 1000, value),
        ) %>%
        arrange(date) %>%
        group_by(series) %>%
        mutate(
          value = ifelse(
            series == "regional vic unemployment rate",
            (value + lag(value) + lag(value, 2)) / 3,
            value
          )
        ) %>%
        ungroup() %>%
        select(date, series, value)
    }

    # build sms email
    sms_content <- sms(labour) |> paste(collapse = "\n")

    shiny::updateTextAreaInput(session = session, "sms_text", value = sms_content)

    smtp <- emayili::server(
      host = "smtp.office365.com",
      port = 587,
      username = Sys.getenv()[["USEREMAIL"]],
      password = keyring::key_get("user-passwd",
        username = Sys.getenv()[["USERNAME"]]
      )
    )

    to_send <- reactive({
      print("create email")

      send <- emayili::envelope(
        from = "spp-data@ecodev.vic.gov.au",
        to = input$to,
        subject = glue::glue(
          "LF SMS: {format(Sys.Date(), '%B-%Y')} ",
          "(not used by SMS Global, this is recommended for record keeping)"
        )
      ) |>
        emayili::text(
          .open = "{{", .close = "}}",
          "{sms}\r\n
                      {{gsub('\n','\r\n', input$sms_text)}}\r\n
                      {/sms}"
        )
      print("email ready")

      send
    })

    observeEvent(input$live, {
      if (input$live) {
        shinyWidgets::updateSwitchInput(session = session, "to", value = Sys.getenv()[["SMS_LABOURFORCE"]])
      } else {
        shinyWidgets::updateSwitchInput(session = session, "to", value = Sys.getenv()[["SMS_LABOURFORCE_TEST"]])
      }
    })

    observeEvent(input$send, {
      tryCatch(
        {
          smtp(to_send(), verbose = TRUE)
          shiny::showNotification("SMS Has Been Sent", type = "message", duration = 5)
        },
        error = function(e) {
          shiny::showNotification("SMS FAILED", type = "error", duration = 5)
        }
      )
    })


    # return geometry to file and object in console
    observeEvent(input$done, {
      stopApp({})
    })
  }

  viewer <- shiny::paneViewer(400)
  shiny::runGadget(ui, server, viewer = viewer)
}
