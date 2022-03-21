editSMS <- function(labour = NULL) {

  ui <- miniPage(
    gadgetTitleBar("Edit SMS"),
    miniContentPanel(
      textAreaInput('sms_text', 'SMS Content', rows = 8, width = '550px', height = '100%'),
      miniButtonBlock(
        div(shiny::actionButton('send', 'Send SMS', value = FALSE, icon = icon('paper-plane'))
      )
    )
  ))

  server <- function(input, output, session) {

    if (is.null(labour)){
      ref_dates <- reference_dates()

      ref_start <- ref_dates$dates$`Start of Reference Week`
      ref_end   <- ref_dates$dates$`End of Reference Week`

      # 2 Get data
      req_series <- c(
        `vic total employed`             = "A84423349V",
        `vic unemployment rate`          = "A84423354L",
        `regional vic unemployment rate` = "A84600079X"
      )

      recode_series <- names(req_series)
      names(recode_series) <- req_series

      labour <- read_abs_series(req_series, show_progress_bars = F) %>%
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
    sms_content <- sms(labour) |> paste(collapse = '\n')

    shiny::updateTextAreaInput(session = session, 'sms_text', value = sms_content)

    smtp <- emayili::server(
      host = "smtp.office365.com",
      port = 587,
      username = Sys.getenv()[['USEREMAIL']],
      password = keyring::key_get('user-passwd',
                                  username = Sys.getenv()[['USERNAME']])
    )

    to_send <- reactive({

      print('create email')

      send <- emayili::envelope(
        from = "spp-data@ecodev.vic.gov.au",
        #to = "labour-force@groups.smsglobal.com",
        to = "matthew.johnson@ecodev.vic.gov.au",
        subject = glue("LF SMS: {format(Sys.Date(), '%B-%Y')} ",
                       "(not used by SMS Global, this is recommended for record keeping)")) |>
        emayili::text(.open = '{{', .close = '}}',
                      "{sms}
                      {{input$sms_text}}
                      {/sms}")
      print('email ready')

      send

    })



    observeEvent(input$send, {

      tryCatch({
        smtp(to_send(), verbose = TRUE)
        showNotification("SMS Has Been Sent", type = 'message', duration = 5)
      },
       error = function(e){
         showNotification("SMS FAILED", type = 'error', duration = 5)
       })

    })


    # return geometry to file and object in console
    observeEvent(input$done, {

      stopApp({})

    })

  }

  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)

}
