#' @rdname tables
#' Produce tables for LFS dashboard and associated briefing materials
#' @param data A dataframe containing data to be summarised and displayed;
#' a DF returned by `filter_dash_data()` is expected
#' @noRd

table_overview <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                             unset = "dashboard"
                           ),
                           title = paste0(
                             "Victorian employment summary, ",
                             format(max(data$date), "%B %Y")
                           )) {
  data <- filter_dash_data(
    series_ids = c(
      "A84423354L",
      "A84423242V",
      "A84423466F",
      "A84424691V",
      "A84600079X", # Regional UR
      "A84423350C",
      "A84423349V",
      "A84423357V",
      "pt_emp_vic",
      "A84423461V",
      "A84423237A",
      "A84424687C",
      "A84423355R",
      "A84423243W",
      "A84423467J",
      "A84424692W",
      "A84426256L",
      "A85223450L",
      "A85223451R",
      "A84423356T"
    ),
    df = dash_data
  )

  # Youth data = 12m rolling average
  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = dplyr::if_else(.data$series_id %in% c(
      "A84433601W",
      "A84424691V",
      "A84424687C",
      "A84424692W"
    ),
    slider::slide_mean(.data$value, before = 11, complete = TRUE),
    .data$value
    ))

  # Regional data = 3m rolling average
  data <- data %>%
    dplyr::mutate(value = dplyr::if_else(.data$series_id == "A84600079X",
      slider::slide_mean(.data$value, before = 2, complete = TRUE),
      .data$value
    )) %>%
    dplyr::ungroup()

  make_table_mem(
    data = data,
    destination = destination,
    title = title,
    row_order = c(
      "A84423354L",
      "A84423242V",
      "A84423466F",
      "A84424691V",
      "A84600079X",
      "A84423350C",
      "A84423349V",
      "A84423357V",
      "pt_emp_vic",
      "A84423237A",
      "A84423461V",
      "A84424687C",
      "A84423355R",
      "A84423243W",
      "A84423467J",
      "A84424692W",
      "A84426256L",
      "A85223450L",
      "A85223451R",
      "A84423356T"
    ),
    highlight_rows = c(
      "A84423354L",
      "A84423349V",
      "A84423355R",
      "A84426256L",
      "A85223450L",
      "A85223451R",
      "A84423356T"
    ),
    notes = " All data seasonally adjusted, other than youth figures, which are smoothed using a 12-month rolling average, and regional figures, which are smoothed using a 3-month rolling average."
  )
}

table_gr_sex <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                           unset = "dashboard"
                         ),
                         title = paste0(
                           "Victorian employment summary by sex, ",
                           format(max(data$date), "%B %Y")
                         )) {
  data <- filter_dash_data(c(
    "A84423237A",
    "A84423461V",
    "A84423238C",
    "A84423462W",
    "A84423242V",
    "A84423466F",
    "A84423243W",
    "A84423467J"
  ))

  make_table_mem(data,
    row_order = c(
      "A84423237A",
      "A84423461V",
      "A84423238C",
      "A84423462W",
      "A84423242V",
      "A84423466F",
      "A84423243W",
      "A84423467J"
    ),
    title = title,
    destination = destination,
  )
}

table_gr_youth_summary <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                     unset = "dashboard"
                                   ),
                                   title = paste0(
                                     "Victorian youth (15-24) labour force status summary, ",
                                     format(max(data$date), "%B %Y"),
                                     " (12-month average)"
                                   )) {
  data <- filter_dash_data(
    c(
      "A84424687C",
      "A84424688F",
      "A84424691V",
      "A84424692W",
      "A84424602F",
      "15-24_females_unemployment rate",
      "15-24_males_unemployment rate"
    )
  )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value, before = 11, complete = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))

  data %>%
    make_table_mem(
      row_order = c(
        "A84424687C",
        "A84424691V",
        "15-24_males_unemployment rate",
        "15-24_females_unemployment rate",
        "A84424688F",
        "A84424692W",
        "A84424602F"
      ),
      highlight_rows = c(
        "A84424687C",
        "A84424691V",
        "A84424688F",
        "A84424692W"
      ),
      title = title,
      destination = destination,
      notes = "Data not seasonally adjusted; smoothed using a 12-month rolling average."
    )
}

table_gr_youth_unemp_region <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                          unset = "dashboard"
                                        ),
                                        title = paste0(
                                          "Youth (15-24) unemployment rate across Victoria, ",
                                          format(max(data$date), "%B %Y"),
                                          " (12-month average)"
                                        )) {
  data <- filter_dash_data(
    c(
      "15-24_employed_ballarat",
      "15-24_employed_bendigo",
      "15-24_employed_geelong",
      "15-24_employed_hume",
      "15-24_employed_latrobe - gippsland",
      "15-24_employed_shepparton",
      "15-24_employed_victoria - north west",
      "15-24_employed_warrnambool and south west",
      "15-24_employed_rest of vic.",
      "15-24_unemployed_ballarat",
      "15-24_unemployed_bendigo",
      "15-24_unemployed_geelong",
      "15-24_unemployed_hume",
      "15-24_unemployed_latrobe - gippsland",
      "15-24_unemployed_shepparton",
      "15-24_unemployed_victoria - north west",
      "15-24_unemployed_warrnambool and south west",
      "15-24_unemployed_rest of vic.",
      "15-24_employed_melbourne - inner",
      "15-24_employed_melbourne - inner east",
      "15-24_employed_melbourne - inner south",
      "15-24_employed_melbourne - north east",
      "15-24_employed_melbourne - north west",
      "15-24_employed_melbourne - outer east",
      "15-24_employed_melbourne - south east",
      "15-24_employed_melbourne - west",
      "15-24_employed_mornington peninsula",
      "15-24_employed_greater melbourne",
      "15-24_unemployed_melbourne - inner",
      "15-24_unemployed_melbourne - inner east",
      "15-24_unemployed_melbourne - inner south",
      "15-24_unemployed_melbourne - north east",
      "15-24_unemployed_melbourne - north west",
      "15-24_unemployed_melbourne - outer east",
      "15-24_unemployed_melbourne - south east",
      "15-24_unemployed_melbourne - west",
      "15-24_unemployed_mornington peninsula",
      "15-24_unemployed_greater melbourne"
    )
  )

  data <- data %>%
    dplyr::select(
      .data$date, .data$series, .data$table_no,
      .data$frequency, .data$value
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      age = .data$split_series[, 1],
      indicator = .data$split_series[, 2],
      sa4 = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$series)

  data <- data %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    ) %>%
    dplyr::group_by(.data$sa4, .data$date, .data$age, .data$table_no, .data$frequency) %>%
    dplyr::mutate(value = 100 * (.data$Unemployed /
      (.data$Unemployed + .data$Employed))) %>%
    dplyr::select(-.data$Employed, -.data$Unemployed) %>%
    dplyr::group_by(.data$sa4, .data$age) %>%
    dplyr::mutate(
      indicator = "Unemployment rate",
      value = slider::slide_mean(.data$value,
        before = 11L,
        complete = TRUE
      )
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate(
      unit = "Percent",
      series_id = paste(.data$age, .data$indicator, .data$sa4, sep = "_"),
      series = paste(.data$age, .data$indicator, .data$sa4, sep = " ; ")
    ) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::mutate(
      sa4 = dplyr::if_else(.data$sa4 == "Rest of Vic.",
        "Regional Victoria",
        .data$sa4
      ),
      indicator = dplyr::if_else(
        .data$sa4 %in% c(
          "Greater Melbourne",
          "Regional Victoria"
        ),
        paste0(.data$sa4, " youth unemployment rate"),
        .data$sa4
      )
    )

  data %>%
    make_table_mem(
      rename_indicators = F,
      row_order = c(
        "15-24_Unemployment rate_Greater Melbourne",
        "15-24_Unemployment rate_Melbourne - Inner",
        "15-24_Unemployment rate_Melbourne - Inner East",
        "15-24_Unemployment rate_Melbourne - Inner South",
        "15-24_Unemployment rate_Melbourne - North East",
        "15-24_Unemployment rate_Melbourne - North West",
        "15-24_Unemployment rate_Melbourne - Outer East",
        "15-24_Unemployment rate_Melbourne - South East",
        "15-24_Unemployment rate_Melbourne - West",
        "15-24_Unemployment rate_Mornington Peninsula",
        "15-24_Unemployment rate_Rest of Vic.",
        "15-24_Unemployment rate_Ballarat",
        "15-24_Unemployment rate_Bendigo",
        "15-24_Unemployment rate_Geelong",
        "15-24_Unemployment rate_Hume",
        "15-24_Unemployment rate_Latrobe - Gippsland",
        "15-24_Unemployment rate_Shepparton",
        "15-24_Unemployment rate_Victoria - North West",
        "15-24_Unemployment rate_Warrnambool and South West"
      ),
      highlight_rows = c(
        "15-24_Unemployment rate_Rest of Vic.",
        "15-24_Unemployment rate_Greater Melbourne"
      ),
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 12-month rolling average."
    )
}

table_reg_nonmetro_states_unemprate <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                                  unset = "dashboard"
                                                ),
                                                title = paste0(
                                                  "Regional unemployment rate by state, ",
                                                  format(max(data$date), "%B %Y"),
                                                  " (3-month average)"
                                                )) {
  data <- filter_dash_data(
    c(
      "A84599629X",
      "A84600079X",
      "A84599785A",
      "A84599719C",
      "A84600247X",
      "A84599635V"
    )
  )

  data <- data %>%
    dplyr::mutate(indicator = gsub("Rest of ", "Regional ", .data$gcc_restofstate, fixed = T)) %>%
    dplyr::group_by(.data$indicator) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    )) %>%
    dplyr::ungroup()

  make_table_mem(data,
    row_order = c(
      "A84600079X",
      "A84599629X",
      "A84599785A",
      "A84600247X",
      "A84599719C",
      "A84599635V"
    ),
    rename_indicators = FALSE,
    destination = destination,
    title = title,
    notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
  )
}

table_reg_nonmetro_emp <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                     unset = "dashboard"
                                   ),
                                   title = paste0(
                                     "Employment across regional Victoria, ",
                                     format(max(data$date), "%B %Y"),
                                     " (3-month average)"
                                   )) {
  data <- filter_dash_data(
    c(
      "A84600075R",
      "A84599661X",
      "A84600027W",
      "A84599667L",
      "A84599673J",
      "A84599679W",
      "A84599925T",
      "A84600117A",
      "A84600033T"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Regional Victoria employed persons",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84600075R",
        "A84599661X",
        "A84600027W",
        "A84599667L",
        "A84599673J",
        "A84599679W",
        "A84599925T",
        "A84600117A",
        "A84600033T"
      ),
      highlight_rows = c("A84600075R"),
      rename_indicators = FALSE,
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_reg_nonmetro_unemp <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                       unset = "dashboard"
                                     ),
                                     title = paste0(
                                       "Unemployment across regional Victoria, ",
                                       format(max(data$date), "%B %Y"),
                                       " (3-month average)"
                                     )) {
  data <- filter_dash_data(
    c(
      "A84600076T",
      "A84599662A",
      "A84600028X",
      "A84599668R",
      "A84599674K",
      "A84599680F",
      "A84599926V",
      "A84600118C",
      "A84600034V"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Regional Victoria unemployed persons",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84600076T",
        "A84599662A",
        "A84600028X",
        "A84599668R",
        "A84599674K",
        "A84599680F",
        "A84599926V",
        "A84600118C",
        "A84600034V"
      ),
      highlight_rows = c("A84600076T"),
      rename_indicators = FALSE,
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_reg_nonmetro_unemprate <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                           unset = "dashboard"
                                         ),
                                         title = paste0(
                                           "Unemployment rate across regional Victoria, ",
                                           format(max(data$date), "%B %Y"),
                                           " (3-month average)"
                                         )) {
  data <- filter_dash_data(
    c(
      "A84595471L",
      "A84599665J",
      "A84600031L",
      "A84599671C",
      "A84599677T",
      "A84599683L",
      "A84599929A",
      "A84600121T",
      "A84600037A"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Regional Victoria unemployment rate",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84595471L",
        "A84599665J",
        "A84600031L",
        "A84599671C",
        "A84599677T",
        "A84599683L",
        "A84599929A",
        "A84600121T",
        "A84600037A"
      ),
      highlight_rows = c("A84595471L"),
      rename_indicators = FALSE,
      title = title,
      destination = destination,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_reg_nonmetro_partrate <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                          unset = "dashboard"
                                        ),
                                        title = paste0(
                                          "Participation rate across regional Victoria, ",
                                          format(max(data$date), "%B %Y"),
                                          " (3-month average)"
                                        )) {
  data <- filter_dash_data(
    c(
      "A84599666K",
      "A84600032R",
      "A84599672F",
      "A84599678V",
      "A84599684R",
      "A84599930K",
      "A84600122V",
      "A84600038C",
      "A84600080J"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Regional Victoria participation rate",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84600080J",
        "A84599666K",
        "A84600032R",
        "A84599672F",
        "A84599678V",
        "A84599684R",
        "A84599930K",
        "A84600122V",
        "A84600038C"
      ),
      highlight_rows = c("A84600080J"),
      rename_indicators = FALSE,
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_reg_metro_states_unemprate <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                               unset = "dashboard"
                                             ),
                                             title = paste0(
                                               "Unemployment rate in metropolitan areas by State, ",
                                               format(max(data$date), "%B %Y")
                                             )) {
  data <- filter_dash_data(
    c(
      "A84599623K",
      "A84600145K",
      "A84600151F",
      "A84600157V",
      "A84600241K",
      "A84599791W"
    )
  )

  data <- data %>%
    dplyr::mutate(indicator = gsub("Greater ", "", .data$gcc_restofstate,
      fixed = T
    )) %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    )) %>%
    dplyr::filter(!is.na(.data$value))

  make_table_mem(data,
    title = title,
    destination = destination,
    row_order = c(
      "A84600145K",
      "A84599623K",
      "A84600151F",
      "A84600241K",
      "A84600157V",
      "A84599791W"
    ),
    notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
  )
}

table_reg_metro_emp <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                  unset = "dashboard"
                                ),
                                title = paste0(
                                  "Employment across Greater Melbourne, ",
                                  format(max(data$date), "%B %Y"),
                                  " (3-month average)"
                                )) {
  data <- filter_dash_data(
    c(
      "A84600141A",
      "A84599655C",
      "A84600015L",
      "A84600183X",
      "A84599553R",
      "A84600111L",
      "A84599847W",
      "A84599919W",
      "A84600021J",
      "A84600189L"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Greater Melbourne employed persons",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84600141A",
        "A84599655C",
        "A84600015L",
        "A84600183X",
        "A84599553R",
        "A84600111L",
        "A84599847W",
        "A84599919W",
        "A84600021J",
        "A84600189L"
      ),
      highlight_rows = c("A84600141A"),
      rename_indicators = FALSE,
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_reg_metro_unemp <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                    unset = "dashboard"
                                  ),
                                  title = paste0(
                                    "Unemployed persons across Greater Melbourne, ",
                                    format(max(data$date), "%B %Y"),
                                    " (3-month average)"
                                  )) {
  data <- filter_dash_data(
    c(
      "A84600142C",
      "A84599656F",
      "A84600016R",
      "A84600184A",
      "A84599554T",
      "A84600112R",
      "A84599848X",
      "A84599920F",
      "A84600022K",
      "A84600190W"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Greater Melbourne unemployed persons",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84600142C",
        "A84599656F",
        "A84600016R",
        "A84600184A",
        "A84599554T",
        "A84600112R",
        "A84599848X",
        "A84599920F",
        "A84600022K",
        "A84600190W"
      ),
      highlight_rows = c("A84600142C"),
      rename_indicators = FALSE,
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_reg_metro_unemprate <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                        unset = "dashboard"
                                      ),
                                      title = paste0(
                                        "Unemployment rate across Greater Melbourne, ",
                                        format(max(data$date), "%B %Y"),
                                        " (3-month average)"
                                      )) {
  data <- filter_dash_data(
    c(
      "A84600145K",
      "A84599659L",
      "A84600019W",
      "A84600187J",
      "A84599557X",
      "A84600115W",
      "A84599851L",
      "A84599923L",
      "A84600025T",
      "A84600193C"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Greater Melbourne unemployment rate",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84600145K",
        "A84599659L",
        "A84600019W",
        "A84600187J",
        "A84599557X",
        "A84600115W",
        "A84599851L",
        "A84599923L",
        "A84600025T",
        "A84600193C"
      ),
      highlight_rows = c("A84600145K"),
      rename_indicators = FALSE,
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_reg_metro_partrate <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                       unset = "dashboard"
                                     ),
                                     title = paste0(
                                       "Participation rate across Greater Melbourne, ",
                                       format(max(data$date), "%B %Y"),
                                       " (3-month average)"
                                     )) {
  data <- filter_dash_data(
    c(
      "A84600146L",
      "A84599660W",
      "A84600020F",
      "A84600188K",
      "A84599558A",
      "A84600116X",
      "A84599852R",
      "A84599924R",
      "A84600026V",
      "A84600194F"
    )
  )

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::if_else(.data$sa4 == "",
        "Greater Melbourne participation rate",
        .data$sa4
      )
    )

  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2L,
      complete = TRUE
    ))

  data %>%
    make_table_mem(
      row_order = c(
        "A84600146L",
        "A84599660W",
        "A84600020F",
        "A84600188K",
        "A84599558A",
        "A84600116X",
        "A84599852R",
        "A84599924R",
        "A84600026V",
        "A84600194F"
      ),
      highlight_rows = c("A84600146L"),
      rename_indicators = FALSE,
      destination = destination,
      title = title,
      notes = "Data not seasonally adjusted; smoothed using a 3-month rolling average."
    )
}

table_ind_unemp_state <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                    unset = "dashboard"
                                  ),
                                  title = paste0(
                                    "Unemployment rate by state, ",
                                    format(max(data$date), "%B %Y")
                                  )) {
  data <- filter_dash_data(
    c(
      "A84423270C",
      "A84423354L",
      "A84423284T",
      "A84423368A",
      "A84423326C",
      "A84423298F",
      "A84423050A"
    )
  )

  data <- data %>%
    dplyr::mutate(indicator = dplyr::if_else(.data$state == "", "Australia", .data$state))

  make_table_mem(
    data = data,
    row_order = c(
      "A84423050A",
      "A84423354L",
      "A84423270C",
      "A84423284T",
      "A84423326C",
      "A84423368A",
      "A84423298F"
    ),
    title = title,
    destination = destination,
    rename_indicators = F
  )
}

table_ind_employment <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                   unset = "dashboard"
                                 )) {
  data <- filter_dash_data(c(
    "A84423349V", # total employed
    "A84423357V", # employed fulltime
    "A84423356T", # emp to pop ratio, all persons
    "A84423244X", # emp to pop ratio, males
    "A84423468K", # emp to pop ratio, females
    "pt_emp_vic"
  ))

  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table_mem(table_data,
    row_order = c(
      "A84423349V",
      "A84423357V",
      "pt_emp_vic",
      "A84423356T",
      "A84423244X",
      "A84423468K"
    ),
    highlight_rows = c(
      "A84423349V",
      "A84423356T"
    ),
    destination = destination,
    notes = "Data not seasonally adjusted."
  )
}

table_ind_unemp_summary <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                      unset = "dashboard"
                                    )) {
  data <- filter_dash_data(c(
    "A84423354L", # Unemp rate
    "A84423350C", # Unemp total
    "A85223451R", # Underut rate
    "A84424691V", # Youth unemp,
    "A84423242V", # Male unemp
    "A84423466F" # Female unemp
  ))

  # Youth unemployment = 12m rolling average
  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = dplyr::if_else(.data$series_id == "A84424691V",
      slider::slide_mean(.data$value, before = 11, complete = TRUE),
      .data$value
    )) %>%
    dplyr::ungroup()

  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table_mem(table_data,
    row_order = c(
      "A84423350C",
      "A84423242V",
      "A84423466F",
      "A84424691V",
      "A84423354L",
      "A85223451R"
    ),
    highlight_rows = c(
      "A84423350C",
      "A84423354L",
      "A85223451R"
    ),
    destination = destination,
    notes = "Data not seasonally adjusted; smoothed using a 12-month rolling average."
  )
}

table_ind_hours_summary <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                      unset = "dashboard"
                                    )) {
  data <- filter_dash_data(c(
    "A84426256L" # , # Total hours
  ))

  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table_mem(table_data,
    destination = destination
  )
}

table_industries_summary <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                       unset = "dashboard"
                                     ),
                                     title = paste0(
                                       "Victorian employment by industry, ",
                                       format(max(data$date), "%B %Y"),
                                       " quarter (not seasonally adjusted)"
                                     )) {
  data <- filter_dash_data(
    c(
      "A84601662A",
      "A84601680F",
      "A84601683L",
      "A84601686V",
      "A84601665J",
      "A84601704L",
      "A84601707V",
      "A84601710J",
      "A84601638A",
      "A84601653X",
      "A84601689A",
      "A84601656F",
      "A84601713R",
      "A84601668R",
      "A84601695W",
      "A84601698C",
      "A84601650T",
      "A84601671C",
      "A84601641R",
      "A84601716W"
    )
  )

  data <- data %>%
    dplyr::mutate(indicator = dplyr::if_else(.data$industry != "",
      .data$industry,
      "Victoria - all industries"
    ))

  make_table_mem(data,
    row_order = c(
      "A84601662A",
      "A84601680F",
      "A84601683L",
      "A84601686V",
      "A84601665J",
      "A84601704L",
      "A84601707V",
      "A84601710J",
      "A84601638A",
      "A84601653X",
      "A84601689A",
      "A84601656F",
      "A84601713R",
      "A84601668R",
      "A84601695W",
      "A84601698C",
      "A84601650T",
      "A84601671C",
      "A84601641R",
      "A84601716W"
    ),
    highlight_rows = "A84601662A",
    title = title,
    notes = "Data is original (not seasonally adjusted).",
    destination = destination,
    rename_indicators = FALSE
  )
}

table_jobactive_regions <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                      unset = "dashboard"
                                    ),
                                    title = paste0(
                                      "Jobactive caseload by Victorian employment regions, ",
                                      format(max(data$date), "%B %Y")
                                    )) {
  data <- filter_dash_data(c(
    "jobactive_total_ballarat",
    "jobactive_total_bendigo",
    "jobactive_total_barwon",
    "jobactive_total_gippsland",
    "jobactive_total_goulburn/murray",
    "jobactive_total_inner metropolitan melbourne",
    "jobactive_total_north eastern melbourne",
    "jobactive_total_north western melbourne",
    "jobactive_total_south coast of victoria",
    "jobactive_total_south eastern melbourne and peninsula",
    "jobactive_total_western melbourne",
    "jobactive_total_wimmera mallee"
  ))

  table_data <- data %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      total = .data$split_series[, 2],
      indicator = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$total, -.data$jobactive)

  table_data <- table_data %>%
    dplyr::group_by(
      .data$date,
      .data$frequency, .data$unit, .data$table_no
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(
      series = "jobactive_total_total",
      series_id = "jobactive_total_total",
      indicator = "Total jobactive caseload"
    ) %>%
    dplyr::bind_rows(table_data)

  make_table(table_data,
    row_order = c(
      "jobactive_total_total",
      "jobactive_total_ballarat",
      "jobactive_total_bendigo",
      "jobactive_total_barwon",
      "jobactive_total_gippsland",
      "jobactive_total_goulburn/murray",
      "jobactive_total_inner metropolitan melbourne",
      "jobactive_total_north eastern melbourne",
      "jobactive_total_north western melbourne",
      "jobactive_total_south coast of victoria",
      "jobactive_total_south eastern melbourne and peninsula",
      "jobactive_total_western melbourne",
      "jobactive_total_wimmera mallee"
    ),
    highlight_rows = "jobactive_total_total",
    title = title,
    destination = destination,
    rename_indicators = FALSE,
    pretty_round = FALSE
  )
}


table_jobactive_aboriginal <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                         unset = "dashboard"
                                       ),
                                       title = paste0(
                                         "Jobactive caseload for Aboriginal Victorians, ",
                                         format(max(data$date), "%B %Y")
                                       )) {
  data <- filter_dash_data(c(
    "jobactive_indigenous_ballarat",
    "jobactive_indigenous_bendigo",
    "jobactive_indigenous_barwon",
    "jobactive_indigenous_gippsland",
    "jobactive_indigenous_goulburn/murray",
    "jobactive_indigenous_inner metropolitan melbourne",
    "jobactive_indigenous_north eastern melbourne",
    "jobactive_indigenous_north western melbourne",
    "jobactive_indigenous_south coast of victoria",
    "jobactive_indigenous_south eastern melbourne and peninsula",
    "jobactive_indigenous_western melbourne",
    "jobactive_indigenous_wimmera mallee"
  ))

  table_data <- data %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      total = .data$split_series[, 2],
      indicator = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$total, -.data$jobactive)

  table_data <- table_data %>%
    dplyr::group_by(
      .data$date,
      .data$frequency, .data$unit, .data$table_no
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(
      series = "jobactive_total_aboriginal",
      series_id = "jobactive_total_aboriginal",
      indicator = "Aboriginal jobactive caseload"
    ) %>%
    dplyr::bind_rows(table_data)


  make_table(table_data,
    row_order = c(
      "jobactive_total_aboriginal",
      "jobactive_indigenous_ballarat",
      "jobactive_indigenous_bendigo",
      "jobactive_indigenous_barwon",
      "jobactive_indigenous_gippsland",
      "jobactive_indigenous_goulburn/murray",
      "jobactive_indigenous_inner metropolitan melbourne",
      "jobactive_indigenous_north eastern melbourne",
      "jobactive_indigenous_north western melbourne",
      "jobactive_indigenous_south coast of victoria",
      "jobactive_indigenous_south eastern melbourne and peninsula",
      "jobactive_indigenous_western melbourne",
      "jobactive_indigenous_wimmera mallee"
    ),
    highlight_rows = "jobactive_total_aboriginal",
    title = title,
    destination = destination,
    rename_indicators = FALSE,
    pretty_round = FALSE
  )
}


table_jobactive_mature_age <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                         unset = "dashboard"
                                       ),
                                       title = paste0(
                                         "Jobactive caseload for mature age Victorians, ",
                                         format(max(data$date), "%B %Y")
                                       )) {
  data <- filter_dash_data(c(
    "jobactive_mature age (50+)_ballarat",
    "jobactive_mature age (50+)_bendigo",
    "jobactive_mature age (50+)_barwon",
    "jobactive_mature age (50+)_gippsland",
    "jobactive_mature age (50+)_goulburn/murray",
    "jobactive_mature age (50+)_inner metropolitan melbourne",
    "jobactive_mature age (50+)_north eastern melbourne",
    "jobactive_mature age (50+)_north western melbourne",
    "jobactive_mature age (50+)_south coast of victoria",
    "jobactive_mature age (50+)_south eastern melbourne and peninsula",
    "jobactive_mature age (50+)_western melbourne",
    "jobactive_mature age (50+)_wimmera mallee"
  ))

  table_data <- data %>%
    dplyr::select(
      .data$date, .data$series_id, .data$series,
      .data$frequency, .data$value, .data$unit, .data$table_no
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      total = .data$split_series[, 2],
      indicator = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$total, -.data$jobactive)

  table_data <- table_data %>%
    dplyr::group_by(
      .data$date,
      .data$frequency, .data$unit, .data$table_no
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(
      series = "jobactive_total_mature",
      series_id = "jobactive_total_mature",
      indicator = "Mature age (50+) jobactive caseload"
    ) %>%
    dplyr::bind_rows(table_data)

  make_table(table_data,
    row_order = c(
      "jobactive_total_mature",
      "jobactive_mature age (50+)_ballarat",
      "jobactive_mature age (50+)_bendigo",
      "jobactive_mature age (50+)_barwon",
      "jobactive_mature age (50+)_gippsland",
      "jobactive_mature age (50+)_goulburn/murray",
      "jobactive_mature age (50+)_inner metropolitan melbourne",
      "jobactive_mature age (50+)_north eastern melbourne",
      "jobactive_mature age (50+)_north western melbourne",
      "jobactive_mature age (50+)_south coast of victoria",
      "jobactive_mature age (50+)_south eastern melbourne and peninsula",
      "jobactive_mature age (50+)_western melbourne",
      "jobactive_mature age (50+)_wimmera mallee"
    ),
    title = title,
    destination = destination,
    highlight_rows = "jobactive_total_mature",
    rename_indicators = FALSE,
    pretty_round = FALSE
  )
}


table_jobactive_pwd <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                  unset = "dashboard"
                                ),
                                title = paste0(
                                  "Jobactive caseload for Victorians with disabilities",
                                  format(max(data$date), "%B %Y")
                                )) {
  data <- filter_dash_data(c(
    "jobactive_pwd_ballarat",
    "jobactive_pwd_bendigo",
    "jobactive_pwd_barwon",
    "jobactive_pwd_gippsland",
    "jobactive_pwd_goulburn/murray",
    "jobactive_pwd_inner metropolitan melbourne",
    "jobactive_pwd_north eastern melbourne",
    "jobactive_pwd_north western melbourne",
    "jobactive_pwd_south coast of victoria",
    "jobactive_pwd_south eastern melbourne and peninsula",
    "jobactive_pwd_western melbourne",
    "jobactive_pwd_wimmera mallee"
  ))

  table_data <- data %>%
    dplyr::select(
      .data$date, .data$series_id, .data$series,
      .data$frequency, .data$value, .data$unit, .data$table_no
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      total = .data$split_series[, 2],
      indicator = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$total, -.data$jobactive)

  table_data <- table_data %>%
    dplyr::group_by(
      .data$date,
      .data$frequency, .data$unit, .data$table_no
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(
      series = "jobactive_total_pwd",
      series_id = "jobactive_total_pwd",
      indicator = "People with disabilities, Victorian total"
    ) %>%
    dplyr::bind_rows(table_data)

  make_table(table_data,
    row_order = c(
      "jobactive_total_pwd",
      "jobactive_pwd_ballarat",
      "jobactive_pwd_bendigo",
      "jobactive_pwd_barwon",
      "jobactive_pwd_gippsland",
      "jobactive_pwd_goulburn/murray",
      "jobactive_pwd_inner metropolitan melbourne",
      "jobactive_pwd_north eastern melbourne",
      "jobactive_pwd_north western melbourne",
      "jobactive_pwd_south coast of victoria",
      "jobactive_pwd_south eastern melbourne and peninsula",
      "jobactive_pwd_western melbourne",
      "jobactive_pwd_wimmera mallee"
    ),
    highlight_rows = "jobactive_total_pwd",
    title = title,
    destination = destination,
    rename_indicators = FALSE,
    pretty_round = FALSE
  )
}

table_jobactive_female <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                     unset = "dashboard"
                                   ),
                                   title = paste0(
                                     "Jobactive caseload for female Victorians, ",
                                     format(max(data$date), "%B %Y")
                                   )) {
  data <- filter_dash_data(c(
    "jobactive_female_ballarat",
    "jobactive_female_bendigo",
    "jobactive_female_barwon",
    "jobactive_female_gippsland",
    "jobactive_female_goulburn/murray",
    "jobactive_female_inner metropolitan melbourne",
    "jobactive_female_north eastern melbourne",
    "jobactive_female_north western melbourne",
    "jobactive_female_south coast of victoria",
    "jobactive_female_south eastern melbourne and peninsula",
    "jobactive_female_western melbourne",
    "jobactive_female_wimmera mallee"
  ))

  table_data <- data %>%
    dplyr::select(
      .data$date, .data$series_id, .data$series,
      .data$frequency, .data$value, .data$unit, .data$table_no
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      total = .data$split_series[, 2],
      indicator = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$total, -.data$jobactive)

  table_data <- table_data %>%
    dplyr::group_by(
      .data$date,
      .data$frequency, .data$unit, .data$table_no
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(
      series = "jobactive_total_female",
      series_id = "jobactive_total_female",
      indicator = "Female jobactive caseload, Victorian total"
    ) %>%
    dplyr::bind_rows(table_data)

  make_table(table_data,
    row_order = c(
      "jobactive_total_female",
      "jobactive_female_ballarat",
      "jobactive_female_bendigo",
      "jobactive_female_barwon",
      "jobactive_female_gippsland",
      "jobactive_female_goulburn/murray",
      "jobactive_female_inner metropolitan melbourne",
      "jobactive_female_north eastern melbourne",
      "jobactive_female_north western melbourne",
      "jobactive_female_south coast of victoria",
      "jobactive_female_south eastern melbourne and peninsula",
      "jobactive_female_western melbourne",
      "jobactive_female_wimmera mallee"
    ),
    highlight_rows = "jobactive_total_female",
    title = title,
    destination = destination,
    rename_indicators = FALSE,
    pretty_round = FALSE
  )
}


table_jobactive_refugees <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                       unset = "dashboard"
                                     ),
                                     title = paste0(
                                       "Victorian jobactive caseload for refugees, ",
                                       format(max(data$date), "%B %Y")
                                     )) {
  data <- filter_dash_data(c(
    "jobactive_refugee_ballarat",
    "jobactive_refugee_bendigo",
    "jobactive_refugee_barwon",
    "jobactive_refugee_gippsland",
    "jobactive_refugee_goulburn/murray",
    "jobactive_refugee_inner metropolitan melbourne",
    "jobactive_refugee_north eastern melbourne",
    "jobactive_refugee_north western melbourne",
    "jobactive_refugee_south coast of victoria",
    "jobactive_refugee_south eastern melbourne and peninsula",
    "jobactive_refugee_western melbourne",
    "jobactive_refugee_wimmera mallee"
  ))

  table_data <- data %>%
    dplyr::select(
      .data$date, .data$series_id, .data$series,
      .data$frequency, .data$value, .data$unit, .data$table_no
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      total = .data$split_series[, 2],
      indicator = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$total, -.data$jobactive)

  table_data <- table_data %>%
    dplyr::group_by(
      .data$date,
      .data$frequency, .data$unit, .data$table_no
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(
      series = "jobactive_total_refugees",
      series_id = "jobactive_total_refugees",
      indicator = "Refugees jobactive caseload, Victorian total"
    ) %>%
    dplyr::bind_rows(table_data)

  make_table(table_data,
    row_order = c(
      "jobactive_total_refugees",
      "jobactive_refugee_ballarat",
      "jobactive_refugee_bendigo",
      "jobactive_refugee_barwon",
      "jobactive_refugee_gippsland",
      "jobactive_refugee_goulburn/murray",
      "jobactive_refugee_inner metropolitan melbourne",
      "jobactive_refugee_north eastern melbourne",
      "jobactive_refugee_north western melbourne",
      "jobactive_refugee_south coast of victoria",
      "jobactive_refugee_south eastern melbourne and peninsula",
      "jobactive_refugee_western melbourne",
      "jobactive_refugee_wimmera mallee"
    ),
    highlight_rows = "jobactive_total_refugees",
    title = title,
    destination = destination,
    rename_indicators = FALSE,
    pretty_round = FALSE
  )
}

table_jobactive_youth <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                    unset = "dashboard"
                                  ),
                                  title = paste0(
                                    "Total jobactive caseload for mature age Victorians, ",
                                    format(max(data$date), "%B %Y")
                                  )) {
  data <- filter_dash_data(c(
    "jobactive_youth (15-24)_ballarat",
    "jobactive_youth (15-24)_bendigo",
    "jobactive_youth (15-24)_barwon",
    "jobactive_youth (15-24)_gippsland",
    "jobactive_youth (15-24)_goulburn/murray",
    "jobactive_youth (15-24)_inner metropolitan melbourne",
    "jobactive_youth (15-24)_north eastern melbourne",
    "jobactive_youth (15-24)_north western melbourne",
    "jobactive_youth (15-24)_south coast of victoria",
    "jobactive_youth (15-24)_south eastern melbourne and peninsula",
    "jobactive_youth (15-24)_western melbourne",
    "jobactive_youth (15-24)_wimmera mallee"
  ))

  table_data <- data %>%
    dplyr::select(
      .data$date, .data$series_id, .data$series,
      .data$frequency, .data$value, .data$unit, .data$table_no
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      total = .data$split_series[, 2],
      indicator = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$total, -.data$jobactive)

  table_data <- table_data %>%
    dplyr::group_by(
      .data$date,
      .data$frequency, .data$unit, .data$table_no
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(
      series = "jobactive_total_youth",
      series_id = "jobactive_total_youth",
      indicator = "Youth (15-24) jobactive caseload"
    ) %>%
    dplyr::bind_rows(table_data)

  make_table(table_data,
    row_order = c(
      "jobactive_total_youth",
      "jobactive_youth (15-24)_ballarat",
      "jobactive_youth (15-24)_bendigo",
      "jobactive_youth (15-24)_barwon",
      "jobactive_youth (15-24)_gippsland",
      "jobactive_youth (15-24)_goulburn/murray",
      "jobactive_youth (15-24)_inner metropolitan melbourne",
      "jobactive_youth (15-24)_north eastern melbourne",
      "jobactive_youth (15-24)_north western melbourne",
      "jobactive_youth (15-24)_south coast of victoria",
      "jobactive_youth (15-24)_south eastern melbourne and peninsula",
      "jobactive_youth (15-24)_western melbourne",
      "jobactive_youth (15-24)_wimmera mallee"
    ),
    highlight_rows = "jobactive_total_youth",
    title = title,
    destination = destination,
    rename_indicators = FALSE,
    pretty_round = FALSE
  )
}
