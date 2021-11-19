#' Function to return numbers rounded for presentation purposes,
#' with commas where appropriate.
#'
#' @param x Numeric vector to round
#' @return Character vector of rounded numbers, with commas separating
#' thousands and 'm' to denote millions.
#'
#' Numbers below 1m are rounded to the nearest 100. Numbers >= 1m are
#' rounded to the nearest 1,000.
#' @examples
#' pretty_round(c(10, 110, 10050, 20500, 2000100, 3445123))
#' @export
pretty_round <- function(x) {
  x <- (x / 1000)
  x <- round2(x, 1)
  x <- x * 1000

  dplyr::case_when(
    # Over 10m, round to 1 decimal as in 123.1m
    abs(x) >= 1e8 ~ paste0(round2(x / 1e6, 1), "m"),
    # Over 1m, round to 3 decimals, as in 3.445m
    abs(x) >= 1e6 ~ paste0(round2(x / 1e6, 3), "m"),
    # Otherwise, format with commas as in 100,000
    TRUE ~ scales::comma(x)
  )
}
