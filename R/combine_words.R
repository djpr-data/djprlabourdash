#' Combine elements of vector into a single string with commas and 'and'
#'
#' Simplified version of `knitr::combine_words()`
#' @author knitr authors
#' @param words Vector of element(s) to be combined into one string.
#' @return A length one character vector; elements will be separated by commas.
#'
#' @examples
#' combine_words(c("a", "b", "c"))
#' combine_words(c(1, 2, 3))
#' @export
#'

combine_words <- function(words) {
  and <- " and "

  n <- length(words)

  if (n == 0) {
    return(words)
  }

  words <- paste0("", words, "")

  if (n == 1) {
    return(words)
  }

  if (n == 2) {
    return(paste(words, collapse = and))
  }

  if (grepl("^ ", and) && grepl(" $", ", ")) {
    and <- gsub("^ ", "", and)
  }

  words[n] <- paste0(and, words[n])

  paste(words, collapse = ", ")
}
