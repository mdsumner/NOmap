#' used in SOleg to test color palettes
#'
#' @param x
#' Object to test for is it discrete
#' @return
#' returns true false
#'
is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.numeric(x)|| is.integer(x)
}
