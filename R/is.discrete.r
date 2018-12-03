## only used for SOleg

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.numeric(x)|| is.integer(x)
}
