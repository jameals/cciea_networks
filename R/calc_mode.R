#' Calculate Mode
#'
#' Simply calculates the mode from a vector of numbers.
#' The second version excludes zeros when calculating the mode.
#'
#' @param v vector of numbers or integers
#' @return mode (numeric)
#' @examples
#' getmode(v=c(30,30,3))
#' @export
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode_nonzero <- function(v){
  uniqv <- unique(v)[which(unique(v)!=0)] # only pull unique, non-zero values
  uniqv[which.max(tabulate(match(v, uniqv)))]
}