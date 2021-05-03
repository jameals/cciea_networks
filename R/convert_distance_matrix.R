#' Convert Distance Matrix
#'
#' Convert a matrix of distance values into a data frame. For script 01, section 2.3
#'
#' @param inDist matrix output from of pairwise distances from parDist function
#' @return data frame of distance values for each pairwise comparison in the matrix
#' @examples
#' hellinger_df_list <- hellinger_dist_list %>% map(~convertDist(.x))
#' @export
convertDist <- function(inDist) {
  if (class(inDist) != "dist") stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) sequence(A) else attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1):1),
    value = as.vector(inDist))
}