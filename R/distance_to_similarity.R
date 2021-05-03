#' Convert Distance to Similarities
#'
#' The Hellinger distance is used to quantify difference between trips,
#' but the infomap function requires similarity values. This function will 
#' obtain a similarity score by subtracting the maximum value of D from 
#' each pairwise comparison's distance value. For script 01, section 3.1
#'
#' @param x data frame distance values
#' @param col column number with distance values
#' @param max.type If "individual", the maximum will be calculated from the given matrix. If "manual", a specified value will be applied as the maximum
#' @param total.max If max.type = "manual", provide the maximum value to be subtracted from each distance 
#' @return data frame with pairwise similarity values
#' @examples
#' similarities_df_list <- hellinger_df_list %>% map(~dist2sim(.x,3, max.type = "manual", total.max = dist_max))
#' @export
dist2sim <- function(x,col, max.type = "individual", total.max=0){
  distances_vec <- x[,col]
  if(max.type == "individual"){
    similarities_vec <- rep(max(distances_vec), times=length(distances_vec)) - distances_vec
  } else if(max.type == "manual"){
    similarities_vec <- rep(total.max, times=length(distances_vec)) - distances_vec
  } else{
    stop("incorrect max.type specified. please choose either INDIVIDUAL or MANUAL.")
  }
  x <- mutate(x, value.sim = similarities_vec)
  return(x)
}