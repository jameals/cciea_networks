#' Generate Catch Matrix for KNN
#'
#' Create a catch matrix (rows=trips, columns=species/gear combos, values = revenue)
#' for fish tickets that have not been assigned metiers with infomap. This will be 
#' called from within the assign_metiers_knn function. It is very similar to the 
#' generate_catch_matrix function used in script 01, but is specialized for the KNN code.
#'
#' @param tickets fish ticket data without metiers
#' @param multigear If TRUE, builds matrix using species/gear combinations; if FALSE, just uses species
#' @param NAto0 If TRUE, converts NAs to 0s in the catch matrix
#' @param adjust.revenue If TRUE, uses the adjusted revenue in the catch matrix; if FALSE, raw reported exvessel revenue
#' @return catch matrix as data frame
#' @examples
#' 
#' @export
generate_catch_matrix_knn <- function(tickets, multigear = FALSE, NAto0 = TRUE, adjust.revenue = TRUE){
  if(multigear == FALSE){
    output.melted <- tickets %>%
      group_by(trip_id, spid_recode) %>%
      summarise(rev = ifelse(adjust.revenue == TRUE, sum(adj_revenue), sum(revenue))) %>%
      mutate(header = spid_recode) %>%
      ungroup() %>%
      dplyr::select(-spid_recode)
  } else if(multigear == TRUE){
    output.melted <- tickets %>%
      mutate(spid_gr = paste0(spid_recode, "_", grgroup)) %>%
      group_by(trip_id, spid_gr) %>%
      summarise(rev = ifelse(adjust.revenue == TRUE, sum(adj_revenue), sum(revenue))) %>%
      mutate(header = spid_gr) %>%
      ungroup() %>%
      dplyr::select(-spid_gr)
  }
  output <- spread(output.melted, key=header, value = rev)
  
  ## remove NAs if desired
  if(NAto0 == TRUE){
    output <- output %>%
      replace(is.na(.), 0)
  }
  
  return(output)
}
