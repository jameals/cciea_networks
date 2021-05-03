#' Generate Catch Matrix, InfoMap R. 2
#'
#' From the fish ticket data, create a catch matrix (rows= trips, 
#' columns = identifiers plus all species/gear combos, values = revenue or pounds).
#' Script 04, Section 2
#'
#' @param mydata catch matrix
#' @param metric values for the matrix are exvessel revenue ('revenue') or landed pounds ('lbs')
#' @param gear  If TRUE, columns will represent all species / gear group combinations; if FALSE, columns will represent all species
#' @param Nato0 If TRUE, replace NA values with 0 (revenue or lbs) when matrix is spread
#' @param adjust.revenue If TRUE, use the adjusted revenue column; If FALSE, use raw recorded revenue
#' @return a list with (1) catch matrix populated by revenue or landed pounds, (2) a melted data frame from catch matrix
#' @examples
#' revmat <- gen_catch_matrix_rerun(tickets = portdat, multigear=TRUE, NAto0 = TRUE)
#' @export
gen_catch_matrix_rerun <- function(mydata, multigear = FALSE, NAto0 = TRUE, adjust.revenue = TRUE){
  if(multigear == FALSE){
    output.melted <- mydata %>%
      group_by(drvid_year, crab_year, trip_id, spid_recode) %>%
      summarise(rev = ifelse(adjust.revenue == TRUE, sum(adj_revenue), sum(revenue))) %>%
      mutate(header = spid_recode) %>%
      ungroup() %>%
      dplyr::select(-spid_recode)
  } else if(multigear == TRUE){
    output.melted <- mydata %>%
      mutate(spid_gr = paste0(spid_recode, "_", grgroup)) %>%
      group_by(drvid_year, crab_year, trip_id, spid_gr) %>%
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