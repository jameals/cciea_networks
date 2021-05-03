#' Calculate Length from 5 years of Registration Data
#'
#' Calculate the vessel length based on 5 years of 
#' vessel registration data. For vessels which do not
#' have adequate registration data for the most recent
#' 3 years. Called by `get_vessel_length.R` in `script_00`
#'
#' @param tmp_vessel_info a data frame with contains: VESSEL_NUM, AGENCY_CODE, REGISTRATION_YEAR, n_lengths, n_unique, max_length, min_length, median2yr
#' @return the final vessel length, and the type of calculation used to arrive at that length
#' @examples
#' olddat_output <- get_historic_length(tmp_vessel_info=old_vessel_info)
#' @export
get_historic_length <- function(tmp_vessel_info){
  ##### if only one unique vessel length was recorded
  if(tmp_vessel_info$n_unique == 1){
    #####-- if only 1 / all years contained vessel data
    if(tmp_vessel_info$n_lengths == 1){
      final_vessel_length <- tmp_vessel_info$max_length
      length_calc <- "unique_1"
    } 
    #####-- if 2 + years contained vessel data
    else{
      final_vessel_length <- tmp_vessel_info$max_length
      length_calc <- "unique_2up"
    }
  } 
  ##### if two unique vessel lengths were recorded
  else if(tmp_vessel_info$n_lengths == 2){
    #####-- if the max length is more than 2 * the min length
    if(tmp_vessel_info$max_length > 2*tmp_vessel_info$min_length){
      final_vessel_length <- NA
      length_calc <- "highSD"
    } 
    #####-- otherwise,
    else{
      final_vessel_length <- tmp_vessel_info$mode_length
      length_calc <- "mode"
    }
  } 
  
  #### if 3+ unique vessel lengths were recorded ####
  else{
    final_vessel_length <- tmp_vessel_info$med_length
    length_calc <- "median"
  }
  return(c(final_vessel_length, length_calc))
}