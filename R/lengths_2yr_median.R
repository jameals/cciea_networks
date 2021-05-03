#' Calculate Median of Vessel Lengths
#'
#' Calculates the median from a vector of vessel lengths.
#'
#' @param v vector of vessel lengths for individual vessel
#' @param years vector of corresponding registration years
#' @return median (numeric)
#' @examples
#' getmode(v=c(30,30,3))
#' @export
get2yrmedian <- function(x, years){
  tmp_dat_frame <- data.frame("Year"=years, "Lengths" = x)
  #tmp_dat_frame <- tmp_dat_frame %>%
    #filter(!is.na(Lengths)) %>%
    #arrange(desc(Year))
  tmp_dat_sub <- tmp_dat_frame[1:2,]
  mean_length <- median(tmp_dat_sub$Lengths)
  return(median_length)
}