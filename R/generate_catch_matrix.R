#' Generate Catch Matrix
#'
#' From the fish ticket data, create a catch matrix (rows= trips, 
#' columns = identifiers plus all species/gear combos, values = revenue or pounds).
#' Script 01, Section 1.3
#'
#' @param mydata catch matrix
#' @param metric values for the matrix are exvessel revenue ('revenue') or landed pounds ('lbs')
#' @param gear  If TRUE, columns will represent all species / gear group combinations; if FALSE, columns will represent all species
#' @param Nato0 If TRUE, replace NA values with 0 (revenue or lbs) when matrix is spread
#' @param adjust.revenue If TRUE, use the adjusted revenue column; If FALSE, use raw recorded revenue
#' @return a list with (1) catch matrix populated by revenue or landed pounds, (2) a melted data frame from catch matrix
#' @examples
#' mydat <- generate_catch_matrix(mydata = rawdat.sub, metric = metric, adjust.revenue = adjusted)
#' @export
generate_catch_matrix <- function(mydata, metric = "revenue", gear = TRUE, NAto0 = TRUE, adjust.revenue = TRUE){
  y <- unique(mydata$crab_year)
  if(length(y) > 2){
    stop("ERROR: your data contains more than two years of fish tickets.")
  }
  if(metric == "revenue"){
    if(gear){
      message("Generated matrix using revenue with gear groups")
      output.melted <- mydata %>%
        mutate(SPECIES_GEAR = paste0(spid_recode, "_", grgroup)) %>%
        group_by(drvid_year, trip_id, removal_type, pcgroup, SPECIES_GEAR) %>%
        summarise(rev = ifelse(adjust.revenue == TRUE, sum(adj_revenue), sum(revenue))) %>%
        mutate(header = paste0(SPECIES_GEAR, ".rev")) %>%
        ungroup() %>%
        dplyr::select(-SPECIES_GEAR)
    } else{
      message("Generated matrix using revenue WITHOUT gear groups")
      output.melted <- mydata %>%
        group_by(drvid_year, trip_id, removal_type, pcgroup, spid_recode) %>%
        summarise(rev = ifelse(adjust.revenue == TRUE, sum(adj_revenue), sum(revenue))) %>%
        mutate(header = paste0(spid_recode, ".rev")) %>%
        ungroup() %>%
        dplyr::select(-spid_recode)
    }
    output <- pivot_wider(output.melted, names_from=header, values_from = rev)
    
  } else if(metric == "lbs"){
    if(gear == TRUE){
      message("Generated matrix using landed pounds, by gear group")
      output.melted <- mydata %>%
        mutate(SPECIES_GEAR = paste0(spid_recode, "_", grgroup)) %>%
        group_by(drvid_year, trip_id, removal_type, pcgroup, SPECIES_GEAR) %>%
        summarise(lbs = sum(pounds)) %>%
        mutate(header = paste0(SPECIES_GEAR, ".lbs")) %>%
        ungroup() %>%
        dplyr::select(-SPECIES_GEAR)
    } else{
      message("Generated matrix using landed pounds WITHOUT gear groups")
      output.melted <- mydata %>%
        group_by(drvid_year, trip_id, removal_type, pcgroup, spid_recode) %>%
        summarise(lbs = sum(pounds)) %>%
        mutate(header = paste0(spid_recode, ".lbs")) %>%
        ungroup() %>%
        dplyr::select(-spid_recode)
    }
    output <- pivot_wider(output.melted, names_from=header, values_from =lbs)
    
  } else{message("ERROR: metric not recognized")}
  
  ## remove NAs if desired
  if(NAto0 == TRUE){
    message("Replaced NAs with zeros")
    output <- output %>%
      replace(is.na(.), 0)
  }
  return(list(output.melted, output))
}
