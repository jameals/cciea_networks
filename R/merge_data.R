#' Merge to Similiarities Data Frame
#'
#' For computational reasons, similarities between trips are calculated by gear group.
#' This function merges all gear group data frames to one. For script 01, section 4.1
#'
#' @param gear.list character vector of all gear group types in the landings input data
#' @param sim.list list of data frames with pairwise trip similarities for each gear group
#' @param ids.list list of data frames with identifying information for each trip in the sim.list
#' @return a data frame with all pairwise trip similarities + gear group column
#' @examples
#' similarities <- merge_df(gear.list = gear_list_filtered, sim.list = similarities_df_list, ids.list = ids_bygear_list)
#' @export
merge_df <- function(gear.list, sim.list, ids.list){
  for(i in seq(1, length(gear.list))){
    # get all the objects together by subsetting the list of gear types, similarities data frames, and ids data frames
    g <- gear.list[i]
    tmp_sim <- sim.list[[i]]
    tmp_ids <- ids.list[[i]]
    
    # create new columns: 
    ## first, a repetitive list of the gear type for the final output
    ## second, an "indices" column for the tmp_ids data frame based on the row numbers
    gear_col <- rep(g, times=length(tmp_sim$row))
    tmp_ids <- tmp_ids %>%
      tibble::rownames_to_column() %>%
      dplyr::select(trip_id, rowname)
    tmp_sim <- tmp_sim %>%
      dplyr::select(-value)
    
    # match the trip_ids from the ids df to the sim df using "index" column
    tmp_ids$rowname <- as.numeric(tmp_ids$rowname)
    tmp_out <- left_join(tmp_sim, tmp_ids, by=c("row" = "rowname"))
    tmp_out <- dplyr::select(tmp_out, c(trip_id, col, value.sim))
    colnames(tmp_out)[1] <- "trip1"
    tmp_out <- left_join(tmp_out, tmp_ids, by=c("col" = "rowname"))
    tmp_out <- dplyr::select(tmp_out, c(trip1, trip_id, value.sim))
    colnames(tmp_out)[2] <- "trip2"
    
    # add in the gear group
    tmp_out <- mutate(tmp_out, gear = gear_col)
    
    if(i == 1){
      similarities <- tmp_out
    } else{
      if(all(colnames(tmp_out) == colnames(similarities))){
        similarities <- rbind(similarities, tmp_out)
      } else{
        stop("ERROR: output dataframe for gear group ", g, " does not match data frame for previous gear groups. Failed to append to final data frame.")
      }
    } #end (else i != 1)
    rm(tmp_out, tmp_ids, tmp_sim, g)
  } #end(for i)
  
  return(similarities)
}

merge_mat <- function(gear.list, sim.list, ids.list){
  for(i in seq(1, length(gear.list))){
    # get all the objects together by subsetting the list of gear types, similarities data frames, and ids data frames
    g <- gear.list[i]
    tmp_sim <- sim.list[[i]]
    tmp_ids <- ids.list[[i]]
    
    # create new columns: 
    ## first, a repetitive list of the gear type for the final output
    ## second, an "indices" column for the tmp_ids data frame based on the row numbers
    gear_col <- rep(g, times=length(tmp_sim$row))
    tmp_ids <- tmp_ids %>%
      tibble::rownames_to_column() %>%
      dplyr::select(trip_id, rowname)
    tmp_sim <- tmp_sim %>%
      dplyr::select(-value)
    
    # change to matrices to save space
    tmp_ids <- data.matrix(tmp_ids)
    tmp_sim <- data.matrix(tmp_sim)
    
    # match the trip_ids from the ids df to the sim df using "index" column
    tmp_out <- merge.Matrix(x=tmp_sim, y=tmp_ids, by.y = "rowname", by.x = "row")
    tmp_out <- tmp_out[,c("row", "col", "value.sim", "trip_id")]
    colnames(tmp_out)[4] <- "trip1"
    tmp_out <- merge.Matrix(x=tmp_out, y=tmp_ids, by.x = "rowname", by.y = "col")
    tmp_out <- tmp_out[,c("trip1","trip_id","value.sim")]
    colnames(tmp_out)[2] <- "trip2"
    
    # add in the gear group
    tmp_out <- as.data.frame(tmp_out)
    
    if(i == 1){
      similarities <- tmp_out
    } else{
      if(all(colnames(tmp_out) == colnames(similarities))){
        similarities <- rbind(similarities, tmp_out)
      } else{
        stop("ERROR: output dataframe for gear group ", g, " does not match data frame for previous gear groups. Failed to append to final data frame.")
      }
    } #end (else i != 1)
  } #end(for i)
  
  return(similarities)
}