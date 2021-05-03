#' Split Catch Matrix by Gear Group
#'
#' For computational reasons, distances between trips are calculated by gear group.
#' his function will split the full catch matrix into a list of matrices, by gear group.
#' For script 01, section 3.1
#'
#' @param gear.list character vector of gear groups from the landings input data
#' @param catch_matrix catch matrix from 'generate_catch_matrix'
#' @return (1) a list of catch matrices with only revenue / lbs split by gear group, (2) a list of matrices with identifying information for 1st outpu, (3) a revised character vector of gear types found in the catch matrix
#' @examples
#' catchmats_bygear <- split_matrix(gear.list = gear_list, catch_matrix = inmat)
#' @export
split_matrix <- function(gear.list, catch_matrix){
  rev_bygear_list <- list()
  ids_bygear_list <- list()
  gear_to_remove <- c()
  for(i in seq(1, length(gear.list))){
    g <- gear.list[i]
    ug <- paste0("_",g) # for MSC gear type, avoids match with MSC species
    # grab the species/gear columns with the given gear group
    tmp_cols <- as.data.frame(colnames(catch_matrix)) %>%
      filter(grepl(ug, colnames(catch_matrix)) == TRUE)
    if(dim(tmp_cols)[1] > 0){
      # subset the columns in the original data
      tmp_mat <- catch_matrix[,c(as.character(tmp_cols$`colnames(catch_matrix)`)), drop=FALSE]
      # get row identifiers to keep (those with some data in given gear group)
      trips_to_keep <- tmp_mat %>%
        mutate(indices = as.numeric(rownames(tmp_mat))) %>%
        mutate(total_rev = rowSums(.)) %>%
        filter(total_rev > indices) 
      # select rows from above from the full data
      gear_mat <- slice(catch_matrix, trips_to_keep$indices)
      # save columns with revenue from given gear, associated identifiers
      gear_mat_rev <- as.matrix(gear_mat[,c(as.character(tmp_cols$`colnames(catch_matrix)`))])
      gear_mat_ids <- gear_mat[,c(as.character(colnames(catch_matrix)[1:4]))]
      # put each data frame into a list
      rev_bygear_list[[i]] <- gear_mat_rev
      ids_bygear_list[[i]] <- gear_mat_ids
      message("trips associated with ", as.character(g), " gear added to lists.\n")
    } else{
      message("trips associated with ", as.character(g), " gear were removed during previous filtering. gear type not added to lists.\n")
      gear_to_remove <- c(gear_to_remove, as.character(g))
    }
  }
  
  if(length(gear_to_remove) > 0){
    if(is.na(gear_to_remove)){
      gear.list <- droplevels(gear.list)
      gear.list <- gear.list[!is.na(gear.list)]
    } else{
    gear.list <- droplevels(gear.list)
    gear.list <- gear.list[!gear.list %in% grep(paste0(gear_to_remove, collapse = "|"), gear.list, value = T)]
    }
    rev_bygear_list <- rev_bygear_list[lapply(rev_bygear_list,length)!=0] #added 2/18. cannot use = NULL
    ids_bygear_list <- ids_bygear_list[lapply(ids_bygear_list,length)!=0] #added 2/18. cannot use = NULL
  }
  
  return(list(rev_bygear_list, ids_bygear_list, gear.list))
}
