#' Collapse Confidential Fisheries, Late Season Directed Network
#'
#' Collapse confidential fisheries (< 3 vessels) in the late season directed
#' networks. Confidential Pot / Hook and Line fisheries are collapsed to an 
#' 'other, pot/hkl' fishery, unless a self-loop has more than three vessels. 
#' If there are no confidential pot / hook and line fisheries, or if aggregating 
#' those fisheries still results in participation of < 3 vessels, confidential 
#' fisheries are collapsed into a more generic 'other' fishery. For Script 09b.
#'
#' @param A confidential adjacency matrix from `gen_adj_matrix_late` function
#' @param vessel_matrix confidential vessel participation matrix from `gen_adj_matrix_late` function
#' @param no_drop specify which nodes *not* to collapse (if confidential will just be dropped)
#' @param default_other when collapsing multiple fisheries into an "other" group, a single vessel can have multiple matrix values (-1,1,2). when this happens, default to a specific value, or use "mode"
#' @return a list with (1) A: adjacency matrix with non-confidential data to create directed network, (2 & 3) vpf_2014 & vpf_2015: vessels participating in each fishery / aggregate in 2014-15 and 2015-16
#' @examples
#' Anew <- collapse_confidential_late(A=A,vessel_matrix = vesselmat, default_other=2)
#' @export
collapse_confidential_late <- function(A, vessel_matrix, no_drop=c("DCRB_POT","other_port","no_fishing"), default_other = "mode"){
  
  ########## ID Confidential Data to Collapse ##########
  # grab the row which contains self-loops; columns where the total is smaller than 3
  diag_row <- matrix(diag(A),nrow=1,dimnames=list("",colnames(A)))
  row_totals <- matrix(rowSums(A),nrow=1,dimnames=list("",colnames(A))) 
  col_totals <- matrix(colSums(A),nrow=1,dimnames=list("",colnames(A))) 
  # which metiers are confidential?
  to_collapse_self <- diag_row[,which(diag_row < 3),drop=FALSE]  # changed from "which(diag_row > 0 & diag_row < 3)" for cleaner output
  to_collapse_rows <- row_totals[,which(row_totals < 3),drop=FALSE]  # changed from "which(row_totals > 0 & row_totals < 3)" for cleaner output
  # pull confidential metiers from columns only if rowsum == 0
  to_collapse_cols <- col_totals[,which(col_totals < 3),drop=FALSE]  # changed from "which(col_totals > 0 & col_totals < 3)" for cleaner output
  rowzero <- row_totals[,which(row_totals == 0),drop=FALSE]
  to_collapse_cols <- to_collapse_cols[,which(colnames(to_collapse_cols) %in% colnames(rowzero)),drop=FALSE]
  # keep metiers from argument "no drop"
  if(any(no_drop %in% colnames(to_collapse_self))){to_collapse_self <- to_collapse_self[,-which(colnames(to_collapse_self) %in% no_drop),drop=FALSE]}
  if(any(no_drop %in% colnames(to_collapse_rows))){to_collapse_rows <- to_collapse_rows[,-which(colnames(to_collapse_rows) %in% no_drop),drop=FALSE]}
  if(any(no_drop %in% colnames(to_collapse_cols))){to_collapse_cols <- to_collapse_cols[,-which(colnames(to_collapse_cols) %in% no_drop),drop=FALSE]}
  # add back in metiers that are not confidential in a non-self category. This means confidential self-loops will just be removed.
  keep_rows <- c()
  for(metier in colnames(to_collapse_self)){
    if(any(A[,metier] >= 3) | any(A[metier,] >= 3)){   # changed 10/29 from  if(any(A[,metier] > 3)){}
      keep_rows <- c(keep_rows, metier)
    }
  }
  keep_rows_index <- which(colnames(to_collapse_self) %in% keep_rows)
  if(length(keep_rows_index)>0){to_collapse_self <- to_collapse_self[,-keep_rows_index,drop=FALSE]}
  # merge confidential metiers from cols / self
  metiers_to_collapse <- unique(c(colnames(to_collapse_rows),colnames(to_collapse_self),colnames(to_collapse_cols)))
  ######################################################
  
  
  
  ########## Collapse Columns / Rows ##########
  if(class(vessel_matrix) == "matrix"){
    vessel_matrix <- as.data.frame(vessel_matrix)
    message("warning: converted the vessel matrix (matrix object) to data frame.")
    print(vessel_matrix)
  }
  ## grab the nonconfidential metiers
  vesselmat_noncon <- vessel_matrix %>% dplyr::select(-all_of(metiers_to_collapse))
  
  ##collapse part 1: by gear type. if there are multiple pot / hkl metiers with 3+ vessels combined, collapse those.
  pl_metiers <- metiers_to_collapse[which(grepl("POT",metiers_to_collapse) | grepl("HKL",metiers_to_collapse))]
  
  if(!is.null(pl_metiers) & length(pl_metiers) > 1){
    message("Created Other Hook & Line / Pot fishery")
    message(paste0(pl_metiers,collapse=", "))
    ## grab the confidential metiers
    vesselmat_con <- vessel_matrix %>% dplyr::select(all_of(pl_metiers))
    
    ## the easy part is combining metiers which have all the same numbers (i.e. participation type). 
    ##   but it's not possible to both stay in and leave a metier.  
    if(default_other=="mode"){
      ## whenever a vessel has different numbers, the *most frequent* among all vessels will be chosen. 
      col_modes <- apply(vesselmat_con[,pl_metiers], 2, getmode_nonzero)
      met_mode <- getmode_nonzero(col_modes)
      ## use the rowwise function to get one value per vessel for all combined metiers
      collapsed_values <- unlist(apply(vesselmat_con[,pl_metiers], 1, collapse_rowwise, met_mode))
    } else if(is.numeric(default_other)){
      ## use the rowwise function to get one value per vessel for all combined metiers
      collapsed_values <- unlist(apply(vesselmat_con[,pl_metiers], 1, collapse_rowwise, default_other))
    } else{
      stop("don't recognize default_other value. make sure you supply one of the following arguments: [1,-1,2,mode]")
    }
    ## add the new non-confidential metier to the non-confidential vessel matrix
    vesselmat_noncon <- vesselmat_noncon %>% bind_cols(OTHR_POT_HKL = collapsed_values)
    #remove these metiers from overall collapsed
    metiers_to_collapse <- metiers_to_collapse[which(!(metiers_to_collapse %in% pl_metiers))]
  } 
  ##collapse part 2: all else into an "other" category
  if(length(metiers_to_collapse) > 1){
    message("Created Other fishery")
    message(paste0(metiers_to_collapse,collapse=", "))
    ## grab the confidential metiers
    vesselmat_con <- vessel_matrix %>% dplyr::select(all_of(metiers_to_collapse))
    
    ## the easy part is combining metiers which have all the same numbers (i.e. participation type). 
    ##   but it's not possible to both stay in and leave a metier.  
    if(default_other=="mode"){
      ## whenever a vessel has different numbers, the *most frequent* among all vessels will be chosen. 
      col_modes <- apply(vesselmat_con[,metiers_to_collapse], 2, getmode_nonzero)
      met_mode <- getmode_nonzero(col_modes)
      ## use the rowwise function to get one value per vessel for all combined metiers
      collapsed_values <- unlist(apply(vesselmat_con[,metiers_to_collapse], 1, collapse_rowwise, met_mode))
    } else if(is.numeric(default_other)){
      ## use the rowwise function to get one value per vessel for all combined metiers
      collapsed_values <- unlist(apply(vesselmat_con[,metiers_to_collapse], 1, collapse_rowwise, default_other))
    } else{
      stop("don't recognize default_other value. make sure you supply one of the following arguments: [1,-1,2,mode]")
    }
    ## add the new non-confidential metier to the non-confidential vessel matrix
    vesselmat_noncon <- vesselmat_noncon %>% bind_cols(OTHR = collapsed_values)
  }
  
  #reorder the matrix so other_port is at the end
  vesselmat_noncon <- vesselmat_noncon %>% dplyr::select(colnames(vesselmat_noncon)[which(!(colnames(vesselmat_noncon) %in% c("X","other_port")))], "other_port")
  
  # get vessel counts for each metier in each year
  ## 2014 will be codes 2 and -1
  vpf_2014 <- unlist(apply(vesselmat_noncon, 2, function(x){sum(x==2) + sum(x==-1)}))
  vpf_2014 <- vpf_2014[-which(names(vpf_2014)=="other_port")]
  ## 2015 will be codes 2 and 1
  vpf_2015 <- unlist(apply(vesselmat_noncon, 2, function(x){sum(x==2) + sum(x==1)}))
  ######################################################
  
  
  
  ################ Fill in adjacency matrix ####################
  # convert to matrix for function
  vesselmatrix_noncon <- as.matrix(vesselmat_noncon)
  # call function to fill in adjacency matrix, with diagonal
  A <- fill_adjacency_matrix(vessel_matrix=vesselmatrix_noncon, self=TRUE)
  ######################################################
  
  
  
  ################ Check confidentiality of OTHER (pot,hl) ####################
  ####################### if there are two OTHR metiers #######################
  if("OTHR_POT_HKL" %in% colnames(A) & "OTHR" %in% colnames(A)){
    # is the self-loop confidential?
    pl_diag <- matrix(diag(A),nrow=1,dimnames=list("",colnames(A)))[,"OTHR_POT_HKL"]
    # are all row entries (minus the self entry) confidential?
    pl_row <- max(A["OTHR_POT_HKL", which(colnames(A)!="OTHR_POT_HKL")])
    # are all column entries (minus the self entry) confidential?
    pl_col <- max(A[which(rownames(A)!="OTHR_POT_HKL"),"OTHR_POT_HKL"])
    
    ## If all above are confidential, redo the groups to merge the two OTHER metiers ##
    if(all(c(pl_diag, pl_row,pl_col) < 3)){
      # reset metiers to collapse
      metiers_to_collapse <- c(pl_metiers, metiers_to_collapse)
      # reset nonconfidential matrix
      vesselmat_noncon <- vessel_matrix %>% dplyr::select(-all_of(metiers_to_collapse))
      
      message("Other (Pot,HL) fishery still confidential. Merging with OTHER.")
      message(paste0(metiers_to_collapse,collapse=", "))
      
      ## grab the confidential metiers
      vesselmat_con <- vessel_matrix %>% dplyr::select(all_of(metiers_to_collapse))
      
      ## the easy part is combining metiers which have all the same numbers (i.e. participation type). 
      ##   but it's not possible to both stay in and leave a metier.  
      if(default_other=="mode"){
        ## whenever a vessel has different numbers, the *most frequent* among all vessels will be chosen. 
        col_modes <- apply(vesselmat_con[,metiers_to_collapse], 2, getmode_nonzero)
        met_mode <- getmode_nonzero(col_modes)
        ## use the rowwise function to get one value per vessel for all combined metiers
        collapsed_values <- unlist(apply(vesselmat_con[,metiers_to_collapse], 1, collapse_rowwise, met_mode))
      } else if(is.numeric(default_other)){
        ## use the rowwise function to get one value per vessel for all combined metiers
        collapsed_values <- unlist(apply(vesselmat_con[,metiers_to_collapse], 1, collapse_rowwise, default_other))
      } else{
        stop("don't recognize default_other value. make sure you supply one of the following arguments: [1,-1,2,mode]")
      }
      ## add the new non-confidential metier to the non-confidential vessel matrix
      vesselmat_noncon <- vesselmat_noncon %>% bind_cols(OTHR = collapsed_values)
      
      #reorder the matrix so other_port is at the end
      vesselmat_noncon <- vesselmat_noncon %>% dplyr::select(colnames(vesselmat_noncon)[which(!(colnames(vesselmat_noncon) %in% c("X","other_port")))], "other_port")
      
      # get vessel counts for each metier in each year
      ## 2014 will be codes 2 and -1
      vpf_2014 <- unlist(apply(vesselmat_noncon, 2, function(x){sum(x==2) + sum(x==-1)}))
      vpf_2014 <- vpf_2014[-which(names(vpf_2014)=="other_port")]
      ## 2015 will be codes 2 and 1
      vpf_2015 <- unlist(apply(vesselmat_noncon, 2, function(x){sum(x==2) + sum(x==1)}))
      
      ################ Fill in adjacency matrix ####################
      # convert to matrix for function
      vesselmatrix_noncon <- as.matrix(vesselmat_noncon)
      # call function to fill in adjacency matrix, with diagonal
      A <- fill_adjacency_matrix(vessel_matrix=vesselmatrix_noncon, self=TRUE)
      ##############################################################
    }
  }
  
  
  
  ################ Clean up adjacency matrix ####################
  # remove remaining confidential data
  for(j in seq(1,dim(A)[2])){
    A[ A[ , j ] < 3 , j ] <- 0
  }
  # remove empty metiers
  empty_cols=names(which(colSums(A)==0)); empty_cols <- empty_cols[which(!(empty_cols %in% c("other_port","no_fishing")))]
  empty_rows=names(which(rowSums(A)==0)); empty_rows <- empty_rows[which(!(empty_rows %in% c("other_port","no_fishing")))]
  empty_metiers <- empty_cols[which(empty_cols %in% empty_rows)]
  A <- A[!(rownames(A) %in% empty_metiers),!(colnames(A) %in% empty_metiers)]
  
  # revise vessel counts for other port / no fishing in 2015
  vpf_2015 <- vpf_2015[which(names(vpf_2015) %in% colnames(A)[which(colnames(A) != "other_port")])]
  vpf_dropped <- colSums(A[,c("other_port","no_fishing")])
  vpf_2015 <- c(vpf_2015,vpf_dropped)
  
  ######################################################
  
  # return a list with the non-confidential adjacency matrix, and vessel counts for each crab year
  return(list(A,vpf_2014,vpf_2015))
}



