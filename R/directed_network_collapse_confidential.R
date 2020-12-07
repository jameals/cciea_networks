#' Collapse Confidential Fisheries, Early Season Directed Network
#'
#' Collapse confidential fisheries (< 3 vessels) in the early season directed
#' networks. Confidential Pot / Hook and Line fisheries are collapsed to an 
#' 'other, pot/hkl' fishery, unless a self-loop has more than three vessels. 
#' If there are no confidential pot / hook and line fisheries, or if aggregating 
#' those fisheries still results in participation of < 3 vessels, confidential 
#' fisheries are collapsed into a more generic 'other' fishery. For Script 09a.
#'
#' @param A confidential adjacency matrix from `gen_adj_matrix_early` function
#' @param vpf_2014 data frame with 2014-15 vessel counts per fishery from `gen_adj_matrix_early` function
#' @param vpf_2015 data frame with 2015-16 vessel counts per fishery from `gen_adj_matrix_early` function
#' @param no_drop specify which nodes *not* to collapse (if confidential will just be dropped)
#' @return a list with (1) A: adjacency matrix with non-confidential data to create directed network, (2 & 3) vpf_2014 & vpf_2015: vessels participating in each fishery / aggregate in 2014-15 and 2015-16
#' @examples
#' new_A_info <- collapse_confidential_early(A=A,vpf_2014=vpf_2014,vpf_2015=vpf_2015)
#' @export
collapse_confidential_early <- function(A, vpf_2014,vpf_2015, no_drop=c("DCRB_POT","other_port","no_fishing")){
  # ID Confidential Data to Collapse #
  # grab the row which contains info about vessels moving out of d. crab fishery, and self-loops
  dcrb_row <- A["DCRB_POT",,drop=FALSE]
  diag_row <- matrix(diag(A),nrow=1,dimnames=list("",colnames(dcrb_row)))
  # which metiers are confidential?
  to_collapse_dcrb <- dcrb_row[,which(dcrb_row > 0 & dcrb_row < 3),drop=FALSE]
  to_collapse_self <- diag_row[,which(diag_row > 0 & diag_row < 3),drop=FALSE]
  # keep metiers from argument "no drop"
  if(any(no_drop %in% colnames(to_collapse_self))){to_collapse_self <- to_collapse_self[,-which(colnames(to_collapse_self) %in% no_drop),drop=FALSE]}
  if(any(no_drop %in% colnames(to_collapse_dcrb))){to_collapse_dcrb <- to_collapse_dcrb[,-which(colnames(to_collapse_dcrb) %in% no_drop),drop=FALSE]}
  # add back in metiers that are not confidential in EITHER dcrb flow or continued participation ("self")
  keep_dcrb <- dcrb_row[,which(dcrb_row > 2),drop=FALSE]; keep_dcrb_index <- which(colnames(to_collapse_self) %in% colnames(keep_dcrb))
  keep_self <- diag_row[,which(diag_row > 2),drop=FALSE]; keep_self_index <- which(colnames(to_collapse_dcrb) %in% colnames(keep_self))
  if(length(keep_dcrb_index)>0){to_collapse_self <- to_collapse_self[,-keep_dcrb_index,drop=FALSE]}
  if(length(keep_self_index)>0){to_collapse_dcrb<-to_collapse_dcrb[,-keep_self_index,drop=FALSE]}
  # merge confidential metiers from dcrb / self
  to_collapse <- cbind(to_collapse_dcrb,to_collapse_self)
  to_collapse <- t(rowsum(t(to_collapse), colnames(to_collapse))) #in case there are metiers repped in both diag and dcrb row
  metiers_to_collapse <- colnames(to_collapse)
  
  # Collapse Columns / Rows into "Other" #
  ##collapse part 1: by gear type. if there are multiple pot / hkl metiers with 3+ vessels combined, collapse those.
  pl_metiers_self <- unlist(lapply(colnames(to_collapse_self),function(x){grepl("POT",x) | grepl("HKL",x)}))
  pl_metiers_dcrb <- unlist(lapply(colnames(to_collapse_dcrb),function(x){grepl("POT",x) | grepl("HKL",x)}))
  if(!is.null(pl_metiers_self)){pl_collapse_self <- to_collapse_self[,which(pl_metiers_self),drop=FALSE]} else{pl_collapse_self=0}
  if(!is.null(pl_metiers_dcrb)){pl_collapse_dcrb <- to_collapse_dcrb[,which(pl_metiers_dcrb),drop=FALSE]} else{pl_collapse_dcrb=0}
  if((sum(pl_collapse_self) > 0 & sum(pl_collapse_self) > 2) | (sum(pl_collapse_dcrb) > 0 & sum(pl_collapse_dcrb) > 2) ){
    message("Created Other Hook & Line / Pot fishery")
    pl_metiers <- colnames(to_collapse)[which(unlist(lapply(colnames(to_collapse),function(x){grepl("POT",x) | grepl("HKL",x)})))]
    message(paste0(pl_metiers,collapse=", "))
    pl_collapse <- to_collapse[,which(colnames(to_collapse) %in% pl_metiers),drop=FALSE]
    #remove these metiers from overall collapsed
    to_collapse <- to_collapse[,which(!(colnames(to_collapse) %in% pl_metiers)),drop=FALSE]
    #subset matrix and create "other pot" metier
    pl_data <- matrix(rowSums(A[,pl_metiers]), ncol=1,dimnames=list(rownames(A),c("OTHR_POT_HKL"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% pl_metiers))]; A <- cbind(A,pl_data) #subtract old metier info / add in new
    pl_data <- matrix(colSums(A[pl_metiers,]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR_POT_HKL"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% pl_metiers)),]; A <- rbind(A,pl_data) #subtract old metier info / add in new
  }
  ##collapse part 2: all else into an "other" category
  if(length(to_collapse) > 0){
    other_data <- matrix(rowSums(A[,colnames(to_collapse),drop=FALSE]), ncol=1,dimnames=list(rownames(A),c("OTHR"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% colnames(to_collapse)))]; A <- cbind(A,other_data) #subtract old metier info / add in new
    other_data <- matrix(colSums(A[colnames(to_collapse),,drop=FALSE]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% colnames(to_collapse))),]; A <- rbind(A,other_data) #subtract old metier info / add in new
  }
  
  # Re order matrix
  added_columns <- which(colnames(A) %in% c("OTHR_POT_HKL","OTHR"))
  end_columns <- which(colnames(A) %in% c("other_port","no_fishing"))
  start_columns <- which(!(seq(1,dim(A)[1]) %in% c(added_columns,end_columns)))
  new_order <- c(start_columns, added_columns, end_columns)
  A <- A[new_order,new_order]
  
  # Re-calculate vessels per fishery #
  if((sum(pl_collapse_self) > 0 & sum(pl_collapse_self) > 2) | (sum(pl_collapse_dcrb) > 0 & sum(pl_collapse_dcrb) > 2) ){
    vpf_op_2014 <- sum(vpf_2014[pl_metiers]); names(vpf_op_2014) <- "OTHR_POT_HKL"
    vpf_op_2015 <- sum(vpf_2015[pl_metiers]); names(vpf_op_2015) <- "OTHR_POT_HKL"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% pl_metiers)]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% pl_metiers)]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  if(length(to_collapse) > 0){
    if(is.null(names(to_collapse))){    ### added Oct 2020 to deal with alternative object types for to_collapse ###
      vpf_op_2014 <- sum(vpf_2014[colnames(to_collapse)]); names(vpf_op_2014) <- "OTHR"
      vpf_op_2015 <- sum(vpf_2015[colnames(to_collapse)]); names(vpf_op_2015) <- "OTHR"
      vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% colnames(to_collapse))]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
      vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% colnames(to_collapse))]; vpf_2015 <- c(vpf_2015,vpf_op_2015)      
    }
    vpf_op_2014 <- sum(vpf_2014[names(to_collapse)]); names(vpf_op_2014) <- "OTHR"
    vpf_op_2015 <- sum(vpf_2015[names(to_collapse)]); names(vpf_op_2015) <- "OTHR"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% colnames(to_collapse))]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% colnames(to_collapse))]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  return(list(A,vpf_2014,vpf_2015))
  
  
}
