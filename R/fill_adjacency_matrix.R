#' Fill an adjacency matrix
#'
#' Fill a fisheries x fisheries adjacency matrix, the foundation
#' of the network graphs. 
#' Called by the `collapse_confidential_late` function.
#'
#' @param vessel_matrix a matrix with vessels in rows, metiers in columns, and participation coded as (0,1,-1,2)
#' @param self record continued participation in a fishery? (represented by the diagonal in the adjacency matrix, and a self-loop in the network graph)
#' @return a fisheries x fisheries adjacency matrix, with cell entries as vessel counts
#' @examples
#' collapsed_values <- unlist(apply(vesselmat_con[,pl_metiers], 1, collapse_rowwise, default_other))
#' @export
fill_adjacency_matrix <- function(vessel_matrix, self=TRUE){
  #Empty matrix
  fisheries <- colnames(vessel_matrix)
  n.metiers <- length(fisheries) -1
  fisheries <- c(fisheries, "no_fishing")
  A <- matrix(ncol = length(fisheries), nrow = length(fisheries), data = 0)
  colnames(A) <- fisheries
  rownames(A) <- fisheries
  #Add in values
  other_port_vessels <- c()
  for(i in seq(1,dim(vessel_matrix)[1])){
    tmpdat=vessel_matrix[i,]
    tmpdat_metiers <- vessel_matrix[i,1:n.metiers]
    # which fisheries did this vessel leave / enter?
    enter_fishery <- names(which(tmpdat_metiers == 1))
    left_fishery <- names(which(tmpdat == -1))
    if(self){
      remain_fishery <- names(which(tmpdat == 2))
    } else{
      remain_fishery <- c() ## UPDATED JAN 07, 2020 - see below
    }
    # for each fishery the vessel left...
    for(j in left_fishery){
      ## if it didn't enter a fishery, add one to no landings or other port
      if(length(enter_fishery) == 0 & length(remain_fishery)==0){  ## UPDATED JAN 07, 2020 - prevents 1 being added to "no fishing" if vessel just remained in fisher(ies)
        ### if there were landings at another port, add 1 to "none"-->"other". otherwise, add 1 to "none"-->"no_fishing"
        if(tmpdat["other_port"]==1){
          A[j,"other_port"] <- A[j,"other_port"] + 1
          other_port_vessels <- c(other_port_vessels, rownames(vessel_matrix)[i])
        } else{
          A[j,"no_fishing"] <- A[j,"no_fishing"] + 1
        }
      } else{
        if("DCRB_POT" %in% enter_fishery){ # record if a vessel moved into d.crab when it left another fishery
          A[j,"DCRB_POT"] <- A[j,"DCRB_POT"] + 1
        } else if(j=="DCRB_POT"){ # record if a vessel moved out of d.crab 
          for(k in enter_fishery){
            A[j,k] <- A[j,k] + 1
          } 
        }
      } #end else
    } #end for(j)
    if(self){
      for(l in remain_fishery){
        A[l,l] <- A[l,l] + 1
      }
    } #end self
  } #end for(i)
  return(A)
}

