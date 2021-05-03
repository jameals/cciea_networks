#' Filter Catch Matrix
#'
#' Filter the catch matrix (rows= trips, columns = species/gear combos, values = revenue or pounds)
#' to remove non-commercial landings records or 'rare' species/gear combinations.
#' Script 01, Section 1.4
#'
#' @param mydata catch matrix
#' @param commercial If TRUE, filter for only commercial landings records
#' @param rare  proportion or number of trips required to retain species/ gear combination
#' @param type If 'fraction', rare parameter is a proportion of trips; If 'number', rare parameter is a number of trips
#' @param ids character vector of ID column names
#' @return filtered catch matrix
#' @examples
#' final.output <- filter_matrix(data = output, commercial = filter_type, rare = filter_rare)
#' @export
filter_matrix <- function(mydata, commercial = TRUE, rare = 0.0005, type = "fraction", ids = c("drvid_year", "trip_id", "removal_type", "pcgroup")){
  orig_dim <- dim(mydata)
  n.ids <- length(ids)
  if(commercial){
    mydata <- mydata %>%
      filter(removal_type %in% c("COMMERCIAL (NON-EFP)","COMMERCIAL (DIRECT SALES)"))
    rows_removed <- orig_dim[1] - dim(mydata)[1]
    message("Filter for Commercial Tickets: removed ", rows_removed, " (", rows_removed/orig_dim[1] * 100 ,"% ) rows.\n")
  }
  if(!is.na(rare)){
    if(type == "fraction"){
      message("Rare species filter: species must appear in at least ", dim(mydata)[1]*rare, " trips...")
      
      ## separate trip identifiers, values
      mydata_ids <- mydata[,1:n.ids]
      mydata_abund <- mydata[,(n.ids+1):length(colnames(mydata))]
      
      ## run vegtab to remove rare species or species/gear combos
      mydata_abund.subset <- vegtab(mydata_abund, minval=rare*nrow(mydata_abund))
      
      ## add trip identifiers back into mydata frame
      mydata.subset <- cbind(mydata_ids, mydata_abund.subset)
      message("removed ", ncol(mydata_abund) - ncol(mydata_abund.subset), " species from mydata.\n")
      ## melt mydata frame
      id.index <- which(colnames(mydata.subset)%in%ids)
      mydata.melt <- pivot_longer(mydata.subset, cols=colnames(mydata.subset[,-id.index]))
      
      # spread temp df to create new output matrix
      mydata <- pivot_wider(mydata.melt, id_cols=all_of(ids), names_from=name)
      
      # remove any trips that don't have mydata
      mydata <- mydata %>%  mutate(total.rev = rowSums(mydata[, (n.ids+1):length(colnames(mydata))])) %>%
        filter(total.rev > 0) %>%
        dplyr::select(-total.rev)
    } else if(type == "number"){
      message("Rare species filter: species must appear in at least ", rare, " trips...")
      
      ## separate trip identifiers, values
      mydata_ids <- mydata[,1:n.ids]
      mydata_abund <- mydata[,(n.ids+1):length(colnames(mydata))]
      
      ## run vegtab to remove rare species or species/gear combos
      mydata_abund.subset <- vegtab(mydata_abund, minval=rare)
      
      ## add trip identifiers back into mydata frame
      mydata.subset <- cbind(mydata_ids, mydata_abund.subset)
      message("removed ", ncol(mydata_abund) - ncol(mydata_abund.subset), " species from mydata.\n")
      ## melt mydata frame
      id.index <- which(colnames(mydata.subset)%in%ids)
      mydata.melt <- pivot_longer(mydata.subset, cols=colnames(mydata.subset[,-id.index]))
      
      # spread temp df to create new output matrix
      mydata <- pivot_wider(mydata.melt, id_cols=all_of(ids), names_from=name)
      
      # remove any trips that don't have mydata
      if(length(colnames(mydata)) == n.ids+1){
        mydata <- mydata %>%  mutate(total.rev = mydata[, n.ids+1]) %>%
          filter(total.rev > 0) %>%
          dplyr::select(-total.rev)
      } else{
        mydata <- mydata %>%  mutate(total.rev = rowSums(mydata[, (n.ids+1):length(colnames(mydata))])) %>%
          filter(total.rev > 0) %>%
          dplyr::select(-total.rev)
      }
    }
  }
  return(mydata)
}

