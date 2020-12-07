#' Collapse Individual Vessel Participation in Confidential Fisheries, Late Season Directed Network
#'
#' Collapses a row (one vessel) of pre-defined confidential fisheries in the 
#' late season directed networks. 
#' Called by the `collapse_confidential_late` function.
#'
#' @param x a row of participation codes (1,-1,2,0) representing a single vessel
#' @param prioritize if a vessel has multiple participation codes for a single collapsed node, and there is no true mode, which code should be returned?
#' @return a single participation code for a collapsed network node
#' @examples
#' collapsed_values <- unlist(apply(vesselmat_con[,pl_metiers], 1, collapse_rowwise, default_other))
#' @export
collapse_rowwise <- function(x, prioritize=met_mode){
  vals <- unique(x)
  if(length(vals)==1){
    # message("unique")
    return(vals)
  } else{
    vals <- vals[which(vals!=0)]
    if(length(vals)==1){
      # message("unique")
      return(vals)
    } else{
      if(prioritize %in% vals){
        val <- vals[which(vals==prioritize)]
        # message("prioritized")
        return(val)        
      } else if(length(unique(tabulate(vals))) > 1){
        val <- getmode(vals)
        # message("mode")
        return(val) 
      } else{
        # prioritize continued activity, then entry, then exit
        if(2 %in% vals){return(2)} else if(1 %in% vals){return(1)} else if(-1%in% vals){return(-1)}else{}
      }
    }
  }
}
