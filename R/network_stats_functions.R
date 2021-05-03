#' Functions for Network Statistics
#'
#' These functions calculate the following network statistics: (1) beta efficiency, 
#' network centralization (unweighted), (3) network centralization (weighted). They
#' are called within the net_stats function. Network #' centralization code is 
#' based on equations from Cinner & Bodin (2010), doi: 10.1371/journal.pone.0011999
#'
#' @param g igraph object
#' @return numeric value
#' @examples
#' 
#' @export
beta_eff = function(g){
  if(is.null(E(g)$weight)){
    beta_eff = NA
  }else{
    beta_eff = mean(E(g)$weight) + var(E(g)$weight)/mean(E(g)$weight)
  }
  return(beta_eff)
}

net_centr = function(g){
  deg = degree(g)
  max_deg = max(deg)
  denom = (vcount(g) - 2) * (vcount(g) - 1)
  numer = 0
  for(d in deg){
    numer = numer + (max_deg - d)
  }
  return(numer/denom)
}

net_centr_weighted = function(g){
  deg=strength(g)
  max_deg=max(deg)
  pos_deg = deg[which(deg >0)]
  denom = ((vcount(g)-2)*(vcount(g)-1)) * mean(pos_deg)
  numer=0
  for(d in deg){
    numer = numer + (max_deg - d)
  }
  return(numer/denom)
}