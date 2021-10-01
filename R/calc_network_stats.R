#' Calculate Network Statistics
#'
#' Calculate all network-level and node-level statistics for a given graph.
#'
#' @param g network, as an igraph object
#' @param y the year or crab year represented by the network
#' @param stats Specify whether you want to calculate node-level stats ('node'), network-level stats ('network') or both
#' @return data frame with statistics
#' @examples
#' stats.net <- net_stats(open_g, y = y, stats="network")
#' stats.node <- net_stats(open_g, y = y, stats="node")
#' stats.all <- net_stats(open_g, y = y, stats="both")
#' @export
net_stats <- function(g, y, stats="both"){
  deg_norm = degree(g, normalized=TRUE) #get degree centralities
  strength_rel = strength(g)/sum(E(g)$weight) #individual node strength
  # by node
  if(stats == "node" | stats == "both"){
    # identity variables for data frame
    year <- rep(y, length(V(g)$name))
    metier = V(g)$name
    gear=as.character(lapply(strsplit(metier,"_"), function(x){return(x[2])}))
    metier.cm = V(g)$common_name
    # node-level metrics
    node_strength = strength(g)/sum(E(g)$weight) #individual node strength
    eigen = eigen_centrality(g, weights = E(g)$weight)$vector #eigenvector centrality
    if(length(E(g)$weight) < 1){
      message("WARNING: No edges in this network.")
      btwn=NA
      btwn_inverted=NA
      basic_btwn =  betweenness(g, directed = FALSE, weights = NULL, normalized = TRUE)
    } else{
      # calculate betweenness centrality
      btwn_norm = betweenness(g, directed = FALSE, weights = E(g)$weight, normalized = TRUE)  # basic betweenness function, normalized by number of vertices
      ew = 1/E(g)$weight   # in the betweenness function, edge weights are costs, not strengths. get inverse of edge weights to use them as strengths in the fx
      btwn_invert = betweenness(g, directed = FALSE, weights = ew, normalized = TRUE)  # use 1/E(g)$weight to get betweenness with edges as strengthes
      ew2 = mean(E(g)$weight, na.rm = T)/E(g)$weight # scale the inverted edge weights: ew * 1/mean(weight)
      btwn_invert_scaled = betweenness(g, directed = FALSE, weights = ew2, normalized = TRUE) # scaled betweenness centrality with edges as strengthes
      # calculate the average nearest neighbor degree (Barrat et al. 2004 PNAS)
      knn_degree = knn(g, weights = E(g)$weight)
    }
    # save node-level stats as data frame
    node_stats <- as.data.frame(cbind(year, metier, gear, metier.cm, node_strength, eigen,btwn,btwn_inverted,basic_btwn,deg_norm))
  }
  
  if(stats=="network" | stats == "both"){
    # network-level metrics 
    beta_eff = beta_eff(g)
    assort = assortativity_degree(g, directed = FALSE) #assortativity
    ld = length(E(g))/length(V(g)) #link density
    ed = edge_density(g) #edge density
    ed_weighted = sum(E(g)$weight)
    N = vcount(g) #number of metiers
    E = ecount(g) #number of edges
    if(E > 1){
      avg_ew = mean(E(g)$weight, na.rm = T) #average edge weight
      sd_ew = sd(E(g)$weight, na.rm = T)
      med_ew = median(E(g)$weight, na.rm = T) #median edge weight
      max_ew = max(E(g)$weight, na.rm = T) #maximum edge weight
      total_ew = sum(E(g)$weight, na.rm = T) #added MF 2/26/2019
      avg_ew_scaled = avg_ew/total_ew #added MF 2/26/2019  NEED TO EDIT DENOMINATOR
      sd_ew_scaled = sd_ew/total_ew #added MF 2/26/2019
      med_ew_scaled = med_ew/total_ew #added MF 2/26/2019
    } else{
      avg_ew = NA #average edge weight
      sd_ew = NA
      med_ew = NA #median edge weight
      max_ew = 0 #maximum edge weight
      avg_ew_scaled = NA
      sd_ew_scaled = NA
      med_ew_scaled = NA
    }
    deg_max = max(deg_norm) #max degree centrality
    deg_min = min(deg_norm) #min degree centrality
    mean_deg = (2*E)/N #mean degree, Lorien Jasny
    strength_mean = mean(strength_rel) # mean node strength
    # network centralization
    nc = net_centr(g) #no edge weights
    nc_weighted = net_centr_weighted(g) #weighted nc
    # modularity
    wtc <- cluster_walktrap(g, weights = E(g)$weight) #find densely connected subgraphs via random walk
    m <- modularity(g, membership(wtc)) #modularity without edges weights
    m_weighted <- modularity(g, membership(wtc),weights=E(g)$weight) #weighted modularity
    # save network-level stats as data frame
    net_stats = as.data.frame(cbind(y,N,E, 
                                    avg_ew, sd_ew, med_ew, max_ew,
                                    avg_ew_scaled, sd_ew_scaled, med_ew_scaled,
                                    deg_max, deg_min, strength_mean,
                                    assort,ed,ld, mean_deg,
                                    nc,nc_weighted,
                                    m,m_weighted,
                                    beta_eff))
  }
  # return output data frame
  if(stats=="network"){
    return(net_stats)
  } else if(stats=="node"){
    return(node_stats)
  } else{
    return(list(node_stats, net_stats))
  }
}