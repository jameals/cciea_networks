#' Generate Participation Network
#'
#' Create a fisheries participation network. Based on original function by 
#' Emma Fuller; edits specified in comments.
#'
#' @param tickets fish tickets data frame
#' @param pcid_choose specify an IOPAC port group
#' @param year_choose Specify a crab year
#' @param filter use the `min_vessels` and `min_contribution` objects to filter the data
#' @param filter_subgraph a filtering option from the original function that was turned off for Fisher et al.
#' @param min_vessels the minimum number of vessels participating in a fishery for that fishery to be retained in the network
#' @param min_contribution the minimum contribution (as a proportion) to total exvessel revenue for a fishery to be retained for a given vessel
#' @param write_out specificy whether to write out the adjacency matrix A that is used to build the graph. not yet coded in below, will make it easier if you want to manually adjust the igraph vertex / edge attributes later. 
#' @return non-confidential fisheries partition network as an igraph object
#' @examples
#' close_g <- participation_network_crabyear(close_dat, filter = TRUE, filter_subgraph = FALSE)
#' @export

#the line below is here to allow us to interact with the script without running the function
tickets <- dat; pcid_choose <- iopac; year_choose <- y; min_vessels <- 3; min_contribution <- 0.10

participation_network_crabyear_bulk <- function(tickets, pcid_choose=NA, year_choose=NA, filter, filter_subgraph, min_vessels = 3, min_contribution = 0.10, write_out, out_dir){
  if(!is.na(pcid_choose)){
    tickets = dplyr::filter(tickets, IOPAC %in% pcid_choose) # updated 03-01-21, was pcgroup %in% pcid_choose
  }
  if(any(!is.na(year_choose))){
    tickets = dplyr::filter(tickets, crab_year %in% year_choose)
  }
  
  if(nrow(tickets)==0){
    return(NA)
  }
  # get total number of boats (MF 2/26/2019, JS/MF 1/27/21)
  fleet_size <- length(unique(filter(dat, drvid!='NONE')$drvid))
  
  # create a df with 2 columns: SPGRPN2 (metier.name in Mary's code) and max_boats, the maximum boats that participated in the metier during the specified year(s)
  n_boats <- tickets %>% filter(drvid!='NONE') %>%
    group_by(crab_year, SPGRPN2) %>% #changed year to crab_year (MF 2/26/2019)
    summarise(n_boats = length(unique(drvid))) %>% #changed summarize to summarise, JS 11092018
    group_by(SPGRPN2) %>%
    summarise(max_boats = max(n_boats)) #changed summarize to summarise, JS
  
  # create a df called boats where each column is a SPGRPN2, and values represent the total revenue for a boat in a crab year from that SPGRPN2
  # create a df called fisheries where each column is a SPGRPN2, and values represent the total revenue for a port in a crab year from that SPGRPN2
  fisheries <- tickets %>% filter(drvid != 'NONE') %>%
    group_by(SPGRPN2, crab_year) %>% #removed mutate; changed year to crab_year MF 2/26/2019
    summarise(revenue = sum(adj_revenue)) %>% #changed summarize to summarise, JS 11092018
    spread(SPGRPN2, revenue, fill = NA)
  fisheries <- as.data.frame(fisheries)
  rownames(fisheries) <- fisheries$crab_year
  fisheries$crab_year <- NULL
  
  # remove the one fishery that didn't have any catch (i.e. rev = 0)
  if(any(rowSums(fisheries,na.rm=T)==0)){fisheries <- fisheries[-which(rowSums(fisheries, na.rm=T)==0),]}
  
  # make a new df with annual % revenue from each metier for each boat
  percent_fisheries <- fisheries/rowSums(fisheries, na.rm = T)
  
  # find median contribution of fisheries to each vessel
  percent_contribution = apply(percent_fisheries, MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  
  # process data: drop metiers if fewer than 3 boats participate
  # in any year, and have to be on average 25% of boats annual revenue
  
  if(filter){
    nb = as.numeric(min_vessels) # changed from 3 to f(x) argument
    percent = as.numeric(min_contribution) # changed from 0.25 to f(x) argument
  }else{
    nb = 0
    percent = 0
  }
  
  fishery_df = as.data.frame(percent_contribution)
  fishery_df$SPGRPN2 = rownames(fishery_df)
  rownames(fishery_df) <- NULL
  fish_df <- left_join(fishery_df, n_boats, by = 'SPGRPN2')
  # build adjacency matrix, where elements are frac rev fishery i * frac rev fishery j * total dollars (sum)
  fisheries2 <- fish_df$SPGRPN2[which(fish_df$max_boats>= nb & 
                                           fish_df$percent_contribution>=percent)] # updated 5/13/2019 from > to >=
  
  vessels <- fish_df$max_boats[which(fish_df$max_boats>= nb & 
                                       fish_df$percent_contribution>=percent)]  # added JS 01/26/2021
  
  vessel_df <- data.frame(v=fisheries2,
                          n=vessels) # added JS 01/27/2021
  
  if(length(fisheries2)==0){
    return(NA)
  }
  A <- matrix(ncol = length(fisheries2), nrow = length(fisheries2), data = 0)
  colnames(A) <- fisheries2
  rownames(A) <- fisheries2
  for(k in 1:nrow(boats)){
    
    for(i in 1:nrow(A)){
      frac_rev_i = percent_fisheries[k,fisheries2[i]]
      if(is.na(frac_rev_i)){next} # if don't fish this, then can skip all other combos
      
      for(j in i:ncol(A)){
        frac_rev_j = percent_fisheries[k,fisheries2[j]]
        if(is.na(frac_rev_j)){next}
        
        total_rev = fisheries[k, fisheries2[i]] + fisheries[k, fisheries2[j]]
        A[i,j] = A[i,j] + frac_rev_i * frac_rev_j * total_rev
      }
    }
    #if(k %% 1000 == 0){cat(paste(' iteration', k))}
  }
  
  if(write_out){
    write.csv(A, here::here(out_dir,paste0("A_confidential_",pcid_choose, "_", year_choose,".csv")), row.names=FALSE)
  }
  
  # create graph
  g <- graph_from_adjacency_matrix(adjmatrix = A, mode = 'undirected', weighted = TRUE, diag= FALSE)
  vertex_size = sum(fisheries[,fisheries2], na.rm=T)
  V(g)$size <- vertex_size #total revenue in all fisheries
  V(g)$percent_size = apply(percent_fisheries[,fisheries2, drop=FALSE], MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  V(g)$importance = V(g)$size*V(g)$percent_size #how much revenue from each fishery
  
  V(g)$fleet = fleet_size #total number of vessels in fishery (MF2/26/2019)
  
  vessel_df2 <- left_join(data.frame(v=V(g)$name),vessel_df, by = "v") # added JS 01/27/2021
  V(g)$vessels <- vessel_df2$n # added JS 01/27/2021
  #V(g)$vessels = vessels # added JS 01/26/2021, replaced with 2 lines above
  
  ##########  changed 2/26/2019 ############
  
  # JS did not modify below here 04/27/2021 #
  
  # if filter_subgraph = TRUE, keep V which make up to 99% of revenue
  if(filter_subgraph){
    if(length(V(g))==1){
      big_g <- g
    }
    else{
      # calculate % cumulative revenue fisheries are responsible for
      size_df <- cbind(V(g), V(g)$importance) #changed from V(g)$size
      size_df <- size_df[order(size_df[,2], decreasing = T),]
      size_df <- cbind(size_df, cumsum(size_df[,2])/sum(size_df[,2]))
      
      big_g <- induced_subgraph(g, V(g)[V(g)[rownames(size_df)[which(size_df[,3]<.99)]]]) #IS THIS KEEPING THE RIGHT VERTICES??
    }
  }else{
    big_g <- g
  }
  return(big_g)
}
