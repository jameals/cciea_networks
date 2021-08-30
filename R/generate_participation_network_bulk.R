#' Generate Participation Network
#'
#' Create an aggregate fisheries participation network. Based on original 
#' function by Emma Fuller; edits specified in comments.
#'
#' @param tickets fish tickets data frame
#' @param edge_type type of edge weighting to use - the connectivity statistic ("connectivity"), or just number of vessels ("vessels")
#' @param pcid_choose specify an IOPAC port group
#' @param state_choose specify a US West Coast State@param state_choose specify a US West Coast State
#' @param year_choose Specify a crab year
#' @param filter use the `min_vessels` and `min_contribution` objects to filter the data
#' @param filter_subgraph a filtering option from the original function that was turned off for Fisher et al.
#' @param min_vessels the minimum number of vessels participating in a fishery for that fishery to be retained in the network
#' @param min_contribution the minimum contribution (as a proportion) to total exvessel revenue at a port group or in a state for a fishery to be retained for a given vessel
#' @param min_rev the minimum revenue (in dollars) generated from all fisheries for a given port or state in a given year
#' @param min_rev_indiv the minimum revenue (in dollars) generated from any one fishery for a given port or state in a given year
#' @param write_out specify whether to write out the adjacency matrix A that is used to build the graph. not yet coded in below, will make it easier if you want to manually adjust the igraph vertex / edge attributes later. 
#' @return non-confidential fisheries partition network as an igraph object
#' @examples
#' close_g <- participation_network_crabyear(close_dat, filter = TRUE, filter_subgraph = FALSE)
#' @export

#the line below is here to allow us to interact with the script without running the function
#tickets <- dat; pcid_choose <- iopac; year_choose <- y; min_vessels <- 3; min_contribution <- 0.10

participation_network_crabyear_bulk <- function(tickets, edge_type="connectivity", pcid_choose=NA, state_choose = NA, year_choose=NA, filter, filter_subgraph, min_vessels = 3, min_contribution = 0.10, min_rev = 1, min_rev_indiv = 1, write_out, out_dir){
  if(!is.na(state_choose)){
    tickets = dplyr::filter(tickets, agid %in% state_choose) # updated 08-20-21
  }
  
  if(!is.na(pcid_choose)){
    tickets = dplyr::filter(tickets, IOPAC %in% pcid_choose) # updated 03-01-21, was pcgroup %in% pcid_choose
  }
  
  if(any(!is.na(year_choose))){
    tickets = dplyr::filter(tickets, crab_year %in% year_choose)
  }
  
  if(nrow(tickets)==0){
    return(NA)
  }
  
  # remove ports or states that don't generate at least $5,000 in revenue across all fisheries in a year. added 08232021
  if(sum(tickets$adj_revenue,na.rm=T)< min_rev){
    return(NA)
  }
  
  # get total number of boats (MF 2/26/2019, JS/MF 1/27/21)
  fleet_size <- length(unique(filter(tickets, drvid!='NONE')$drvid))
  
  # create a df with 2 columns: SPGRPN2 (metier.name in Mary's code) and max_boats, the maximum boats that participated in the metier during the specified year(s)
  n_boats <- tickets %>% filter(drvid!='NONE') %>%
    group_by(crab_year, SPGRPN2) %>% #changed year to crab_year (MF 2/26/2019)
    summarise(n_boats = length(unique(drvid))) %>% #changed summarize to summarise, JS 11092018
    group_by(SPGRPN2) %>%
    summarise(max_boats = max(n_boats)) #changed summarize to summarise, JS
  
  # create a df called fisheries where each column is a SPGRPN2, and values represent the total revenue for a port or state in a crab year from that SPGRPN2
  fisheries <- tickets %>% filter(drvid != 'NONE') %>%
    group_by(SPGRPN2, crab_year) %>% #removed mutate; changed year to crab_year MF 2/26/2019
    summarise(revenue = sum(adj_revenue)) %>% #changed summarize to summarise, JS 11092018
    spread(SPGRPN2, revenue, fill = NA)
  fisheries <- as.data.frame(fisheries)
  rownames(fisheries) <- fisheries$crab_year
  fisheries$crab_year <- NULL
  
  # JS added 08302021 to avoid issue of filters leading to no fisheries that meet rev cutoffs
  if(is.null(nrow(fisheries))==TRUE){
    return(NA)
  }

  
  ##### calculate percent contributions, then remove fisheries with below min_rev_indiv ##### added 08232021
  # make a new df with annual % revenue from each metier for each fishery
  percent_fisheries <- fisheries/rowSums(fisheries, na.rm = T) 
  percent_fisheries_mat <- as.matrix(percent_fisheries)
  # for each fishery, set percent_fisheries table value as "NA" for fisheries that don't generate at least min_rev_indiv in revenue annually. added 08192021
  percent_fisheries_mat[which(fisheries<min_rev_indiv)] <- NA
  percent_fisheries <- as.data.frame(percent_fisheries_mat, row.names=rownames(percent_fisheries_mat), col.names=colnames(percent_fisheries_mat))
  rm(percent_fisheries_mat)
  # for each fishery, set filtered_fisheries table value as "NA" for fisheries that don't generate at least min_rev_indiv in revenue annually.
  fisheries_mat <- as.matrix(fisheries)
  fisheries_mat[which(fisheries_mat<min_rev_indiv)] <- NA
  filtered_fisheries <- as.data.frame(fisheries_mat, row.names=rownames(fisheries_mat), col.names=colnames(fisheries_mat))
  
  # JS added 08302021 to avoid issue of filters leading to no fisheries that meet rev cutoffs
  if(is.null(nrow(fisheries))==TRUE){
    return(NA)
  }

  # so percent_fisheries is now a df with a single row (corresponding to year), each column has a name corresponding to a metier (sp group), and the values represent the % revenue contributed from each metier (sp group) to total fisheries rev in that year. metiers (sp groups) that contribute less than min_rev_indiv are represented with NA
  # filtered_fisheries is now a df with a single row (corresponding to year), each column has a name corresponding to a metier (sp group), and the values represent the absolute revenue in $ contributed from each metier (sp group). metiers (sp groups) that contribute less than min_rev_indiv are represented with NA
  
  # find median contribution of fisheries across all vessels
  # if year_choose=XXXX, percent_fisheries == percent_contribution. Otherwise, percent_contribution will take a median of revenue over all included crab years
  percent_contribution = apply(percent_fisheries, MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  
  # process data: drop metiers if fewer than min_vessels boats participate
  # in any year, and have to be on average min_contribution*100% of boats annual revenue
  
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
  
  fisheries2 <- fish_df$SPGRPN2[which(fish_df$max_boats>= nb & 
                                           fish_df$percent_contribution>=percent)] # updated 5/13/2019 from > to >=
  
  vessels <- fish_df$max_boats[which(fish_df$max_boats>= nb & 
                                       fish_df$percent_contribution>=percent)]  # added JS 01/26/2021
  
  vessel_df <- data.frame(v=fisheries2,
                          n=vessels) # added JS 01/27/2021
  
  if(length(fisheries2)==0){
    return(NA)
  }
  
  # get total number of boats INCLUDED IN THE NETWORK - added MF 20210819. this should be only the total number of boats that meet criteria for inclusion for fisheries that meet criteria for inclusion, modified JFS 08242021 
  network_fleet_size <- as.numeric(
    tickets %>% 
    filter (SPGRPN2 %in% vessel_df$v) %>% 
    group_by(crab_year) %>% 
    summarise(n_boats = length(unique(drvid))) %>% 
    summarise(network_fleet_size = max(n_boats)) 
  )
  
  # build adjacency matrix
  A <- matrix(ncol = length(fisheries2), nrow = length(fisheries2), data = 0)
  colnames(A) <- fisheries2
  rownames(A) <- fisheries2
  
  # if year_choose=XXXX, there should only be k=1 row in fisheries. 
  # Otherwise, if there is more than 1 row in fisheries, the A matrix will reflect fishery participation combined over multiple years. 
  if(edge_type=="connectivity"){
    #  matrix where elements are frac rev fishery i * frac rev fishery j * total dollars (sum)
    for(k in 1:nrow(fisheries)){
      
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
    } #end "connectivity"
  } else if(edge_type=="vessels"){
    #   matrix where elements are counts of vessels participating in fishery i & j
    for(k in 1:nrow(fisheries2)){
      for(i in 1:nrow(A)){
        for(j in i:ncol(A)){
          if(!is.na(percent_fisheries[k,fisheries2[i]]) & !is.na(percent_fisheries[k,fisheries2[j]])){
            A[i,j] = A[i,j] + as.numeric(
              tickets %>% 
              filter (SPGRPN2 == fisheries2[i] | SPGRPN2 == fisheries2[j]) %>% 
              group_by(crab_year) %>% 
              summarise(n_boats = length(unique(drvid))) %>% 
              summarise(max_boats = max(n_boats)) 
            )
          }
        }
      } 
      #if(k %% 1000 == 0){cat(paste(' iteration', k))}
    } 
    A_counts <- A
    # matrix where elements are proportion of vessels participating in fishery i & j (out of total # vessels included in network)
    A_confidential <- A_counts / network_fleet_size
    # non-confidential version of matrix A
    A <- A_confidential; A[which(A_counts < 3)] <- 0
    
  } else{stop("ERROR: unrecognized edge type; cannot generate input matrix.")}
  
  if(write_out & !is.na(pcid_choose)){ # added "& !is.na(pcid_choose)" 08202021
    if(edge_type=="connectivity"){
      write.csv(A, here::here(out_dir,paste0("A_",pcid_choose, "_", year_choose,"_",contr_cutoff*100,"_connectivity.csv")), row.names=FALSE)
    } else if(edge_type=="vessels"){
      # write out both confidential and non-confidential versions of the A matrix.
      write.csv(A, here::here(out_dir,paste0("A_",pcid_choose, "_", year_choose,"_",contr_cutoff*100,"_vessels.csv")), row.names=TRUE)
      write.csv(A_confidential, here::here(out_dir,paste0("A_",pcid_choose, "_", year_choose,"_",contr_cutoff*100,"_confidential_vessels.csv")), row.names=TRUE)
    }
  }
  
  # added 08202021  
  if(write_out & !is.na(state_choose)){
    if(edge_type=="connectivity"){
      write.csv(A, here::here(out_dir,paste0("A_",state_choose, "_", year_choose,"_",contr_cutoff*100,"_connectivity.csv")), row.names=FALSE)
    } else if(edge_type=="vessels"){
      # write out both confidential and non-confidential versions of the A matrix.
      write.csv(A, here::here(out_dir,paste0("A_",state_choose, "_", year_choose,"_",contr_cutoff*100,"_vessels.csv")), row.names=TRUE)
      write.csv(A_confidential, here::here(out_dir,paste0("A_",state_choose, "_", year_choose,"_",contr_cutoff*100,"_vessels_confidential.csv")), row.names=TRUE)
    }
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
  V(g)$total_vessels <- network_fleet_size #total number of vessels in network (differs from "fleet" attribute assigned on line ) - added MF 20210819
  
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
