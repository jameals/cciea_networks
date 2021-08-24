#' Generate Participation Network
#'
#' Create a vessel-level fisheries participation network. Based on original 
#' function byEmma Fuller; edits specified in comments.
#'
#' @param tickets fish tickets data frame
#' @param edge_type type of edge weighting to use - the connectivity statistic ("connectivity"), or just number of vessels ("vessels")
#' @param pcid_choose specify an IOPAC port group
#' @param state_choose specify a US West Coast State
#' @param year_choose Specify a crab year
#' @param filter use the `min_vessels` and `min_contribution` objects to filter the data
#' @param filter_subgraph a filtering option from the original function that was turned off for Fisher et al.
#' @param min_vessels the minimum number of vessels participating in a fishery for that fishery to be retained in the network
#' @param min_contribution the minimum contribution (as a proportion) to total exvessel revenue across all vessels for a fishery to be retained in the network
#' @param min_rev the minimum revenue (in dollars) generated from all fisheries for a given vessel in a given year
#' @param min_rev_indiv the minimum revenue (in dollars) generated from any one fishery for a given vessel in a given year
#' @param write_out specificy whether to write out the adjacency matrix A that is used to build the graph. not yet coded in below, will make it easier if you want to manually adjust the igraph vertex / edge attributes later. 
#' @return non-confidential fisheries partition network as an igraph object
#' @examples
#' close_g <- participation_network_crabyear(close_dat, filter = TRUE, filter_subgraph = FALSE)
#' @export

participation_network_crabyear <- function(tickets, edge_type="connectivity", pcid_choose=NA, state_choose = NA, year_choose=NA, filter, filter_subgraph, min_vessels = 3, min_contribution = 0.10, min_rev = 1, min_rev_indiv = 1, write_out, out_dir){
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
  # get total number of boats (MF 2/26/2019, JS/MF 1/27/21)
  fleet_size <- length(unique(filter(tickets, drvid!='NONE')$drvid))
  
  # create a df with 2 columns: SPGRPN2 (metier.name in Mary's code) and max_boats, the maximum boats that participated in the metier during the specified year(s)
  n_boats <- tickets %>% filter(drvid!='NONE') %>%
    group_by(crab_year, SPGRPN2) %>% #changed year to crab_year (MF 2/26/2019)
    summarise(n_boats = length(unique(drvid))) %>% #changed summarize to summarise, JS 11092018
    group_by(SPGRPN2) %>%
    summarise(max_boats = max(n_boats)) #changed summarize to summarise, JS
  
  # create a df where each column is a SPGRPN2, and values represent the total revenue for a boat in a crab year from that SPGRPN2
  boats <- tickets %>% filter(drvid != 'NONE') %>%
    group_by(drvid, SPGRPN2, crab_year) %>% #removed mutate; changed year to crab_year MF 2/26/2019
    summarise(revenue = sum(adj_revenue)) %>% #changed summarize to summarise, JS 11092018
    pivot_wider(names_from=SPGRPN2, values_from=revenue, values_fill = NA)
  boats <- as.data.frame(boats)
  rownames(boats) <- paste(boats$drvid, boats$crab_year, sep="_")
  boats$drvid <- NULL
  boats$crab_year <- NULL
  
  # remove the one boat that didn't sell catch (i.e. rev = 0). removed 08132021
  # if(any(rowSums(boats,na.rm=T)==0)){boats <- boats[-which(rowSums(boats, na.rm=T)==0),]}
  
  # remove boats that don't generate at least min_rev in revenue annually. added 08112021
  if(any(rowSums(boats,na.rm=T)<min_rev)){boats <- boats[-which(rowSums(boats, na.rm=T)<min_rev),]}
  
  ##### calculate percent contributions, then remove fisheries with below min_rev_indiv #####
  # make a new df with annual % revenue from each metier for each boat
  percent_boats <- boats/rowSums(boats, na.rm = T)
  percent_boats_mat <- as.matrix(percent_boats)
  # for each boat, set percent_boats table value as "NA" for fisheries that don't generate at least min_rev_indiv in revenue annually. added 08192021
  percent_boats_mat[which(boats<min_rev_indiv)] <- NA
  percent_boats <- as.data.frame(percent_boats_mat, row.names=rownames(percent_boats_mat), col.names=colnames(percent_boats_mat))
  rm(percent_boats_mat)
  # for each boat, set filtered_boats table value as "NA" for fisheries that don't generate at least min_rev_indiv in revenue annually.
  boats_mat <- as.matrix(boats)
  boats_mat[which(boats_mat<min_rev_indiv)] <- NA
  filtered_boats <- as.data.frame(boats_mat, row.names=rownames(boats_mat), col.names=colnames(boats_mat))

  
  ##### OR remove fisheries first, then calculate percent contribution ##### -- hashed out by MF 08192021
  # for each boat, set table value as "NA" for fisheries that don't generate at least min_rev_indiv in revenue annually. added 08192021
  # if(any(boats < min_rev_indiv, na.rm=TRUE)){boats[which(boats < min_rev_indiv),] <- NA}
  
  #!! MF -- the above didn't work for me; I still have values < 500 in the matrix. I had to use the following code
  # boats_mat <- as.matrix(boats)
  # boats_mat[which(boats_mat<min_rev_indiv)] <- NA
  # boats <- as.data.frame(boats_mat, row.names=rownames(boats_mat), col.names=colnames(boats_mat))
  
  # boats <- boats %>%
  #   mutate(
  #     revenue = case_when(
  #     revenue < min_rev_indiv ~ NA,
  #     revenue == revenue
  #     )
  #   )
  
  # make a new df with annual % revenue from each metier for each boat
  # percent_boats <- boats/rowSums(boats, na.rm = T)
  
  
  # find median contribution of fisheries across vessels (with min_rev_indiv applied)
  percent_contribution = apply(percent_boats, MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  
  # process data: drop metiers if fewer than 3 boats participate
  # in any year, and have to be on average (min_contribution*100)% of boats annual revenue
  if(filter){
    nb = as.numeric(min_vessels) # changed from 3 to f(x) argument
      percent = as.numeric(min_contribution) # changed from 0.25 to f(x) argument
  }else{
    nb = 0
    percent = 0
  }
  
  # make a new df with median contribution of fisheries across vessels
  fishery_df = as.data.frame(percent_contribution)
  fishery_df$SPGRPN2 = rownames(fishery_df)
  rownames(fishery_df) <- NULL
  
  # make a new df with vessels per fishery
  fish_df <- left_join(fishery_df, n_boats, by = 'SPGRPN2')
  
  fisheries <- fish_df$SPGRPN2[which(fish_df$max_boats>= nb & 
                                           fish_df$percent_contribution>=percent)] # updated 5/13/2019 from > to >=
  
  vessels <- fish_df$max_boats[which(fish_df$max_boats>= nb & 
                                       fish_df$percent_contribution>=percent)]  # added JS 01/26/2021
  
  vessel_df <- data.frame(v=fisheries,
                          n=vessels) # added JS 01/27/2021
  
  if(length(fisheries)==0){
    return(NA)
  }
  
  # get total number of boats INCLUDED IN THE NETWORK - added MF 20210819
  network_fleet_size <- dim(percent_boats)[1]
  
  # build adjacency matrix
  A <- matrix(ncol = length(fisheries), nrow = length(fisheries), data = 0)
  colnames(A) <- fisheries
  rownames(A) <- fisheries
  
  if(edge_type=="connectivity"){
    #  matrix where elements are frac rev fishery i * frac rev fishery j * total dollars (sum)
    for(k in 1:nrow(boats)){
      
      for(i in 1:nrow(A)){
        frac_rev_i = percent_boats[k,fisheries[i]]
        if(is.na(frac_rev_i)){next} # if don't fish this, then can skip all other combos
        
        for(j in i:ncol(A)){
          frac_rev_j = percent_boats[k,fisheries[j]]
          if(is.na(frac_rev_j)){next}
          
          total_rev = boats[k, fisheries[i]] + boats[k, fisheries[j]]
          A[i,j] = A[i,j] + frac_rev_i * frac_rev_j * total_rev
        }
      }
      #if(k %% 1000 == 0){cat(paste(' iteration', k))}
    } #end "connectivity"
  } else if(edge_type=="vessels"){
    #   matrix where elements are counts of vessels participating in fishery i & j
    for(k in 1:nrow(boats)){
      for(i in 1:nrow(A)){
        for(j in i:ncol(A)){
          if(!is.na(percent_boats[k,fisheries[i]]) & !is.na(percent_boats[k,fisheries[j]])){
            A[i,j] = A[i,j] + 1
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
  
  # if(write_out){
    # write.csv(A, here::here(out_dir,paste0("A_",state_choose, "_", year_choose,".csv")), row.names=FALSE)
  # }
  
  if(write_out & !is.na(pcid_choose)){ # added "& !is.na(pcid_choose)" 08202021
    if(edge_type=="connectivity"){
      write.csv(A, here::here(out_dir,paste0("A_",pcid_choose, "_", year_choose,"_connectivity.csv")), row.names=FALSE)
    } else if(edge_type=="vessels"){
      # write out both confidential and non-confidential versions of the A matrix.
      write.csv(A, here::here(out_dir,paste0("A_",pcid_choose, "_", year_choose,"_vessels.csv")), row.names=TRUE)
      write.csv(A_confidential, here::here(out_dir,paste0("A_",pcid_choose, "_", year_choose,"_confidential_vessels.csv")), row.names=TRUE)
    }
  }

  # added 08202021  
  if(write_out & !is.na(state_choose)){
    if(edge_type=="connectivity"){
      write.csv(A, here::here(out_dir,paste0("A_",state_choose, "_", year_choose,".csv")), row.names=FALSE)
    } else if(edge_type=="vessels"){
      # write out both confidential and non-confidential versions of the A matrix.
      write.csv(A, here::here(out_dir,paste0("A_",state_choose, "_", year_choose,".csv")), row.names=TRUE)
      write.csv(A_confidential, here::here(out_dir,paste0("A_",state_choose, "_", year_choose,"_confidential.csv")), row.names=TRUE)
    }
  }
  
  # create graph
  g <- graph_from_adjacency_matrix(adjmatrix = A, mode = 'undirected', weighted = TRUE, diag= FALSE)
  vertex_size = sum(boats[,fisheries], na.rm=T)
  V(g)$size <- vertex_size #total revenue in all fisheries
  V(g)$percent_size = apply(percent_boats[,fisheries, drop=FALSE], MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  V(g)$importance = V(g)$size*V(g)$percent_size #how much revenue from each fishery. note that the plot_networks.R function relies on the igraph library, which scales all nodes to the max size. so in practice, node size is based simply on percent_size and all node sizes are rellattive within a networ
  
  V(g)$fleet = fleet_size #total number of vessels active in given port group, fishing year (MF2/26/2019)
  
  vessel_df2 <- left_join(data.frame(v=V(g)$name),vessel_df, by = "v") # added JS 01/27/2021
  V(g)$vessels <- vessel_df2$n # added JS 01/27/2021
  #V(g)$vessels = vessels # added JS 01/26/2021, replaced with 2 lines above
  V(g)$total_vessels <- network_fleet_size #total number of vessels in network (differs from "fleet" attribute assigned on line ) - added MF 20210819
  
  ##########  changed 2/26/2019 ############
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
