#' Match Metiers to Trips
#'
#' Match metiers (communities) to trip IDs, and automatically generate names 
#' for each metier (community) identified by the infomap algorithm. For script 04, section 2
#'
#' @param revenue data frame with revenue for each species/gear combination in each metier
#' @param vertices trips IDs data frame used as input for infomap algorithm
#' @param name if TRUE, generate a character name for each metier based on revenue per species/gear combination
#' @return a data frame with trip IDs matched to a metier (numeric output from infomap) and a metier name (optional)
#' @examples
#' trip_community_key <- metier_key_rerun(revenue = rev_by_community, vertices = vertices_ext)
#' @export
metier_key_rerun <- function(revenue, vertices, name = TRUE){
  if(name == TRUE){
    # get 90% of top revenue for each community
    top_rev_by_community <- revenue %>%
      arrange(community, desc(total_rev)) %>%
      group_by(community) %>%
      summarise(lower_lim = 0.9*max(total_rev, na.rm=TRUE))
    rev_by_community_wlims <- left_join(revenue, top_rev_by_community, by="community")
    # save all species/grgroups that are within 10% of top revenue
    comm_names_df <- filter(rev_by_community_wlims, lower_lim < total_rev)
    comm_names <- c()
    # take the headings with ".rev" at the end and fix the text
    for(i in unique(comm_names_df$community)){
      tmp_names_df <- filter(comm_names_df, community == i)
      tmp_names_list <- as.character(tmp_names_df$variable)
      tmp_names_list <- gsub(".rev", "", tmp_names_list)
      tmp_name <- paste0(tmp_names_list, collapse="_")
      comm_names[i] <- tmp_name
    }
    # output data frame with community numbers and new names
    comm_df <- data.frame(community = seq(1, length(comm_names)),
                          comm_id = comm_names)
    trip_community_key <- left_join(vertices, comm_df, by="community")
    trip_community_key <- dplyr::select(trip_community_key, node, drvid_year, crab_year, community, comm_id)
    colnames(trip_community_key) <- c("trip_id", "drvid_year", "crab_year", "comm_num", "comm_id")
  }
  else{
    comm_df <- data.frame(community = seq(1, length(unique(vertices$community))),
                          comm_id = rep(NA, length(unique(vertices$community))))
    trip_community_key <- left_join(vertices, comm_df, by="community")
    trip_community_key <- dplyr::select(trip_community_key, node, drvid_year, crab_year, community, comm_id)
    colnames(trip_community_key) <- c("trip_id", "drvid_year", "crab_year","comm_num", "comm_id")
  }
  return(trip_community_key)
}
