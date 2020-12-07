#' Generate Network Edges Data Frame
#'
#' Generate a data frame that matches pairs of nodes to the weight of the edge between
#' them, quantified by similarity value. For script 01, section 5.1
#'
#' @param similarities data frame with pairwise trip similarity scores
#' @param vertices trip IDs data frame used as input for infomap algorithm
#' @return a data frame to build network edges in igraph 
#' @examples
#' edges_df <- generate_network_edges(similarities = similarities, vertices = vertices_df)
#' @export
generate_network_edges <- function(similarities, vertices){
  vertices_thin <- dplyr::select(vertices, c(rownum, node))
  vertices_thin$node <- as.character(vertices_thin$node)
  similarities_thin <- dplyr::select(similarities, trip1, trip2, value.sim) #new in v2
  similarities_thin$trip1 <- as.character(similarities_thin$trip1)
  similarities_thin$trip2 <- as.character(similarities_thin$trip2)
  edges_df <- left_join(similarities_thin, vertices_thin, by=c("trip1" = "node"))
  edges_df <- dplyr::select(edges_df,-trip1)
  colnames(edges_df) <- c("trip2", "hellinger.sim", "trip1")
  edges_df <- left_join(edges_df, vertices_thin, by=c("trip2" = "node"))
  edges_df <- dplyr::select(edges_df,-trip2)
  colnames(edges_df) <- c("hellinger.sim", "trip1", "trip2")
  head(edges_df)
  
  ## rearrange
  edges_df <- edges_df[,c("trip1", "trip2", "hellinger.sim")]
  return(edges_df)
}
