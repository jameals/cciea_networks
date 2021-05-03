#' Identify Metiers with InfoMap
#'
#' Create a graph object with the given vertices and edges, and then 
#' use the InfoMap function in the igraph package to identify metiers
#' with the infomap algorithm. For script 01, section 6.1
#'
#' @param vertices data frame of vertices / nodes (individual trips)
#' @param edges data frame of edges (similarity values for pairwise trip comparisons)
#' @param trials The number of attempts to partition the network for infomap algorithm (>1)
#' @return an object from the igraph communities class
#' @examples
#' mycommunities <- identify_metiers(edges = edges_df, vertices = vertices_ext)
#' @export
identify_metiers <- function(vertices, edges, trials = 100){
  # Create an iGraph `graph` object
  mygraph <- graph_from_data_frame(edges, directed=FALSE, vertices=vertices)
  # Use `cluster_infomap` to identify "community structure that minimizes the expected description length of a random walker trajectory"
  mycommunities <- cluster_infomap(mygraph, e.weights = edges$hellinger.sim, nb.trials=trials)
  return(mycommunities)
}

