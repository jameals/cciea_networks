#' Color Network Nodes
#'
#' Use the gear type and species ID to assign a color to
#' each node (vertex) in the network. Meant for both 
#' directed and undirected network plotting functions.
#'
#' @param g network as an igraph object
#' @return character vector with colors for each node, ordered
#' @examples
#' vertex_cols <- vertex_color(g = close_g); V(close_g)$colors <- vertex_col
#' @export
vertex_color <- function(g){
  vertex_cols <- c()
  for(i in seq(1, vcount(g))){
    tmp_node <- V(g)$name[i]
    if(grepl("1",tmp_node) == TRUE | grepl("3",tmp_node) == TRUE |
       grepl("19",tmp_node) == TRUE | grepl("50",tmp_node) == TRUE | 
       grepl("55",tmp_node) == TRUE){
      tmp_col = "tan1" # groundfish species groups
    } else if(grepl("2",tmp_node) == TRUE){
      tmp_col = "darkgoldenrod1" # whiting
    } else if(grepl("20",tmp_node) == TRUE | grepl("21",tmp_node)){
      tmp_col = "cyan4" # shrimp
    } else if(grepl("30",tmp_node) == TRUE){
      tmp_col = "chartreuse3" # salmon
    } else if(grepl("35",tmp_node) == TRUE){
      tmp_col = "chocolate4" # tuna
    } else if(grepl("40",tmp_node) == TRUE | grepl("45",tmp_node)){
      tmp_col = "hotpink" # pelagics
    } else if(grepl("25",tmp_node) == TRUE){
      tmp_col = "darkorange2" # crab
    } else{
      if(tmp_node=="60" | tmp_node=="65"){
        tmp_col="gray" # shellfish
      } else if(tmp_node=="70"){
        tmp_col="gray" # squid, should change to red
      } else{
        tmp_col = "plum3" # other, group 80
      }
    }
    vertex_cols[i] <- tmp_col
  }
  return(vertex_cols)
}
