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
    if(grepl("HKL",tmp_node) == TRUE & grepl("POT",tmp_node) == TRUE){
      tmp_col = "tan1"
    } else if(grepl("HKL",tmp_node) == TRUE){
      tmp_col = "darkgoldenrod1"
    } else if(grepl("NET",tmp_node) == TRUE){
      tmp_col = "cyan4"
    } else if(grepl("TLS",tmp_node) == TRUE){
      tmp_col = "chartreuse3"
    } else if(grepl("TWL",tmp_node) == TRUE){
      tmp_col = "chocolate4"
    } else if(grepl("TWS",tmp_node) == TRUE){
      tmp_col = "hotpink"
    } else if(grepl("POT",tmp_node) == TRUE){
      tmp_col = "darkorange2"
    } else{
      if(tmp_node=="other_port" | tmp_node=="no_fishing"){
        tmp_col="gray"
      } else if(tmp_node=="none"){
        tmp_col="gray"
      } else{
        tmp_col = "plum3"
      }
    }
    vertex_cols[i] <- tmp_col
  }
  return(vertex_cols)
}
