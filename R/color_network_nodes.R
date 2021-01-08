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

library(RColorBrewer)
#palette(brewer.pal(n = 10, name = "Set3"))
pal3 <- brewer.pal(n = 10, name = "Set3")

# need to fix series of if_else. maybe use case_when from tidy

vertex_color <- function(g){
  vertex_cols <- c()
  for(i in seq(1, vcount(g))){
    tmp_node <- V(g)$name[i]
    # if(grepl("1",tmp_node) == TRUE | grepl("3",tmp_node) == TRUE |
    #    grepl("19",tmp_node) == TRUE | grepl("50",tmp_node) == TRUE | 
    #    grepl("55",tmp_node) == TRUE){
    #   tmp_col = pal3[1] # groundfish species groups
    # } else if(grepl("2",tmp_node) == TRUE){
    #   tmp_col = pal3[2] # whiting
    #} else 
    if(grepl("20",tmp_node) == TRUE | grepl("21",tmp_node)){
      tmp_col = pal3[3] # shrimp
    } 
    #else if(grepl("30",tmp_node) == TRUE){
    #   tmp_col = pal3[4] # salmon
    # } else if(grepl("35",tmp_node) == TRUE){
    #   tmp_col = pal3[5] # tuna
    # } else if(grepl("40",tmp_node) == TRUE | grepl("45",tmp_node)){
    #   tmp_col = pal3[6] # pelagics
    # } else if(grepl("25",tmp_node) == TRUE){
    #   tmp_col = pal3[7] # crab
    # } else if(grepl("70",tmp_node) == TRUE){
    #     tmp_col = pal3[8] # squid
    #   } else{
    #   if(tmp_node=="60" | tmp_node=="65"){
    #     tmp_col = pal3[9] # shellfish
    #   } else{
    #     tmp_col = pal3[10] # other, group 80
    #   }
    # }
    vertex_cols[i] <- tmp_col
  }
  return(vertex_cols)
}

#https://data.library.virginia.edu/setting-up-color-palettes-in-r/
# display.brewer.pal(10, "Set3")
# mypalette<-brewer.pal(8, "Set3")
# image(1:7,1,as.matrix(1:7),col=mypalette,xlab="Greens (sequential)", ylab="",xaxt="n",yaxt="n",bty="n")
# brewer.pal(n = 10, name = "Set3")
# palette(brewer.pal(n = 10, name = "Set3"))
