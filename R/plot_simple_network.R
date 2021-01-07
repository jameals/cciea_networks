#' Plot Simple Participation Network
#'
#' Use an igraph object to plot a simple fisheries participation
#' network. The simple network plot uses a circular layout by 
#' default, and colors all nodes the same except for
#' the highlighted node, which is orange.
#' 
#'
#' @param g network as an igraph object
#' @param highlight which node to highlight in orange. Should specify from "common_name" vertex attribute.
#' @param outdir directory where image files will be saved
#' @param file_suffix string to be added to the suffix of the image file name. script will automatically name the files by port group, year, and layout type.
#' @return Null. Will write plots directly to files
#' @examples
#' plot_network(tmpgraph, outdir="data/networks/participation/plots")
#' @export
plot_simple <- function(g, highlight="WOC_CRAB", outdir, file_suffix="_cciea"){
  port_group <- unique(V(g)$p)
  y <- unique(V(g)$year)
  l <-layout.circle(g)
  l <- cbind(l, 1:vcount(g))
  rownames(l) <- V(g)$name
  tmpnames <- list(V(g)$common_name)
  V(g)$colors <- unlist(lapply(tmpnames, function (x) {ifelse(x==highlight,"darkorange1","gray25")}))
  png(here::here(outdir, paste0(port_group,"_", y,"_simple_dcrb",file_suffix,".png")),bg="transparent")
  plot(g, vertex.size = V(g)$importance/(max(V(g)$importance)*0.025), 
       layout = l, #where to put the nodes on the plot
       edge.width = sqrt(E(g)$weight)/(max(sqrt(E(g)$weight))*0.10),
       edge.curved=F,
       axes = F,
       edge.color = 'gray68',
       vertex.color = V(g)$colors, 
       vertex.label = NA, 
       vertex.frame.color=NA) #vertex.label.color = '#cb4b16'
  dev.off()
}
