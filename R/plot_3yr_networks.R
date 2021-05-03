#' Simple Participation Network Plotting for 3-year Avg Networks
#'
#' Create a simple plot of a participation network and write it out
#' to a file. The plot will have unlabeled, grey nodes and a 
#' circular layout. The Dungeness crab node will be highlighted
#' in orange. Note that the default file name is based on 
#' function usage to plot the 3-year average participation 
#' networks used in Figure 1 in Fisher et al.
#' For script Fig1_3yrNetworks.Rmd
#'
#' @param g network as an igraph object
#' @param outdir directory to save the png file (relative)
#' @return NULL
#' @examples
#' plot_3yr(tmpgraph, outdir=pngdir)
#' @export
plot_3yr <- function(g, outdir){
  port_group <- unique(V(g)$p)
  y <- unique(V(g)$year)
  l <-layout.circle(g)
  l <- cbind(l, 1:vcount(g))
  rownames(l) <- V(g)$name
  tmpnames <- list(V(g)$name)
  V(g)$colors <- unlist(lapply(tmpnames, function (x) {ifelse(x=="DCRB_POT","darkorange1","gray25")}))
  png(here::here(outdir, paste0(port_group,"_", y,"_3yrAVG_circular_dcrb.png")),bg="transparent")
  plot(g, vertex.size = V(g)$importance/(max(V(g)$importance)*0.025), 
       layout = l, #where to put the nodes on the plot
       edge.width = sqrt(E(g)$weight)/(max(sqrt(E(g)$weight))*0.10),
       edge.curved=F,
       axes = F,
       edge.color = 'gray98',
       vertex.color = V(g)$colors, 
       vertex.label = NA, 
       vertex.frame.color=NA) #vertex.label.color = '#cb4b16'
  dev.off()
}