#' Plot Participation Network
#'
#' Use an igraph object to plot a fisheries participation
#' network.
#' Warning: The Fruchterman & Reingold layout is informative but 
#' can result in overlap / odd clustering, especially if there 
#' is a fishery that has no / weakly connecting edges to other 
#' fisheries in the network.
#'
#' @param g network as an igraph object
#' @param layout_type network plot layout. Fruchterman-Reingold, Circular, or Both [fr / c / both]
#' @param outdir directory where image files will be saved
#' @param file_suffix string to be added to the suffix of the image file name. script will automatically name the files by port group, year, and layout type.
#' @return Null. Will write plots directly to files
#' @examples
#' plot_network(tmpgraph, layout_type="both", outdir="data/networks/participation/plots")
#' @export
plot_network <- function(g, layout_type, outdir, file_suffix="_cciea"){
  port_group <- unique(V(g)$p)
  y <- unique(V(g)$year)
  if(layout_type=="fr" | layout_type=="both"){
    l <-  layout.fruchterman.reingold(g)
    # l <- layout_with_fr(g) #this is not the same as above??
    l <- cbind(l, 1:vcount(g))           # switched from a solid number to a function to get number of vertices -- M.F. 11/19/2018
    rownames(l) <- V(g)$name
    png(here::here(outdir, paste0(port_group,"_", y,"_fr",file_suffix,".png")),bg="transparent")  # if this resolution isn't good enough, add: width = 2000, height = 1500,res=300
    plot(g, vertex.size = V(g)$importance/(max(V(g)$importance)*0.02), 
         layout = l, #where to put the nodes on the plot
         edge.width = sqrt(E(g)$weight)/(0.037*max(sqrt(E(g)$weight))),
         edge.curved = F, 
         axes = F,
         edge.color = 'gray',
         vertex.label = V(g)$common_name,
         vertex.color =adjustcolor(V(g)$colors, alpha.f=0.90),
         vertex.label.family = 'sans', 
         vertex.label.color = "gray25",
         vertex.label.cex= 1.2, 
         vertex.frame.color=NA
         # vertex.label.dist = c(-9,-8,-9,-10),               # these can be adjusted manually to make it look nicer, which is super annoying
         # vertex.label.degree = c(pi^(0.9),pi/5,pi/5,pi*1.1) # these can be adjusted manually to make it look nicer, which is super annoying
    )
    dev.off()
  }
  if (layout_type=="c" | layout_type=="both"){
    l <-layout.circle(g)
    l <- cbind(l, 1:vcount(g))
    rownames(l) <- V(g)$name
    tmpnames <- list(V(g)$name)
    png(here::here(outdir, paste0(port_group,"_", y,"_circular",file_suffix,".png")),bg="transparent")  # if this resolution isn't good enough, add: width = 2000, height = 1500,res=300
    plot(g, vertex.size = V(g)$importance/(max(V(g)$importance)*0.02), 
         layout = l, #where to put the nodes on the plot
         edge.width = sqrt(E(g)$weight)/(0.037*max(sqrt(E(g)$weight))),
         edge.curved = F, 
         axes = F,
         edge.color = 'gray',
         vertex.label = V(g)$common_name,
         vertex.color =adjustcolor(V(g)$colors, alpha.f=0.90),
         vertex.label.family = 'sans', 
         # vertex.label.color = V(g)$colors, # to have same color as vertices
         vertex.label.color = "gray25",
         vertex.label.cex= 1.2, 
         vertex.frame.color=NA
         # vertex.label.dist = c(-9,-8,-9,-10),               # these can be adjusted manually to make it look nicer, which is super annoying
         # vertex.label.degree = c(pi^(0.9),pi/5,pi/5,pi*1.1) # these can be adjusted manually to make it look nicer, which is super annoying
    )
    dev.off()
  } 
  if(!(layout_type %in% c("c","fr","both"))){message("ERROR: don't recognize layout type. try again with [fr/c/both]")}
  
}
