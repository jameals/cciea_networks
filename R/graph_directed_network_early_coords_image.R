#' Graph the Directed Network, Early Season
#'
#' Graph a directed network to show flow of vessels between the 2015
#' Dungeness crab fishery and 2016 alternatives during the early season.
#' This is the function used for Fisher et al., although the plot_directed_early
#' function allows for several plotting methods. For script Fig3.Rmd
#'
#' @param g igraph object
#' @param dist_y specify the vertical distance between nodes, for when x_coords is TRUE
#' @param image_object a character string that provides the file name for the dungeness crab node icon, for when crab_image is TRUE
#' @param image_size the size of the dungeness crab node icon, for when crab_image is TRUE
#' @param image_aspect the aspect ratio (width:height) of the dungeness crab node icon, for when crab_image is TRUE
#' @param image_offset the distance to the left of the node to place the dungeness crab node icon, for when crab_image is TRUE
#' @param image_transparent the alpha value for the dungeness crab node icon, for when crab_image is TRUE 
#' @return a ggraph object
#' @examples
#' plot_directed_early_coords_image(g=mygraph, image_object=here::here("data/input","crab_icon.png"))
#' @export
plot_directed_early_coords_image <- function (g, dist_y=1.2, image_object=img_file, image_size=0.15, image_aspect=1.2, image_offset=0.15, image_transparent=FALSE) {
  
  ##################### for all graphs ####################
  # save the port group
  p=unique(V(g)$port)
  # save the vessel size category
  s=unique(V(g)$vsize)
  s=ifelse(s=="large","Large","Small")
  # the graph will be labeled with the port group, for large vessels (on left of combined graph)
  if(s=="Large"){ylabel=p} else{ylabel=""}
  
  # color the network nodes
  V(g)$color <- vertex_color(g) # node colors
  # edit common names
  g <- rename_vertices(g)
  # get rid of crab node label
  V(g)$common_name[which(V(g)$common_name=="Dungeness\ncrab")] <- ""
  # remove any nodes with size of 0 in 2015 (except for Dungeness crab)
  g <- delete.vertices(g, V(g)[which(V(g)$size==0 & V(g)$name != "DCRB_POT")])
  
  ## get node sizes, and reset 2015 d.crab node size to the 2014 participation
  dcrb_node <- which(V(g)$name=="DCRB_POT")
  vsizes=V(g)$size
  vsizes[dcrb_node] = V(g)$size14[dcrb_node]
  
  # to plot vertices with only self loops on the left side of the graph
  self_g = delete.edges(g,E(g)[!(which_loop(g))])
  self_loops <- as_edgelist(self_g)[,1]
  self_only <- self_loops[which(self_loops %in% names(which((degree(simplify(g))==0))))]
  
  # set the 'y' axis locations of each node. This also requires you to set the start and end of the edges. 
  node.y <- unlist(lapply(V(g)$name,FUN=function(i){ifelse(i=="DCRB_POT", 1,ifelse(i %in% self_loops, 0.1,0))}))
  edge.y <- unlist(lapply(as_edgelist(g)[,1],FUN=function(i){ifelse(i=="DCRB_POT", 1,ifelse(i %in% self_loops, 0.1,0))}))
  edge.yend <- unlist(lapply(as_edgelist(g)[,2],FUN=function(i){ifelse(i=="DCRB_POT", 1,ifelse(i %in% self_loops, 0.1,0))}))
  
  node.y.lab.left <- unlist(lapply(V(g)$name,FUN=function(i){ifelse(i=="DCRB_POT", 1.1,NA)}))
  node.y.lab.right <- unlist(lapply(V(g)$name,FUN=function(i){ifelse(i=="DCRB_POT", NA,ifelse(i %in% self_loops, -0.1,-0.1))}))
  #####################################################
  
  ##################### assign x-coordinates #####################
  # Get the edge list
  el <- data.frame(as_edgelist(g)) %>%
    mutate(X1=as.character(X1),
           X2=as.character(X2))
  
  # how many non-crab fisheries total?
  nmets <- length(V(g)$name) - 1
  m <- -1.5
  # these will be the right side node locations
  x.locs <- seq(from=m, length.out=nmets, by=dist_y)
  # assign an order to the nodes - put self loops on top, then no fishing / other port. have 'other' at the bottom 
  node.x.df <- data.frame(node=V(g)$name) %>%
    filter(node != "DCRB_POT") %>%
    mutate(order= ifelse(node %in% self_only,1,
                         ifelse(node == "no_fishing",2, ifelse(node=="other_port",3,
                                                               ifelse(grepl("OTHR",node),nmets,
                                                                      NA)))))
  # for nodes that don't have a specific order, assign some middle numbers
  #    then arrange in descending order, and append the x.locs vector
  node.x.df <- node.x.df %>% mutate(order=ifelse(!is.na(order),order,sample(rep(seq(from=nth(node.x.df$order, n=-2L)+1, to=nmets-1),2), size=1))) %>%
    dplyr::arrange(desc(order)) %>%
    mutate(xval=x.locs) %>%
    dplyr::select(-order) %>%
    # add d.crab node
    bind_rows(data.frame(node="DCRB_POT",xval=mean(range(x.locs))))
  
  # use data frame to get x axis node positions for graph, in the original node order
  node.x.df <- node.x.df[match(V(g)$name, node.x.df$node),] ## match requires exact same elements, no duplicate values
  node.x <- node.x.df$xval
  # use data frame to get x axis edge start / end positions for graph
  node.x.df$node <- as.character(node.x.df$node)
  el <- left_join(el,node.x.df,by=c("X1"="node"))
  el <- left_join(el,node.x.df,by=c("X2"="node")); colnames(el) <- c("X1","X2", "edge.x","edge.xend")
  #############################################################################################
  
  
  ##################### graph #####################
  # rescale node size #
  vsizes_scaled <- scales::rescale(c(1,vsizes), to=c(2,10))
  vsizes_scaled <- vsizes_scaled[2:length(vsizes_scaled)]
  
  ###### if none of the vertices have self-loops ######
  if(ecount(self_g) < 1){
    plot_base <- ggraph(g, 'igraph', algorithm = 'tree') + 
      geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,x=el$edge.x, 
                             xend=el$edge.xend,width=E(g)$weight), 
                         color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
      geom_node_point(aes(y=node.y,x=node.x),color=factor(V(g)$color),
                      size=vsizes_scaled) +
      geom_node_text(aes(y=node.y.lab.left,x=node.x, label = common_name, hjust=1), size=6) +
      geom_node_text(aes(y=node.y.lab.right,x=node.x, label = common_name, hjust=0), size=6) +
      #annotate("text", x = 1.35, y = 0.5, label = paste0(s," Vessels"), size=4) +
      xlab(ylabel) +
      scale_edge_width(range = c(2, 6)) +
      theme_void() +
      theme(legend.position="none",
            axis.title.y=element_text(angle=90,size=20,hjust=0.5),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      coord_flip() +
      scale_x_continuous(limits=c(-2,max(x.locs)+1), expand=expansion(add=c(0.15,0.2))) +
      scale_y_reverse(expand=expansion(add=c(0.25,1.3)))
    # get the x coordinate for the dungeness crab node
    img_xcoord <- node.x.df$xval[which(node.x.df$node=="DCRB_POT")]
    # re-plot graph with image
    if(image_transparent){
      plot_out <- plot_base +
        geom_image(aes(x=img_xcoord, y=1+image_offset, image=img_file), image_fun=transparent, size=image_size, alpha=0.2, asp=image_aspect)
    } else{
      plot_out <- plot_base +
        geom_image(aes(x=img_xcoord, y=1+image_offset, image=img_file), size=image_size, asp=image_aspect)
    } # end if image_transparent is true
    
    ###### if 1+ vertices have self-loops ######
  } else{
    plot_base <- ggraph(g, 'igraph', algorithm = 'tree') +
      geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,x=el$edge.x, xend=el$edge.xend,
                             width=E(g)$weight), 
                         color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
      geom_edge_loop(aes(y=edge.y,x=el$edge.x,width=E(g)$weight, span=50,direction=0,strength=0.90), color="grey85") +
      geom_node_point(aes(y=node.y,x=node.x),color=factor(V(g)$color),
                      size=vsizes_scaled) +
      geom_node_text(aes(y=node.y.lab.left,x=node.x, label = common_name, hjust=1), size=6) +
      geom_node_text(aes(y=node.y.lab.right,x=node.x, label = common_name, hjust=0), size=6) +
      xlab(ylabel) +
      scale_edge_width(range = c(2, 6)) +
      theme_void() +
      theme(legend.position="none",
            axis.title.y=element_text(angle=90,size=20,hjust=0.5),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      coord_flip() +
      scale_x_continuous(limits=c(-2,max(x.locs)+1), expand=expansion(add=c(0.15,0.2))) +
      scale_y_reverse(expand=expansion(add=c(0.25,1.3)))
    # get the x coordinate for the dungeness crab node
    img_xcoord <- node.x.df$xval[which(node.x.df$node=="DCRB_POT")]
    # re-plot graph with image
    if(image_transparent){
      plot_out <- plot_base +
        geom_image(aes(x=img_xcoord, y=1+image_offset, image=img_file), image_fun=transparent, size=image_size, alpha=0.2, asp=image_aspect)
    } else{
      plot_out <- plot_base +
        geom_image(aes(x=img_xcoord, y=1+image_offset, image=img_file), size=image_size, asp=image_aspect)
    } # end if image_transparent is true
  } # end plot with self-loops
  
  return(plot_out)
}
