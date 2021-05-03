#' Graph the Directed Network, Early Season
#'
#' Graph a directed network to show flow of vessels between the 2015
#' Dungeness crab fishery and 2016 alternatives during the early season.
#' Original function name: plot_graph. For script Fig3.Rmd
#'
#' @param g igraph object
#' @return a ggraph object
#' @examples
#' plot_directed_early(g=mygraph)
#' @export
plot_directed_early = function (g) {
  
  ####### for all graphs ######
  # save the port group
  p=unique(V(g)$port) # port group
  # color the network nodes
  V(g)$color <- vertex_color(g) # node colors
  # remove any nodes with size of 0 in 2015 (except for Dungeness crab)
  g <- delete.vertices(g, V(g)[which(V(g)$size==0 & V(g)$name != "DCRB_POT")])
  
  ## get node sizes, and reset 2015 d.crab node size to the 2014 participation
  dcrb_node <- which(V(g)$name=="DCRB_POT")
  vsizes=V(g)$size
  vsizes[dcrb_node] = V(g)$size14[dcrb_node]
  
  # to plot vertices with only self loops on the left side of the graph
  self_only <- c()
  self_g = delete.edges(g,E(g)[!(which_loop(g))])
  self_loops <- as_edgelist(self_g)[,1]
  
  # set the 'y' axis locations of each node. This also requires you to set the start and end of the edges. 
  node.y <- unlist(lapply(V(g)$name,FUN=function(i){ifelse(i %in% c("DCRB_POT",self_only), 1,ifelse(i %in% self_loops, 0.1,0))}))
  edge.y <- unlist(lapply(as_edgelist(g)[,1],FUN=function(i){ifelse(i %in% c("DCRB_POT",self_only), 1,ifelse(i %in% self_loops, 0.1,0))}))
  edge.yend <- unlist(lapply(as_edgelist(g)[,2],FUN=function(i){ifelse(i %in% c("DCRB_POT",self_only), 1,ifelse(i %in% self_loops, 0.1,0))}))
  node.y.lab <- unlist(lapply(V(g)$name,FUN=function(i){ifelse(i %in% c("DCRB_POT",self_only), 1.2,ifelse(i %in% self_loops, -0.5,-0.5))}))
  
  ####### all vessels - no vessel sizes ######
  if(is.null(V(g)$vsize)){
    ylabel = p
    ### if none of the vertices have self-loops ###
    if(ecount(self_g) < 1){
      ggraph(g, 'igraph', algorithm = 'tree') + 
        geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,width=sqrt((E(g)$weight))), 
                           color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
        geom_node_point(aes(y=node.y),color=factor(V(g)$color),
                        size=10*sqrt(vsizes)/sqrt(max(vsizes))) +
        geom_node_text(aes(y=node.y.lab,label = common_name), size=5) +
        #annotate("text", x = 1.35, y = 0.5, label = paste0(s," Vessels"), size=4) +
        xlab(ylabel) +
        xlim(c(-3.1,3.1)) +
        theme_void() +
        theme(legend.position="none", 
              axis.title.y=element_text(angle=90,size=20,hjust=0.5),
              panel.border = element_rect(colour = "black", fill=NA, size=1)) +
        coord_flip() +
        scale_y_reverse(expand=expansion(add=c(0.5,1)))
      ### if 1+ vertices have self-loops ###
    } else{
      ggraph(g, 'igraph', algorithm = 'tree') + 
        geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,width=sqrt((E(g)$weight))), 
                           color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
        geom_edge_loop(aes(y=edge.y,width=sqrt((E(g)$weight)), span=50,direction=0,strength=0.90), color="grey85") +
        geom_node_point(aes(y=node.y),color=factor(V(g)$color),
                        size=10*sqrt(vsizes)/sqrt(max(vsizes))) +
        geom_node_text(aes(y=node.y.lab,label = common_name), size=5) +
        #annotate("text", x = 1.35, y = 0.5, label = paste0(s," Vessels"), size=4) +
        xlab(ylabel) +
        xlim(c(-3.1,3.1)) +
        theme_void() +
        theme(legend.position="none", 
              axis.title.y=element_text(angle=90,size=20,hjust=0.5),
              panel.border = element_rect(colour = "black", fill=NA, size=1)) +
        coord_flip() +
        scale_y_reverse(expand=expansion(add=c(0.5,1)))
    }
    
  ####### small / large vessel graphs ######
  } else{
    # save the size category
    s=unique(V(g)$vsize)
    s=ifelse(s=="large","Large","Small")
    # the graph will be labeled with the port group, for large vessels (on left of combined graph)
    if(s=="Large"){ylabel=p} else{ylabel=""}
    
    ### if none of the vertices have self-loops ###
    if(ecount(self_g) < 1){
      ggraph(g, 'igraph', algorithm = 'tree') + 
        geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,width=sqrt((E(g)$weight))), 
                           color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
        geom_node_point(aes(y=node.y),color=factor(V(g)$color),
                        size=10*sqrt(vsizes)/sqrt(max(vsizes))) +
        geom_node_text(aes(y=node.y.lab,label = common_name), size=5) +
        #annotate("text", x = 1.35, y = 0.5, label = paste0(s," Vessels"), size=4) +
        xlab(ylabel) +
        xlim(c(-2.7,2.7)) +
        # ylim(c(-0.05,1.1)) +
        theme_void() +
        theme(legend.position="none", 
              axis.title.y=element_text(angle=90,size=20,hjust=0.5),
              panel.border = element_rect(colour = "black", fill=NA, size=1)) +
        coord_flip() +
        scale_y_reverse(expand=expansion(add=c(0.5,1)))
    ### if 1+ vertices have self-loops ###
    } else{
      ggraph(g, 'igraph', algorithm = 'tree') + 
        geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,width=sqrt((E(g)$weight))), 
                           color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
        geom_edge_loop(aes(y=edge.y,width=sqrt((E(g)$weight)), span=50,direction=0,strength=0.90), color="grey85") +
        geom_node_point(aes(y=node.y),color=factor(V(g)$color),
                        size=10*sqrt(vsizes)/sqrt(max(vsizes))) +
        geom_node_text(aes(y=node.y.lab,label = common_name), size=5) +
        xlab(ylabel) +
        xlim(c(-2.7,2.7)) +
        # ylim(c(-0.09,1.15)) +
        theme_void() +
        theme(legend.position="none", 
              axis.title.y=element_text(angle=90,size=20,hjust=0.5),
              panel.border = element_rect(colour = "black", fill=NA, size=1)) +
        coord_flip() +
        scale_y_reverse(expand=expansion(add=c(0.5,1)))
    }
  }
}
