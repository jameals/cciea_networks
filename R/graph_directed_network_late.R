#' Graph the Directed Network, Late Season
#'
#' Graph a directed network to show flow of vessels between the 2015
#' Dungeness crab fishery and 2016 alternatives during the late season.
#' Original function name: plot_graph. For script FigS5.Rmd
#'
#' @param g igraph object
#' @param dist_y set the vertical distance between network nodes (this is actually on the x axis due to coord_flip())
#' @return a ggraph object
#' @examples
#' plot_directed_late(g=mygraph,dist_y=1.4)
#' @export
plot_directed_late = function (g, dist_y=1.8) {
  ## prepare for plotting ##
  
  # color and size of vertices
  p=unique(V(g)$port)
  V(g)$color <- vertex_color(g)
  vsizes=V(g)$size
  
  # for plotting self-loops
  self_g = delete.edges(g,E(g)[!(which_loop(g))])
  self_loops <- as_edgelist(self_g)[,1]
  
  ## set location of nodes ##
  # initiate data frame & specify order of nodes (fishery --> d.crab --> no fishing / other port) 
  node.x <- c(); node.x.lab <- c()
  m=-1.5; d=-1; f=0
  node.x.df <- data.frame(node=as.character(),
                          xval=as.numeric())
  
  # Did vessels shift into D. crab fishery?
  el <- data.frame(as_edgelist(g)) %>%
    mutate(X1=as.character(X1),
           X2=as.character(X2))
  crab_flow <- ifelse(dim(dplyr::filter(el,X1 != "DCRB_POT" & X2=="DCRB_POT"))[1] > 0,TRUE,FALSE)
  
  # loop through nodes and assign x position
  for(n in V(g)$name){
    if(!(n %in% c("other_port","no_fishing"))){
      if(n=="DCRB_POT" & crab_flow){
        node.x <- c(node.x, d)
        node.x.lab <- c(node.x.lab, d-0.4)
        node.x.df <- rbind(node.x.df, data.frame(node=as.character(n),xval=d))
      } else{
        node.x <- c(node.x, m)
        node.x.lab <- c(node.x.lab,m)
        node.x.df <- rbind(node.x.df, data.frame(node=as.character(n),xval=m))
        m <- m + dist_y
      }
    } else{
      node.x <- c(node.x, f)
      node.x.lab <- c(node.x.lab, f)
      node.x.df <- rbind(node.x.df, data.frame(node=as.character(n),xval=f))
      f <- f + dist_y
    }
  }
  node.x.df$node <- as.character(node.x.df$node)
  el <- left_join(el,node.x.df,by=c("X1"="node"))
  el <- left_join(el,node.x.df,by=c("X2"="node")); colnames(el) <- c("X1","X2", "edge.x","edge.xend")
  # set y position; if crab_flow=TRUE, the d.crab node will be in the middle instead of the left side
  node.y <- unlist(lapply(V(g)$name,FUN=function(i){ifelse(i == "DCRB_POT" & crab_flow, 0.5,ifelse(i %in% c("other_port","no_fishing"),0,0.9))}))
  node.y.lab <- ifelse(node.y==0.9, 1.2, ifelse(node.y==0,-0.4,0.5))
  edge.y <- unlist(lapply(el[,1],FUN=function(i){ifelse(i == "DCRB_POT" & crab_flow, 0.5,ifelse(i %in% c("other_port","no_fishing"),0,0.9))}))
  edge.yend <- unlist(lapply(el[,2],FUN=function(i){ifelse(i == "DCRB_POT" & crab_flow, 0.5,ifelse(i %in% c("other_port","no_fishing"),0,0.9))}))
  # a 0.3 offset for the labels isn't enough when there are 3 columns of nodes
  if(any(node.y==0)){node.y.lab[which(node.y.lab==1.2)] <- 1.4}
  # a 0.3 offset for the labels is too much when there is one column of nodes
  if(all(node.y==0.9)){node.y.lab[which(node.y.lab==1.2)] <- 1}
  
  
  # how many fisheries total?
  nmets <- length(V(g)$name)
  
  # what vessel size is being plotted? capitalize size class
  s=unique(V(g)$vsize)
  if(s=="large"){
    s="Large"; ylabel=p
  } else{
    s="Small"; ylabel=""
  }
  
  # set x upper limit based on number of nodes
  x_up <- max(node.x.df$xval) + 1.75
  
  # rescale node size
  vsizes_scaled <- scales::rescale(c(1,vsizes), to=c(2,10))
  vsizes_scaled <- vsizes_scaled[2:length(vsizes_scaled)]
  
  ## graph of large / small vessels ##
  # if the graph does not include self-loops...
  if(ecount(self_g) < 1){
    ggraph(g, 'igraph', algorithm = 'tree') + 
      geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,x=el$edge.x, xend=el$edge.xend,width=((E(g)$weight)^(1/3))), 
                         color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
      geom_node_point(aes(y=node.y, x = node.x),color=factor(V(g)$color),
                      size=vsizes_scaled) +
      geom_node_text(aes(y=node.y.lab,x=node.x.lab,label = common_name), size=5) +
      xlab(ylabel) +
      xlim(c(-2.5,6.5)) +
      # ylim(c(-0.05,1.1)) +
      theme_void() +
      theme(legend.position="none", 
            axis.title.y=element_text(size=18,hjust=0.5,angle=90),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      coord_flip() +
      scale_y_reverse(expand=expansion(mult=0.4))
  } 
  # if the graph does include self-loops
  else{
    ggraph(g, 'igraph', algorithm = 'tree') + 
      geom_edge_diagonal(aes(y=edge.y,yend=edge.yend,x=el$edge.x, xend=el$edge.xend,width=((E(g)$weight)^(1/3))), 
                         color="grey85",end_cap = circle(0.5),arrow=arrow(length=unit(0.3,'cm'))) +
      geom_edge_loop(aes(y=edge.y,x=el$edge.x,width=(E(g)$weight)^(1/3), span=50,direction=0,strength=1.30), color="grey85") +
      geom_node_point(aes(y=node.y, x = node.x),color=factor(V(g)$color),
                      size=vsizes_scaled) +
      geom_node_text(aes(y=node.y.lab,x=node.x.lab,label = common_name), size=5) +
      xlab(ylabel) +
      xlim(c(-2.5,x_up)) +
      # ylim(c(-0.09,1.15)) +
      theme_void() +
      theme(legend.position="none", 
            axis.title.y=element_text(size=18,hjust=0.5,angle=90),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      coord_flip() +
      scale_y_reverse(expand=expansion(mult=0.4))
  }
}