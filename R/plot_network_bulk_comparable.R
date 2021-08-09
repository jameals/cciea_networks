#' Plot Comparable Participation Networks
#'
#' Use igraph objects to plot a series of fisheries participation
#' networks that have consistent node placement across plots.
#'
#' @param g network as an igraph object
#' @param outdir directory where image files will be saved
#' @param file_suffix string to be added to the suffix of the image file name. script will automatically name the files by port group and year.
#' @return Null. Will write plots directly to files
#' @examples
#' plot_comparable_networks(graphs_list, outdir="data/networks/participation/plots/comparable")
#' @export
plot_comparable_networks <- function(glist, outdir, file_suffix=paste0("_cciea_bulk_",contr_cutoff*100, "_compare")){
  
  ######## set up - functions ########
  # get the vertex IDs for a graph
  get_vid <- function(igraph_obj){
    vertex_attr(igraph_obj)[[1]]
  }

  # add () to vessel counts, and remove 0 vessel counts, for plotting
  add_vessel_labels <- function(igraph_obj){
    label_vec <- c()
    for(i in seq(1,length(V(igraph_obj)$name))){
      if(V(igraph_obj)$vessels[i] != 0){
        label_vec[i] <- paste0(" (", V(igraph_obj)$vessels[i], ")")
      } else{
        label_vec[i] <- ""
      }
    }
    return(label_vec)
  }
  ###################################
  
  
  # get all the unique node IDs from the list of graphs you want to compare
  unique_node_ids <- unique(unlist(lapply(glist,get_vid)))
  
  # build a matrix to see which graphs have which nodes
  node_mat <- matrix(nrow=length(unique_node_ids), ncol=length(glist), dimnames=list(c(unique_node_ids), 
                                                                         c(seq(1,length(glist)))))
  for(i in seq(1,dim(node_mat)[2])){     # for each graph
    for(j in seq(1,dim(node_mat)[1])){   # for each node
      tmp_g <- glist[[i]]
      tmp_node <- unique_node_ids[j]
      if(tmp_node %in% get_vid(tmp_g)){
        node_mat[j,i] <- 1
      } else{
        node_mat[j,i] <- 0
      }
    }
  }
  
  # if any graphs are missing a node, add it in as a blank
  new_glist = list()
  for(i in seq(1,dim(node_mat)[2])){
    tmp_g <- glist[[i]]
    if(any(node_mat[,i] == 0)){
      # identify the missing nodes
      missing_nodes <- c(unique_node_ids[which(!(unique_node_ids %in% get_vid(tmp_g)))])
      for(j in seq(1,length(missing_nodes))){
        tmp_node <- missing_nodes[j]
        
        # get attributes from a graph that does have that node
        ref_g <- glist[[which(node_mat[tmp_node,] == 1)[[1]]]]
        ref_attr <- vertex_attr(ref_g, index=which(V(ref_g)$name == tmp_node))
        
        # reset graph-specific attributes 
        # -- to zero
        ref_attr$size         <- 0
        ref_attr$percent_size <- 0
        ref_attr$importance   <- 0
        ref_attr$vessels      <- 0
        # -- to reflect new graph
        ref_attr$p            <- unique(V(tmp_g)$p)
        ref_attr$year         <- unique(V(tmp_g)$year)
        ref_attr$fleet        <- unique(V(tmp_g)$fleet)
        
        # add new node to graph
        if(j==1){
          new_tmp_g <- add_vertices(tmp_g, nv=1, attr=ref_attr)
        } else{
        new_tmp_g <- add_vertices(new_tmp_g, nv=1, attr=ref_attr)
        }
        
      } # end for(j)
      new_glist[[i]] <- new_tmp_g
      message("added ", length(missing_nodes), " empty vertices to graph ", i)
    
      } else{
      new_glist[[i]] <- tmp_g}  # end if(any == 0)
  } # for(i)
  
  
  
  # plot each graph, using the same order of nodes
  for(i in seq(1,length(new_glist))){
    g <- new_glist[[i]]
    # get port group / year info from graph object
    port_group <- unique(V(g)$p)
    y <- unique(V(g)$year)
    # prepare the layout with the `layout_in_circle` function, which allows you to specify vertex order for plot
    new_order <- match(unique_node_ids,V(g)$name)
    l <-layout_in_circle(g, order=new_order)
    # remove labels for empty nodes
    V(g)$common_name[which(V(g)$vessels == 0)] <- ""
    V(g)$vessel_label <- add_vessel_labels(g)
    
    # plot & save to file
    png(here::here(outdir, paste0(port_group,"_", y,"_circular",file_suffix,".png")),bg="transparent")  # if this resolution isn't good enough, add: width = 2000, height = 1500,res=300
    if(vcount(g) == 1 | ecount(g) == 0){
      plot(g, vertex.size = V(g)$importance/(max(V(g)$importance)*0.02), 
           layout = l, #where to put the nodes on the plot
           # edge.width = sqrt(E(g)$weight)/(0.037*max(sqrt(E(g)$weight))),
           edge.curved = F, 
           axes = F,
           edge.color = brewer.pal(n = 11, name = "Set3")[9],
           vertex.label = str_wrap(paste0(V(g)$common_name, V(g)$vessel_label),
                                   width = 10), # JS updated 01-29-2021
           vertex.color = adjustcolor(V(g)$colors, alpha.f=0.90),
           vertex.label.family = 'sans', 
           # vertex.label.color = V(g)$colors, # to have same color as vertices
           vertex.label.color = "gray25",
           vertex.label.cex= 1.6, # changed from 1.2, JS 01-29-21
           vertex.frame.color=NA
           # vertex.label.dist = c(-9,-8,-9,-10),               # these can be adjusted manually to make it look nicer, which is super annoying
           # vertex.label.degree = c(pi^(0.9),pi/5,pi/5,pi*1.1) # these can be adjusted manually to make it look nicer, which is super annoying
      )
      dev.off()
    } else{
      plot(g, vertex.size = V(g)$importance/(max(V(g)$importance)*0.02), 
           layout = l, #where to put the nodes on the plot
           edge.width = sqrt(E(g)$weight)/(0.037*max(sqrt(E(g)$weight))),
           edge.curved = F, 
           axes = F,
           edge.color = brewer.pal(n = 11, name = "Set3")[9],
           # vertex.label = remove_empty_labels(g), 
           vertex.label = str_wrap(paste0(V(g)$common_name, V(g)$vessel_label),
                 width = 10), # JS updated 01-29-2021
           vertex.color =adjustcolor(V(g)$colors, alpha.f=0.90),
           vertex.label.family = 'sans', 
           # vertex.label.color = V(tmp_g)$colors, # to have same color as vertices
           vertex.label.color = "grey25",
           vertex.label.cex= 1.6, # changed from 1.2, JS 01-29-21
           vertex.frame.color=NA
           # vertex.label.dist = c(-9,-8,-9,-10),               # these can be adjusted manually to make it look nicer, which is super annoying
           # vertex.label.degree = c(pi^(0.9),pi/5,pi/5,pi*1.1) # these can be adjusted manually to make it look nicer, which is super annoying
      )
      dev.off()
    }
    
  } # end for(i in new_glist)
    
    
}
  