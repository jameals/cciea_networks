#' Rename Network Nodes / Vertices
#'
#' User-friendly fishery names to replace the abbreviate species and gear codes
#' that define each node (vertex) in the network. Function is marked where
#' additional names can be hard-coded into the function. Theoretically, 'str_replace'
#' should work for all renaming, but I had some trouble with it as the pipeline
#' extended beyond 4-5 lines. 
#' For scripts Fig3.Rmd and FigS5.Rmd
#'
#' @param g network as an igraph object
#' @return igraph object with newly defined "common_name" attribute
#' @examples
#' vertex_cols <- vertex_color(g = early_igraph)
#' @export
rename_vertices <- function(g){
  new_names <- V(g)$common_name %>%
    str_replace("Misc. Pot/H&L", "Other (Pot,HL)") %>%
    str_replace("Misc. Fisheries", "Other") %>%
    str_replace("Rockfish/Lcod","RockLing") %>%
    str_replace("DTS Trawl","Groundfish") %>%
    str_replace("Dcrab","D.crab")
  new_names[which(new_names=="Hagfish (pot)")] <- "Hagfish"
  new_names[which(new_names=="Sablefish (Lgl)")] <- "Sablefish"
  new_names[which(new_names=="Sablefish (pot)")] <- "Sablefish"
  new_names[which(new_names=="Chinook (trl)")] <- "Chinook"
  new_names[which(new_names=="Shrimp (pot)")] <- "Shrimp"
  new_names[which(new_names=="C. Halibut (pole)")] <- "C. Halibut"
  ## add lines HERE: new_names[which(new_names=="{old name}")] <- "{new name}" ##
  V(g)$common_name <- new_names
  return(g)
}