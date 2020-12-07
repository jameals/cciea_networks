#' KNN Metier Assignment
#'
#' Assign new fish ticket data metiers based on the k-nearest neighbor algorithm (KNN),
#' which is conducted using customized R code in this function (instead of a hard-coded
#' R function). For use in script 03 section 1.2
#'
#' @param train per-trip revenue data with metiers ('neighbors').
#' @param key A data frame with two columns: trip_id, and the identified metier from infomap
#' @param test per-trip revenue data without metiers
#' @param k The k value to use in the k-nearest neighbor algorithm
#' @param adjust.revenue If TRUE, uses the adjusted revenue in the catch matrix; if FALSE, raw reported exvessel revenue
#' @param print.plot If TRUE, plot membership to metiers
#' @return data frame with assigned metiers
#' @examples
#' tmp_test_metiers <- assign_metiers_bygear(train = traindat, key = metier_key, test = tmp_tix, k = 2)
#' @export
knn_assign_metiers <- function(train, key, test, k, adjust.revenue = TRUE, print.plot=TRUE){
  message("KNN function will have ", length(unique(train$trip_id)),  " trips in training set and ",length(unique(test$trip_id)), " trips to assign.")
  
  # which gear group are we working with?
  gears <- unique(test$grgroup)
  mygear <- gears[which.max(tabulate(match(test$grgroup,gears)))]
  
  
  # Combine the train and test data into a single matrix
  if(all(colnames(train) == colnames(test))){
    all_tickets <- rbind(train, test)
  } else{
    stop("ERROR: training and testing data do not have same number or order of columns.")
  }
  
  # Convert each data frame into a distance matrix
  ### the function combines multiple rows for same ticket and produces matrix
  ### column names will be spid/grgroup combos
  tickets_spread <- generate_catch_matrix_knn(tickets=all_tickets, multigear=TRUE, adjust.revenue=adjust.revenue)
  ### move trip_id to rownames
  tickets_mat <- as.matrix(dplyr::select(tickets_spread, -trip_id))
  rownames(tickets_mat) <- tickets_spread$trip_id
  rm(tickets_spread)
  ### relativize each row by sqrt(total), find euclidean distance
  tickets_dist <- as.matrix(parallelDist(decostand(tickets_mat, "hellinger"), method="euclidean"))
  ## add trip ids to distance matrix
  rownames(tickets_dist) <- rownames(tickets_mat)
  colnames(tickets_dist) <- rownames(tickets_mat)
  
  
  # Subset distance matrix
  design.set <- unique(as.character(train$trip_id))
  test.set <- unique(as.character(test$trip_id))
  NN.dist <- tickets_dist[design.set, test.set]
  rm(tickets_dist)
  
  
  # Predict group membership, when there is more than one ticket
  predict_metier <- function(nn, k=1){
    if(round(max(nn), digits = 10) != round(min(nn), digits=10)){ ## prevents metier assignment if trip is equally dissimilar to all trips in train data
      nn.order <- order(nn) ## change to function: ordering completed within function on single column
      nn.sub <- nn.order[1:k] ## change to function: subset of ordered data completed within function on single column
      ids <- design.set[nn.sub]
      tab <- table(key$metier.num[which(key$trip_id %in% ids)])
      names(which.max(tab))
    } else{ 
      return(NA) ## for any trips equally dissimilar to all training data trips
    }
  } #end function 
  if(length(test.set) > 1){
    start.time <- Sys.time()
    ## Prediction function using "K" nearest neighbors
    pred <- apply(NN.dist, 2, predict_metier, k=k)
    message(paste0("Time to predict: ", Sys.time()-start.time))
    ## Make data frame of predicted metiers
    pred_metiers <- as.data.frame(pred)
    pred_metiers <- data.frame(trip_id = names(pred),
                               metier.num = pred_metiers$pred)
    
  } else if(length(test.set == 1)){
    pred_metiers <- data.frame(trip_id = test.set,
                               metier.num = predict_metier(NN.dist, k = k))
  }
  
  ## Plot membership to metiers
  if(print.plot==TRUE){
    if(all(is.na(pred_metiers$metier.num))){
      message("No metiers assigned. Skipped plotting.")
    } else{
      ### make a table showing pairwise comparison of metier assignments
      predictions_table <- table(pred_metiers[, c("metier.num")])
      predictions_df <- as.data.frame(predictions_table)
      ### plot
      myplot <- ggplot(data = predictions_df, aes(x = Var1, y = Freq)) +
        geom_col() +
        xlab("Assigned Metier") +
        ylab("Total Tickets") +
        ggtitle(paste0("K = ", k, "; ", mygear)) +
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              axis.title = element_text(size = 11))
      print(myplot)
    }
  } #end plot 
  
  # Return a data frame of tickets with infomap-identified and knn-assigned tickets
  return(pred_metiers)
}


