#' Average Similarities
#'
#' Some pairwise trip comparisons will be duplicated if similarities 
#' are calculated by gear group. This function averages the similarities
#' from duplicated comparisons, and replaces all duplicates in the full
#' data frame of pairwise trip similarities. For script 01, section 4.2
#'
#' @param duplicates_df data frame containing duplicate entries of trip comparisons
#' @param similarities data frame with all pairwise trip comparisons
#' @param plot if TRUE, print plot to compare distributions of duplicated v. averaged similarities
#' @return similarity data frame without duplicates, and new averaged similarity values
#' @examples
#' similarities <- average_similarities(duplicates_df=duplicate_rows, similarities = similarities, plot=TRUE)
#' @export
average_similarities <- function(duplicates_df, similarities, plot = TRUE){
  # average duplicated values by the 'grp' value 
  #    (will be the same for true & reciprocal duplicates)
  avg_df <- duplicates_df %>%
    group_by(grp) %>%
    summarise(avg.sim=mean(value.sim,na.rm=TRUE),
              sd.sim=sd(value.sim,na.rm=TRUE)) %>%
    mutate(value.sim=avg.sim) %>% dplyr::select(-avg.sim)
  
  if(plot){
    # Standard deviation for averaged similarities
    print(ggplot(avg_df,aes(x=grp,y=sd.sim)) + 
            geom_point(size=2) + ylab("St.Dev. of Avg Sim") +
            theme_bw() + theme(axis.text.x=element_text(angle=90,hjust=1)))
    # Compare new averaged similarities to old.
    suppressWarnings(print(ggplot() +
      geom_histogram(data=duplicates_df, aes(x = value.sim), fill="lightblue") +
      geom_histogram(data=avg_df, aes(x = value.sim), fill="coral", alpha = 0.5) +
      ylab("Trip Pairs") +
      xlab("Similarity") +
      ggtitle("Compare duplicate to average similarities") + theme_bw()))
  }
  # Remove trip comparisons at given indices
  similarities_nodup <- anti_join(similarities, duplicates_df,by=c("trip1","trip2"))
  similarities_nodup <- anti_join(similarities, duplicates_df,by=c("trip1"="trip2",
                                                                   "trip2"="trip1"))
  
  # Add in the averaged trip comparisons
  new_similarities <- duplicates_df %>%
    left_join(avg_df, by="grp") %>%
    # make the average value the new similarity value
    mutate(value.sim=value.sim.y) %>%
    dplyr::select(trip1,trip2,value.sim,gear)
  similarities_nodup <- rbind(similarities_nodup, new_similarities)
  
  # plot the similarity score distribution by gear group
  if(plot){
      suppressWarnings(print(ggplot(similarities_nodup, aes(x=value.sim)) +
                               geom_histogram() +
                               facet_wrap(~gear, scales="free_y") +
                               xlab("Similarity Metric") +
                               ylab("Number of Comparisons") +
                               ggtitle(paste0("Final Similarities")) + theme_bw()))
    }
  
  return(similarities_nodup)
}
