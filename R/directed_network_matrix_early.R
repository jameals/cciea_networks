#' Generate Directed Network, Early Season
#'
#' Create a directed network to show flow of vessels between the 2015
#' Dungeness crab fishery and 2016 alternatives during the early season.
#' Original function name: vessel_adjacency_matrix_dcrb. For script 09a
#'
#' @param tickets fish tickets data frame
#' @param p specify a port group
#' @param self calculate the diagonal of the matrix
#' @param write write out the adjacency matrix to a .csv file
#' @param outdir if write is TRUE, the directory to save the adjacency matrix
#' @param size vessel size category [large / small / NA]. used for file name if write=TRUE
#' @param noncrab_fishtix fish ticket data frame from non-crab ports (used for looking for vessel movement)
#' @return a list with (1) A: adjacency matrix to create directed network, (2) n_dcrb_vessels: total number of vessels, (3 & 4) vpf_2014 & vpf_2015: vessels participating in each fishery in 2014-15 and 2015-16
#' @examples
#' A_info <- gen_adj_matrix_early(fishtix_lg,p=p,self=TRUE,write=TRUE,size="large",noncrab_fishtix=noncrab_tix)
#' @export
gen_adj_matrix_early <- function(fishtix,p,self=FALSE,write=FALSE,outdir=NULL,size=NA,noncrab_fishtix=NULL){
  #### Subset fish tickets ####
  sub_fishtix <- filter(fishtix, pcgroup == p)
  open_info <- filter(dates_df, pcgroup == p)
  odate <- open_info$odate
  ## pull fish ticket data for given year, port group, during response period
  ext_sub_fishtix <- sub_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "early", "late"))
  sub_fishtix <- ext_sub_fishtix %>%
    filter(response=='early')
  ## pull fish ticket data for given year, OTHER port group, during response period
  op_fishtix <- fishtix %>%
    filter(pcgroup != p) %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "early", "late")) %>%
    filter(response=='early')
  if(!is.null(noncrab_fishtix)){  ## UPDATE JAN 07,2020 - with identify "other port" beyond those in fish tickets provided with metiers.
    noncrab_fishtix <- noncrab_fishtix %>%
      filter(!(pcgroup %in% unique(fishtix$pcgroup))) %>%
      filter(removal_type %in% c("COMMERCIAL (NON-EFP)","COMMERCIAL(DIRECT SALES)")) %>%
      mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
      mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "early", "late")) %>%
      filter(response=='early') %>%
      dplyr::select(drvid,pcgroup,crab_year,removal_type,response)
    op_fishtix <- op_fishtix %>%
      dplyr::select(drvid,pcgroup,crab_year,removal_type,response) %>%
      rbind(noncrab_fishtix)
  }
  
  
  
  #### Create Matrices: Within port ####
  #Grab Dungeness crab vessels from 2014-15 
  
  dcrb_vessels <- sub_fishtix %>%
    filter(crab_year==2014 & adj_revenue > 0) %>%
    filter(metier.name=="DCRB_POT") %>%
    dplyr::select(drvid) %>%
    distinct()
  
  n_dcrb_vessels <- dim(dcrb_vessels)[1]
  sub_fishtix <- filter(sub_fishtix, drvid %in% dcrb_vessels$drvid); dim(sub_fishtix)
  #2014 Matrix: 1/0 whether each vessel was in each fishery in 2014
  fishtix_2014 <- filter(sub_fishtix, crab_year==2014)
  mytab_2014 <- with(fishtix_2014, table(drvid, metier.name))
  mydf_2014 <- as.data.frame(mytab_2014)
  mydf_binary_2014 <- mydf_2014 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.name, value=in_fishery) %>%
    arrange(drvid)
  #2015 Matrix: 1/0 whether each vessel was in each fishery in 2015
  fishtix_2015 <- filter(sub_fishtix, crab_year==2015)
  mytab_2015 <- with(fishtix_2015, table(drvid, metier.name))
  mydf_2015 <- as.data.frame(mytab_2015)
  mydf_binary_2015 <- mydf_2015 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.name, value=in_fishery) %>%
    arrange(drvid)
  
  
  
  #### Subtract Matrices (2015-2014): Within port ####
  #Add in blank columns
  missing14 <- colnames(mydf_binary_2015)[which(!(colnames(mydf_binary_2015) %in% colnames(mydf_binary_2014)))]
  mydf_binary_2014[,missing14] <- 0
  missing15 <- colnames(mydf_binary_2014)[which(!(colnames(mydf_binary_2014) %in% colnames(mydf_binary_2015)))]
  mydf_binary_2015[,missing15] <- 0
  #Add in vessels that dropped out in 2015
  dropped_vessels <- mydf_binary_2014$drvid[which(!(mydf_binary_2014$drvid %in% mydf_binary_2015$drvid))]
  if(length(dropped_vessels) > 0){
    num.metiers <- dim(mydf_binary_2015)[2] -1
    emptydf <- mydf_binary_2015[0,]
    emptydf[1:length(dropped_vessels),] <- 0
    emptydf[,1] <- dropped_vessels
    dim(emptydf)
    mydf_binary_2015 <- rbind(mydf_binary_2015, emptydf)
  }
  #Turn data frames into matrices
  mydf_binary_2014$drvid <- as.character(mydf_binary_2014$drvid)
  mydf_binary_2015$drvid <- as.character(mydf_binary_2015$drvid)
  vesselmat_2014 <- data.matrix(mydf_binary_2014[,-1,drop=FALSE]); rownames(vesselmat_2014) <- mydf_binary_2014$drvid
  vesselmat_2015 <- data.matrix(mydf_binary_2015[,-1,drop=FALSE]); rownames(vesselmat_2015) <- mydf_binary_2015$drvid
  vesselmat_2015 <- vesselmat_2015[rownames(vesselmat_2014),colnames(vesselmat_2014),drop=FALSE]
  #Subtract 2014 from 2015
  vesselmat_diff <- vesselmat_2015-vesselmat_2014
  if(self){
    vesselmat_sum <- vesselmat_2015+vesselmat_2014
    for(i in seq(1,nrow(vesselmat_sum))){
      for(j in seq(1,ncol(vesselmat_sum))){
        if(vesselmat_sum[i,j]>1){
          vesselmat_diff[i,j] <- vesselmat_diff[i,j] + 2
        }
      }
    }
  } #end if(self)
  
  #### Add external ports to difference matrix ####
  #Search for vessels at other ports during the closure in 2015
  sub_op_fishtix <- op_fishtix %>%
    filter(crab_year==2015) %>%
    filter(drvid %in% rownames(vesselmat_diff) & crab_year==2015) %>%
    filter(drvid %in% dropped_vessels) ## UPDATE 01-07-2020: avoids marking active vessels as "other port", if they fished at more than two locations. makes sure that the "other_port" column truly represents departures.
  dim(sub_op_fishtix)
  op_boats <- unique(sub_op_fishtix$drvid); length(op_boats)
  #Add to landings matrix
  # data for other port sub-matrix
  tmp_dat <- ifelse(rownames(vesselmat_diff) %in% op_boats, 1,0)
  # create sub-matrix for other port column
  tmp_mat <- as.matrix(x=tmp_dat,nrow=dim(vesselmat_diff)[1], ncol=1); rownames(tmp_mat)=rownames(vesselmat_diff); colnames(tmp_mat) = c("other_port")
  # bind matrices
  vesselmat_diff<-cbind(vesselmat_diff,tmp_mat)
  
  if(write){
    write.csv(vesselmat_diff, here::here(outdir, paste0(p,"_early_Amat_",size,".csv")))
  }
  
  #### Fill in adjacency matrix ####
  #Empty matrix
  fisheries <- colnames(vesselmat_diff)
  n.metiers <- length(fisheries) -1
  fisheries <- c(fisheries, "no_fishing")
  A <- matrix(ncol = length(fisheries), nrow = length(fisheries), data = 0)
  colnames(A) <- fisheries
  rownames(A) <- fisheries
  #Add in values
  other_port_vessels <- c()
  for(i in seq(1,dim(vesselmat_diff)[1])){
    tmpdat=vesselmat_diff[i,]
    tmpdat_metiers <- vesselmat_diff[i,1:n.metiers]
    # which fisheries did this vessel leave / enter?
    enter_fishery <- names(which(tmpdat_metiers == 1))
    left_fishery <- names(which(tmpdat == -1))
    if(self){
      remain_fishery <- names(which(tmpdat == 2))
    } else{
      remain_fishery <- c() ## UPDATED JAN 07, 2020 - see below
    }
    # for each fishery the vessel left...
    for(j in left_fishery){
      if(j=="DCRB_POT"){
        ## if it didn't enter a fishery, add one to no landings or other port
        if(length(enter_fishery) == 0 & length(remain_fishery)==0){   ## UPDATED JAN 07, 2020 - prevents 1 being added to "no fishing" if vessel just remained in fisher(ies)
          ### if there were landings at another port, add 1 to "none"-->"other". otherwise, add 1 to "none"-->"no_fishing"
          if(tmpdat["other_port"]==1 & j=="DCRB_POT"){
            A[j,"other_port"] <- A[j,"other_port"] + 1
            other_port_vessels <- c(other_port_vessels, rownames(vesselmat_diff)[i])
          } else if(j=="DCRB_POT"){
            A[j,"no_fishing"] <- A[j,"no_fishing"] + 1
          }
        } else{
          ## otherwise, add one to intersect between leave / enter
          for(k in enter_fishery){
            A[j,k] <- A[j,k] + 1
          } 
        } #end else
      } #end j=="DCRB_POT"
    } #end for(j)
    if(self){
      for(l in remain_fishery){
        A[l,l] <- A[l,l] + 1
      }
    }
  } #end for(i)
  
  
  #### Get number of vessels per fishery, for later filtering ####
  vpf_2015 <- colSums(vesselmat_2015)
  vpf_2014 <- colSums(vesselmat_2014)
  
  return(list(A,n_dcrb_vessels,vpf_2014,vpf_2015))
}

