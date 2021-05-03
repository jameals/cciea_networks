########### Create adjacency matrix for vessel flow network ###########
# 
# 5/30/2019 - M. Fisher
#
#######################################################################


########### Create Adjacency Matrices for Flow Networks ###########
vessel_adjacency_matrix <- function(fishtix,p,r,filter=FALSE){
  #### Subset fish tickets ####
  sub_fishtix <- filter(fishtix, pcgroup == p)
  open_info <- filter(dates_df, pcgroup == p)
  odate <- open_info$odate
  ## pull fish ticket data for given year, port group, during response period
  sub_fishtix <- sub_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response==r)
  ## pull fish ticket data for given year, OTHER port group, during response period
  op_fishtix <- filter(fishtix, pcgroup != p)
  op_fishtix <- op_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response==r)
  
  
  
  #### Create Matrices: Within port ####
  #Grab Dungeness crab vessels from 2014
  dcrb_vessels <- sub_fishtix %>%
    filter(crab_year==2014 & adj_revenue > 0) %>%
    filter(metier.2010=="DCRB_POT") %>%
    dplyr::select(drvid) %>%
    distinct()
  n_dcrb_vessels <- dim(dcrb_vessels)[1]
  sub_fishtix <- filter(sub_fishtix, drvid %in% dcrb_vessels$drvid); dim(sub_fishtix)
  #2014 Matrix: 1/0 whether each vessel was in each fishery in 2014
  fishtix_2014 <- filter(sub_fishtix, crab_year==2014)
  mytab_2014 <- with(fishtix_2014, table(drvid, metier.2010))
  mydf_2014 <- as.data.frame(mytab_2014)
  mydf_binary_2014 <- mydf_2014 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
    arrange(drvid)
  #2015 Matrix: 1/0 whether each vessel was in each fishery in 2015
  fishtix_2015 <- filter(sub_fishtix, crab_year==2015)
  mytab_2015 <- with(fishtix_2015, table(drvid, metier.2010))
  mydf_2015 <- as.data.frame(mytab_2015)
  mydf_binary_2015 <- mydf_2015 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
    arrange(drvid)
  
  
  
  #### Subtract Matrices (2015-2014): Within port ####
  #Add in blank columns
  missing14 <- colnames(mydf_binary_2015)[which(!(colnames(mydf_binary_2015) %in% colnames(mydf_binary_2014)))]
  mydf_binary_2014[,missing14] <- 0
  
  missing15 <- colnames(mydf_binary_2014)[which(!(colnames(mydf_binary_2014) %in% colnames(mydf_binary_2015)))]
  mydf_binary_2015[,missing15] <- 0
  #Add in vessels that dropped out in 2015
  dropped_vessels <- mydf_binary_2014$drvid[which(!(mydf_binary_2014$drvid %in% mydf_binary_2015$drvid))]
  num.metiers <- dim(mydf_binary_2015)[2] -1
  emptydf <- mydf_binary_2015[0,]
  emptydf[1:length(dropped_vessels),] <- 0
  emptydf[,1] <- dropped_vessels
  dim(emptydf)
  mydf_binary_2015 <- rbind(mydf_binary_2015, emptydf)
  #Turn data frames into matrices
  vesselmat_2014 <- data.matrix(mydf_binary_2014); rownames(vesselmat_2014) <- mydf_binary_2014$drvid; vesselmat_2014 <- vesselmat_2014[,-1]
  vesselmat_2015 <- data.matrix(mydf_binary_2015); rownames(vesselmat_2015) <- mydf_binary_2015$drvid; vesselmat_2015 <- vesselmat_2015[,-1]
  vesselmat_2015 <- vesselmat_2015[,colnames(vesselmat_2014)]
  #Subtract 2014 from 2015
  vesselmat_diff <- vesselmat_2015-vesselmat_2014
  
  
  #### Add external ports to difference matrix ####
  #Search for vessels at other ports during the closure in 2015
  sub_op_fishtix <- op_fishtix %>%
    filter(crab_year==2015) %>%
    filter(drvid %in% rownames(vesselmat_diff) & crab_year==2015)
  dim(sub_op_fishtix)
  op_boats <- unique(sub_op_fishtix$drvid); length(op_boats)
  #Add to landings matrix
  # data for other port sub-matrix
  tmp_dat <- ifelse(rownames(vesselmat_diff) %in% op_boats, 1,0)
  # create sub-matrix for other port column
  tmp_mat <- as.matrix(x=tmp_dat,nrow=dim(vesselmat_diff)[1], ncol=1); rownames(tmp_mat)=rownames(vesselmat_diff); colnames(tmp_mat) = c("other_port")
  # bind matrices
  vesselmat_diff<-cbind(vesselmat_diff,tmp_mat)
  
  
  #### Fill in adjacency matrix ####
  #Empty matrix
  fisheries <- colnames(vesselmat_diff)
  n.metiers <- length(fisheries) -1
  fisheries <- c(fisheries, "none", "no_fishing")
  A <- matrix(ncol = length(fisheries), nrow = length(fisheries), data = 0)
  colnames(A) <- fisheries
  rownames(A) <- fisheries
  #Add in values
  other_port_vessels <- c()
  for(i in seq(1,dim(vesselmat_diff)[1])){
    tmpdat=vesselmat_diff[i,]
    tmpdat_metiers <- vesselmat_diff[i,1:n.metiers]
    # which fisheries did this vessel leave / enter?
    enter_fishery <- names(which(tmpdat_metiers > 0))
    left_fishery <- names(which(tmpdat < 0))
    # for each fishery the vessel left...
    for(j in left_fishery){
      ## if it didn't enter a fishery, add one to the "none" column - ie. no landings at given port
      if(length(enter_fishery) == 0){
        A[j,"none"] <- A[j,"none"] + 1
        ### if there were landings at another port, add 1 to "none"-->"other". otherwise, add 1 to "none"-->"no_fishing"
        if(tmpdat["other_port"]==1 & j=="DCRB_POT"){
          A["none","other_port"] <- A["none","other_port"] + 1
          other_port_vessels <- c(other_port_vessels, rownames(vesselmat_diff)[i])
        } else if(j=="DCRB_POT"){
          A["none","no_fishing"] <- A["none","no_fishing"] + 1
        }
      } else{
        ## otherwise, add one to intersect between leave / enter
        for(k in enter_fishery){
          A[j,k] <- A[j,k] + 1
        }
      }
    }
  }
  
  
  
  #### Get number of vessels per fishery, for later filtering ####
  vpf_2015 <- colSums(vesselmat_2015)
  vpf_2014 <- colSums(vesselmat_2014)
  
  return(list(A,n_dcrb_vessels,vpf_2014,vpf_2015))
}



vessel_adjacency_matrix_dcrb <- function(fishtix,p,r,filter=FALSE, self=FALSE,write=FALSE,size=NA,noncrab_fishtix=NULL){
  #### Subset fish tickets ####
  sub_fishtix <- filter(fishtix, pcgroup == p)
  open_info <- filter(dates_df, pcgroup == p)
  odate <- open_info$odate
  ## pull fish ticket data for given year, port group, during response period
  ext_sub_fishtix <- sub_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open"))
  sub_fishtix <- ext_sub_fishtix %>%
    filter(response==r)
  ## pull fish ticket data for given year, OTHER port group, during response period
  op_fishtix <- fishtix %>%
    filter(pcgroup != p) %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response==r)
  if(!is.null(noncrab_fishtix)){  ## UPDATE JAN 07,2020 - with identify "other port" beyond those in fish tickets provided with metiers.
    noncrab_fishtix <- noncrab_fishtix %>%
      filter(!(pcgroup %in% unique(fishtix$pcgroup))) %>%
      filter(removal_type %in% c("COMMERCIAL (NON-EFP)","COMMERCIAL(DIRECT SALES)")) %>%
      mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
      mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
      filter(response==r) %>%
      dplyr::select(drvid,pcgroup,crab_year,removal_type,response)
    op_fishtix <- op_fishtix %>%
      dplyr::select(drvid,pcgroup,crab_year,removal_type,response) %>%
      rbind(noncrab_fishtix)
  }
  
  
  
  #### Create Matrices: Within port ####
  #Grab Dungeness crab vessels from 2014
  if(r=="closure"){
    dcrb_vessels <- sub_fishtix %>%
      filter(crab_year==2014 & adj_revenue > 0) %>%
      filter(metier.2010=="DCRB_POT") %>%
      dplyr::select(drvid) %>%
      distinct()
  } else{
    dcrb_vessels <- ext_sub_fishtix %>%
      filter(crab_year==2014 & adj_revenue > 0) %>%
      filter(metier.2010=="DCRB_POT") %>%
      dplyr::select(drvid) %>%
      distinct()
  }
  n_dcrb_vessels <- dim(dcrb_vessels)[1]
  sub_fishtix <- filter(sub_fishtix, drvid %in% dcrb_vessels$drvid); dim(sub_fishtix)
  #2014 Matrix: 1/0 whether each vessel was in each fishery in 2014
  fishtix_2014 <- filter(sub_fishtix, crab_year==2014)
  mytab_2014 <- with(fishtix_2014, table(drvid, metier.2010))
  mydf_2014 <- as.data.frame(mytab_2014)
  mydf_binary_2014 <- mydf_2014 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
    arrange(drvid)
  #2015 Matrix: 1/0 whether each vessel was in each fishery in 2015
  fishtix_2015 <- filter(sub_fishtix, crab_year==2015)
  mytab_2015 <- with(fishtix_2015, table(drvid, metier.2010))
  mydf_2015 <- as.data.frame(mytab_2015)
  mydf_binary_2015 <- mydf_2015 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
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
  #Add in vessels that fished later in 2015 than in 2014 (open period only)
  if(r=="open"){
    dropped_vessels <- mydf_binary_2015$drvid[which(!(mydf_binary_2015$drvid %in% mydf_binary_2014$drvid))]
    if(length(dropped_vessels) > 0){
      num.metiers <- dim(mydf_binary_2014)[2] -1
      emptydf <- mydf_binary_2014[0,]
      emptydf[1:length(dropped_vessels),] <- 0
      emptydf[,1] <- dropped_vessels
      dim(emptydf)
      mydf_binary_2014 <- rbind(mydf_binary_2014, emptydf)
    }
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
  }
  
  
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
    write.csv(vesselmat_diff, paste0("output/networks/crab_vessel/",p,"_",r,"_Avesselmat_",size,"_v7_newVL.csv"))
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
  if(r=="closure"){
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
  }  else{
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
          ## if it didn't enter a fishery, add one to no landings or other port
          if(length(enter_fishery) == 0 & length(remain_fishery)==0){  ## UPDATED JAN 07, 2020 - prevents 1 being added to "no fishing" if vessel just remained in fisher(ies)
            ### if there were landings at another port, add 1 to "none"-->"other". otherwise, add 1 to "none"-->"no_fishing"
            if(tmpdat["other_port"]==1){
              A[j,"other_port"] <- A[j,"other_port"] + 1
              other_port_vessels <- c(other_port_vessels, rownames(vesselmat_diff)[i])
            } else{
              A[j,"no_fishing"] <- A[j,"no_fishing"] + 1
            }
          } else{
            ## otherwise, add one to intersect between leave / enter
            for(k in enter_fishery){
              A[j,k] <- A[j,k] + 1
            } 
          } #end else
      } #end for(j)
      if(self){
        for(l in remain_fishery){
          A[l,l] <- A[l,l] + 1
        }
      } #end self
    } #end for(i)
  } #end else(r==open)
  
  
  #### Get number of vessels per fishery, for later filtering ####
  vpf_2015 <- colSums(vesselmat_2015)
  vpf_2014 <- colSums(vesselmat_2014)
  
  return(list(A,n_dcrb_vessels,vpf_2014,vpf_2015))
}



vessel_adjacency_matrix_dcrb_centered <- function(fishtix,p,r,filter=FALSE, self=FALSE,write=FALSE,size=NA,noncrab_fishtix=NULL){
  #### Subset fish tickets ####
  sub_fishtix <- filter(fishtix, pcgroup == p)
  open_info <- filter(dates_df, pcgroup == p)
  odate <- open_info$odate
  ## pull fish ticket data for given year, port group, during response period
  ext_sub_fishtix <- sub_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open"))
  sub_fishtix <- ext_sub_fishtix %>%
    filter(response==r)
  ## pull fish ticket data for given year, OTHER port group, during response period
  op_fishtix <- filter(fishtix, pcgroup != p)
  op_fishtix <- op_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response==r)
  if(!is.null(noncrab_fishtix)){  ## UPDATE JAN 07,2020 - with identify "other port" beyond those in fish tickets provided with metiers.
    noncrab_fishtix <- noncrab_fishtix %>%
      filter(!(pcgroup %in% unique(fishtix$pcgroup))) %>%
      filter(removal_type %in% c("COMMERCIAL (NON-EFP)","COMMERCIAL(DIRECT SALES)")) %>%
      mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
      mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
      filter(response==r) %>%
      dplyr::select(drvid,pcgroup,crab_year,removal_type,response)
    op_fishtix <- op_fishtix %>%
      dplyr::select(drvid,pcgroup,crab_year,removal_type,response) %>%
      rbind(noncrab_fishtix)
  }
  
  
  
  #### Create Matrices: Within port ####
  #Grab Dungeness crab vessels from 2014. 
  if(r=="closure"){
    dcrb_vessels <- sub_fishtix %>%
      filter(crab_year==2014 & adj_revenue > 0) %>%
      filter(metier.2010=="DCRB_POT") %>%
      dplyr::select(drvid) %>%
      distinct()
  } else{
    ## will pull from both periods to track d. crab vessels that may have fished later in the season than normal
    dcrb_vessels <- ext_sub_fishtix %>%
      filter(crab_year==2014 & adj_revenue > 0) %>%
      filter(metier.2010=="DCRB_POT") %>%
      dplyr::select(drvid) %>%
      distinct()
  }
  n_dcrb_vessels <- dim(dcrb_vessels)[1]
  sub_fishtix <- filter(sub_fishtix, drvid %in% dcrb_vessels$drvid); dim(sub_fishtix)
  #2014 Matrix: 1/0 whether each vessel was in each fishery in 2014
  fishtix_2014 <- filter(sub_fishtix, crab_year==2014)
  mytab_2014 <- with(fishtix_2014, table(drvid, metier.2010))
  mydf_2014 <- as.data.frame(mytab_2014)
  mydf_binary_2014 <- mydf_2014 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
    arrange(drvid)
  #2015 Matrix: 1/0 whether each vessel was in each fishery in 2015
  fishtix_2015 <- filter(sub_fishtix, crab_year==2015)
  mytab_2015 <- with(fishtix_2015, table(drvid, metier.2010))
  mydf_2015 <- as.data.frame(mytab_2015)
  mydf_binary_2015 <- mydf_2015 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
    arrange(drvid)
  
  
  
  #### Subtract Matrices (2015-2014): Within port ####
  #Add in blank columns
  missing14 <- colnames(mydf_binary_2015)[which(!(colnames(mydf_binary_2015) %in% colnames(mydf_binary_2014)))]
  mydf_binary_2014[,missing14] <- 0
  missing15 <- colnames(mydf_binary_2014)[which(!(colnames(mydf_binary_2014) %in% colnames(mydf_binary_2015)))]
  mydf_binary_2015[,missing15] <- 0
  #Add in vessels that dropped out in 2015
  dropped_vessels <- mydf_binary_2014$drvid[which(!(mydf_binary_2014$drvid %in% mydf_binary_2015$drvid))]
  dropped_count <- length(dropped_vessels)
  if(dropped_count > 0){
    num.metiers <- dim(mydf_binary_2015)[2] -1
    emptydf <- mydf_binary_2015[0,]
    emptydf[1:length(dropped_vessels),] <- 0
    emptydf[,1] <- dropped_vessels
    dim(emptydf)
    mydf_binary_2015 <- rbind(mydf_binary_2015, emptydf)
  }
  #Add in vessels that fished later in 2015 than in 2014 (open period only)
  if(r=="open"){
    dropped_vessels14 <- mydf_binary_2015$drvid[which(!(mydf_binary_2015$drvid %in% mydf_binary_2014$drvid))]
    if(length(dropped_vessels14) > 0){
      num.metiers <- dim(mydf_binary_2014)[2] -1
      emptydf <- mydf_binary_2014[0,]
      emptydf[1:length(dropped_vessels14),] <- 0
      emptydf[,1] <- dropped_vessels14
      dim(emptydf)
      mydf_binary_2014 <- rbind(mydf_binary_2014, emptydf)
    }
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
  }
  
  
  #### Add external ports to difference matrix ####
  #Search for vessels at other ports during the closure in 2015
  sub_op_fishtix <- op_fishtix %>%
    filter(crab_year==2015) %>%
    filter(drvid %in% rownames(vesselmat_diff) & crab_year==2015) %>%
    filter(drvid %in% dropped_vessels) ## UPDATE 01-07-2020: avoids marking active vessels as "other port", if they fished at more than two locations. makes sure that the "other_port" column truly represents departures.
  dim(sub_op_fishtix)
  op_boats <- unique(sub_op_fishtix$drvid); op_count <- length(op_boats)
  #Add to landings matrix
  # data for other port sub-matrix
  tmp_dat <- ifelse(rownames(vesselmat_diff) %in% op_boats, 1,0)
  # create sub-matrix for other port column
  tmp_mat <- as.matrix(x=tmp_dat,nrow=dim(vesselmat_diff)[1], ncol=1); rownames(tmp_mat)=rownames(vesselmat_diff); colnames(tmp_mat) = c("other_port")
  # bind matrices
  vesselmat_diff<-cbind(vesselmat_diff,tmp_mat)
  
  if(write){
    write.csv(vesselmat_diff, paste0("output/networks/crab_vessel/",p,"_",r,"_Avesselmat_",size,"_v8_newVL.csv"))
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
  if(r=="closure"){
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
          if(length(enter_fishery) == 0 & length(remain_fishery)==0){
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
  }  else{ # if r = open
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
        ## if it didn't enter a fishery, add one to no landings or other port
        if(length(enter_fishery) == 0 & length(remain_fishery)==0){  ## UPDATED JAN 07, 2020 - prevents 1 being added to "no fishing" if vessel just remained in fisher(ies)
          ### if there were landings at another port, add 1 to "none"-->"other". otherwise, add 1 to "none"-->"no_fishing"
          if(tmpdat["other_port"]==1){
            A[j,"other_port"] <- A[j,"other_port"] + 1
            other_port_vessels <- c(other_port_vessels, rownames(vesselmat_diff)[i])
          } else{
            A[j,"no_fishing"] <- A[j,"no_fishing"] + 1
          }
        } else{
          if("DCRB_POT" %in% enter_fishery){ # record if a vessel moved into d.crab when it left another fishery
            A[j,"DCRB_POT"] <- A[j,"DCRB_POT"] + 1
          } else if(j=="DCRB_POT"){ # record if a vessel moved out of d.crab 
            for(k in enter_fishery){
              A[j,k] <- A[j,k] + 1
            } 
          }
        } #end else
      } #end for(j)
      if(self){
        for(l in remain_fishery){
          A[l,l] <- A[l,l] + 1
        }
      } #end self
    } #end for(i)
  } #end else(r==open)
  
  
  #### Get number of vessels per fishery, for later filtering ####
  vpf_2015_dropped <- c(op_count,dropped_count-op_count)
  names(vpf_2015_dropped) <- c("other_port","no_fishing")
  vpf_2015 <- c(colSums(vesselmat_2015),vpf_2015_dropped)
  vpf_2014 <- colSums(vesselmat_2014)
  
  return(list(A,n_dcrb_vessels,vpf_2014,vpf_2015))
}

vessel_adjacency_matrix_portfolios_dcrb <- function(fishtix,p,r,filter=FALSE, self=FALSE,write=FALSE,size=NA){
  #### Subset fish tickets ####
  sub_fishtix <- filter(fishtix, pcgroup == p)
  open_info <- filter(dates_df, pcgroup == p)
  odate <- open_info$odate
  ## pull fish ticket data for given year, port group, during response period
  ext_sub_fishtix <- sub_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open"))
  sub_fishtix <- ext_sub_fishtix %>%
    filter(response==r)
  ## pull fish ticket data for given year, OTHER port group, during response period
  op_fishtix <- filter(fishtix, pcgroup != p)
  op_fishtix <- op_fishtix %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response==r)
  
  
  
  #### Create Matrices: Within port ####
  #Grab Dungeness crab vessels from 2014
  if(r=="closure"){
    dcrb_vessels <- sub_fishtix %>%
      filter(crab_year==2014 & adj_revenue > 0) %>%
      filter(metier.2010=="DCRB_POT") %>%
      dplyr::select(drvid) %>%
      distinct()
  } else{
    dcrb_vessels <- ext_sub_fishtix %>%
      filter(crab_year==2014 & adj_revenue > 0) %>%
      filter(metier.2010=="DCRB_POT") %>%
      dplyr::select(drvid) %>%
      distinct()
  }
  n_dcrb_vessels <- dim(dcrb_vessels)[1]
  sub_fishtix <- filter(sub_fishtix, drvid %in% dcrb_vessels$drvid); dim(sub_fishtix)
  #2014 Matrix: 1/0 whether each vessel was in each fishery in 2014
  fishtix_2014 <- filter(sub_fishtix, crab_year==2014 & !is.na(metier.2010))
  portfolios_2014 <- fishtix_2014 %>%
    group_by(drvid) %>%
    summarise(portfolio=paste0(unique(metier.2010),collapse="-"))
  mytab_2014 <- with(portfolios_2014, table(portfolio))
  mydf_2014 <- as.data.frame(mytab_2014)
  mydf_binary_2014 <- mydf_2014 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
    arrange(drvid)
  #2015 Matrix: 1/0 whether each vessel was in each fishery in 2015
  fishtix_2015 <- filter(sub_fishtix, crab_year==2015)
  mytab_2015 <- with(fishtix_2015, table(drvid, metier.2010))
  mydf_2015 <- as.data.frame(mytab_2015)
  mydf_binary_2015 <- mydf_2015 %>%
    mutate(in_fishery = ifelse(Freq>0,1,0)) %>%
    dplyr::select(-Freq)%>%
    spread(key=metier.2010, value=in_fishery) %>%
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
  #Add in vessels that fished later in 2015 than in 2014 (open period only)
  if(r=="open"){
    dropped_vessels <- mydf_binary_2015$drvid[which(!(mydf_binary_2015$drvid %in% mydf_binary_2014$drvid))]
    if(length(dropped_vessels) > 0){
      num.metiers <- dim(mydf_binary_2014)[2] -1
      emptydf <- mydf_binary_2014[0,]
      emptydf[1:length(dropped_vessels),] <- 0
      emptydf[,1] <- dropped_vessels
      dim(emptydf)
      mydf_binary_2014 <- rbind(mydf_binary_2014, emptydf)
    }
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
  }
  
  
  #### Add external ports to difference matrix ####
  #Search for vessels at other ports during the closure in 2015
  sub_op_fishtix <- op_fishtix %>%
    filter(crab_year==2015) %>%
    filter(drvid %in% rownames(vesselmat_diff) & crab_year==2015)
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
    write.csv(vesselmat_diff, paste0("output/networks/crab_vessel/",p,"_",r,"_Avesselmat_",size,"_v7_newVL.csv"))
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
  if(r=="closure"){
    for(i in seq(1,dim(vesselmat_diff)[1])){
      tmpdat=vesselmat_diff[i,]
      tmpdat_metiers <- vesselmat_diff[i,1:n.metiers]
      # which fisheries did this vessel leave / enter?
      enter_fishery <- names(which(tmpdat_metiers == 1))
      left_fishery <- names(which(tmpdat == -1))
      if(self){
        remain_fishery <- names(which(tmpdat == 2))
      }
      # for each fishery the vessel left...
      for(j in left_fishery){
        if(j=="DCRB_POT"){
          ## if it didn't enter a fishery, add one to no landings or other port
          if(length(enter_fishery) == 0){
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
  }  else{
    for(i in seq(1,dim(vesselmat_diff)[1])){
      tmpdat=vesselmat_diff[i,]
      tmpdat_metiers <- vesselmat_diff[i,1:n.metiers]
      # which fisheries did this vessel leave / enter?
      enter_fishery <- names(which(tmpdat_metiers == 1))
      left_fishery <- names(which(tmpdat == -1))
      if(self){
        remain_fishery <- names(which(tmpdat == 2))
      }
      # for each fishery the vessel left...
      for(j in left_fishery){
        ## if it didn't enter a fishery, add one to no landings or other port
        if(length(enter_fishery) == 0){
          ### if there were landings at another port, add 1 to "none"-->"other". otherwise, add 1 to "none"-->"no_fishing"
          if(tmpdat["other_port"]==1){
            A[j,"other_port"] <- A[j,"other_port"] + 1
            other_port_vessels <- c(other_port_vessels, rownames(vesselmat_diff)[i])
          } else{
            A[j,"no_fishing"] <- A[j,"no_fishing"] + 1
          }
        } else{
          ## otherwise, add one to intersect between leave / enter
          for(k in enter_fishery){
            A[j,k] <- A[j,k] + 1
          } 
        } #end else
      } #end for(j)
      if(self){
        for(l in remain_fishery){
          A[l,l] <- A[l,l] + 1
        }
      } #end self
    } #end for(i)
  } #end else(r==open)
  
  
  #### Get number of vessels per fishery, for later filtering ####
  vpf_2015 <- colSums(vesselmat_2015)
  vpf_2014 <- colSums(vesselmat_2014)
  
  return(list(A,n_dcrb_vessels,vpf_2014,vpf_2015))
}


########### Collapse Confidential Columns for Flow Networks ###########
# will collapse pot / hook and line fishery into "other" category if either self or d.crab flow is < 3
collapse_confidential_columns_v1 <- function(A, vpf_2014,vpf_2015, no_drop=c("DCRB_POT","other_port","no_fishing")){
  # ID Confidential Data to Collapse #
  # grab the row which contains info about vessels moving out of d. crab fishery, and self-loops
  dcrb_row <- A["DCRB_POT",,drop=FALSE]
  diag_row <- matrix(diag(A),nrow=1,dimnames=list("",colnames(dcrb_row)))
  # which metiers are confidential?
  to_collapse_dcrb <- dcrb_row[,which(dcrb_row > 0 & dcrb_row < 3),drop=FALSE]
  to_collapse_self <- diag_row[,which(diag_row > 0 & diag_row < 3),drop=FALSE]
  # keep metiers from argument "no drop"
  if(any(no_drop %in% colnames(to_collapse_self))){to_collapse_self <- to_collapse_self[,-which(colnames(to_collapse_self) %in% no_drop),drop=FALSE]}
  if(any(no_drop %in% colnames(to_collapse_dcrb))){to_collapse_dcrb <- to_collapse_dcrb[,-which(colnames(to_collapse_dcrb) %in% no_drop),drop=FALSE]}
  # add back in metiers that are not confidential in EITHER dcrb flow or continued participation ("self")
  keep_dcrb <- dcrb_row[,which(dcrb_row > 2),drop=FALSE]; keep_dcrb_index <- which(colnames(to_collapse_self) %in% colnames(keep_dcrb))
  keep_self <- diag_row[,which(diag_row > 2),drop=FALSE]; keep_self_index <- which(colnames(to_collapse_dcrb) %in% colnames(keep_self))
  if(length(keep_dcrb_index)>0){to_collapse_self <- to_collapse_self[,-keep_dcrb_index,drop=FALSE]}
  if(length(keep_self_index)>0){to_collapse_dcrb<-to_collapse_dcrb[,-keep_self_index,drop=FALSE]}
  # merge confidential metiers from dcrb / self
  to_collapse <- cbind(to_collapse_dcrb,to_collapse_self)
  to_collapse <- t(rowsum(t(to_collapse), colnames(to_collapse))) #in case there are metiers repped in both diag and dcrb row
  metiers_to_collapse <- colnames(to_collapse)
  
  # Collapse Columns / Rows into "Other" #
  ##collapse part 1: by gear type. if there are multiple pot / hkl metiers with 3+ vessels combined, collapse those.
  pl_metiers_self <- unlist(lapply(colnames(to_collapse_self),function(x){grepl("POT",x) | grepl("HKL",x)}))
  pl_metiers_dcrb <- unlist(lapply(colnames(to_collapse_dcrb),function(x){grepl("POT",x) | grepl("HKL",x)}))
  if(!is.null(pl_metiers_self)){pl_collapse_self <- to_collapse_self[,which(pl_metiers_self),drop=FALSE]} else{pl_collapse_self=0}
  if(!is.null(pl_metiers_dcrb)){pl_collapse_dcrb <- to_collapse_dcrb[,which(pl_metiers_dcrb),drop=FALSE]} else{pl_collapse_self=0}
  if(((sum(pl_collapse_self) > 0 & sum(pl_collapse_self) > 2) | sum(pl_collapse_self)==0) & ((sum(pl_collapse_dcrb) > 0 & sum(pl_collapse_dcrb) > 2) | sum(pl_collapse_dcrb)==0) ){
    message("Created Other Hook & Line / Pot fishery")
    pl_metiers <- colnames(to_collapse)[which(unlist(lapply(colnames(to_collapse),function(x){grepl("POT",x) | grepl("HKL",x)})))]
    message(pl_metiers)
    pl_collapse <- to_collapse[,which(colnames(to_collapse)==pl_metiers),drop=FALSE]
    #remove these metiers from overall collapsed
    to_collapse <- to_collapse[,which(!(colnames(to_collapse)==pl_metiers)),drop=FALSE]
    #subset matrix and create "other pot" metier
    pl_data <- matrix(rowSums(A[,pl_metiers]), ncol=1,dimnames=list(rownames(A),c("OTHR_POT_HKL"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% pl_metiers))]; A <- cbind(A,pl_data) #subtract old metier info / add in new
    pl_data <- matrix(colSums(A[pl_metiers,]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR_POT_HKL"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% pl_metiers)),]; A <- rbind(A,pl_data) #subtract old metier info / add in new
  }
  ##collapse part 2: all else into an "other" category
  if(length(to_collapse) > 0){
    other_data <- matrix(rowSums(A[,colnames(to_collapse),drop=FALSE]), ncol=1,dimnames=list(rownames(A),c("OTHR"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% colnames(to_collapse)))]; A <- cbind(A,other_data) #subtract old metier info / add in new
    other_data <- matrix(colSums(A[colnames(to_collapse),,drop=FALSE]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% colnames(to_collapse))),]; A <- rbind(A,other_data) #subtract old metier info / add in new
  }

  
  # Re-calculate vessels per fishery #
  if(((sum(pl_collapse_self) > 0 & sum(pl_collapse_self) > 2) | sum(pl_collapse_self)==0) & ((sum(pl_collapse_dcrb) > 0 & sum(pl_collapse_dcrb) > 2) | sum(pl_collapse_dcrb)==0) ){
    vpf_op_2014 <- sum(vpf_2014[pl_metiers]); names(vpf_op_2014) <- "OTHR_POT_HKL"
    vpf_op_2015 <- sum(vpf_2015[pl_metiers]); names(vpf_op_2015) <- "OTHR_POT_HKL"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% pl_metiers)]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% pl_metiers)]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  if(length(to_collapse) > 0){
    vpf_op_2014 <- sum(vpf_2014[names(to_collapse)]); names(vpf_op_2014) <- "OTHR"
    vpf_op_2015 <- sum(vpf_2015[names(to_collapse)]); names(vpf_op_2015) <- "OTHR"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% colnames(to_collapse))]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% colnames(to_collapse))]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  return(list(A,vpf_2014,vpf_2015))
  
  
}

# will NOT collapse pot / hook and line fishery into "other" category if either self or d.crab flow is > 3
collapse_confidential_columns_v2 <- function(A, vpf_2014,vpf_2015, no_drop=c("DCRB_POT","other_port","no_fishing")){
  # ID Confidential Data to Collapse #
  # grab the row which contains info about vessels moving out of d. crab fishery, and self-loops
  dcrb_row <- A["DCRB_POT",,drop=FALSE]
  diag_row <- matrix(diag(A),nrow=1,dimnames=list("",colnames(dcrb_row)))
  # which metiers are confidential?
  to_collapse_dcrb <- dcrb_row[,which(dcrb_row > 0 & dcrb_row < 3),drop=FALSE]
  to_collapse_self <- diag_row[,which(diag_row > 0 & diag_row < 3),drop=FALSE]
  # keep metiers from argument "no drop"
  if(any(no_drop %in% colnames(to_collapse_self))){to_collapse_self <- to_collapse_self[,-which(colnames(to_collapse_self) %in% no_drop),drop=FALSE]}
  if(any(no_drop %in% colnames(to_collapse_dcrb))){to_collapse_dcrb <- to_collapse_dcrb[,-which(colnames(to_collapse_dcrb) %in% no_drop),drop=FALSE]}
  # add back in metiers that are not confidential in EITHER dcrb flow or continued participation ("self")
  keep_dcrb <- dcrb_row[,which(dcrb_row > 2),drop=FALSE]; keep_dcrb_index <- which(colnames(to_collapse_self) %in% colnames(keep_dcrb))
  keep_self <- diag_row[,which(diag_row > 2),drop=FALSE]; keep_self_index <- which(colnames(to_collapse_dcrb) %in% colnames(keep_self))
  if(length(keep_dcrb_index)>0){to_collapse_self <- to_collapse_self[,-keep_dcrb_index,drop=FALSE]}
  if(length(keep_self_index)>0){to_collapse_dcrb<-to_collapse_dcrb[,-keep_self_index,drop=FALSE]}
  # merge confidential metiers from dcrb / self
  to_collapse <- cbind(to_collapse_dcrb,to_collapse_self)
  to_collapse <- t(rowsum(t(to_collapse), colnames(to_collapse))) #in case there are metiers repped in both diag and dcrb row
  metiers_to_collapse <- colnames(to_collapse)
  
  # Collapse Columns / Rows into "Other" #
  ##collapse part 1: by gear type. if there are multiple pot / hkl metiers with 3+ vessels combined, collapse those.
  pl_metiers_self <- unlist(lapply(colnames(to_collapse_self),function(x){grepl("POT",x) | grepl("HKL",x)}))
  pl_metiers_dcrb <- unlist(lapply(colnames(to_collapse_dcrb),function(x){grepl("POT",x) | grepl("HKL",x)}))
  if(!is.null(pl_metiers_self)){pl_collapse_self <- to_collapse_self[,which(pl_metiers_self),drop=FALSE]} else{pl_collapse_self=0}
  if(!is.null(pl_metiers_dcrb)){pl_collapse_dcrb <- to_collapse_dcrb[,which(pl_metiers_dcrb),drop=FALSE]} else{pl_collapse_dcrb=0}
  if((sum(pl_collapse_self) > 0 & sum(pl_collapse_self) > 2) | (sum(pl_collapse_dcrb) > 0 & sum(pl_collapse_dcrb) > 2) ){
    message("Created Other Hook & Line / Pot fishery")
    pl_metiers <- colnames(to_collapse)[which(unlist(lapply(colnames(to_collapse),function(x){grepl("POT",x) | grepl("HKL",x)})))]
    message(paste0(pl_metiers,collapse=", "))
    pl_collapse <- to_collapse[,which(colnames(to_collapse) %in% pl_metiers),drop=FALSE]
    #remove these metiers from overall collapsed
    to_collapse <- to_collapse[,which(!(colnames(to_collapse) %in% pl_metiers)),drop=FALSE]
    #subset matrix and create "other pot" metier
    pl_data <- matrix(rowSums(A[,pl_metiers]), ncol=1,dimnames=list(rownames(A),c("OTHR_POT_HKL"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% pl_metiers))]; A <- cbind(A,pl_data) #subtract old metier info / add in new
    pl_data <- matrix(colSums(A[pl_metiers,]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR_POT_HKL"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% pl_metiers)),]; A <- rbind(A,pl_data) #subtract old metier info / add in new
  }
  ##collapse part 2: all else into an "other" category
  if(length(to_collapse) > 0){
    other_data <- matrix(rowSums(A[,colnames(to_collapse),drop=FALSE]), ncol=1,dimnames=list(rownames(A),c("OTHR"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% colnames(to_collapse)))]; A <- cbind(A,other_data) #subtract old metier info / add in new
    other_data <- matrix(colSums(A[colnames(to_collapse),,drop=FALSE]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% colnames(to_collapse))),]; A <- rbind(A,other_data) #subtract old metier info / add in new
  }
  
  # Re order matrix
  added_columns <- which(colnames(A) %in% c("OTHR_POT_HKL","OTHR"))
  end_columns <- which(colnames(A) %in% c("other_port","no_fishing"))
  start_columns <- which(!(seq(1,dim(A)[1]) %in% c(added_columns,end_columns)))
  new_order <- c(start_columns, added_columns, end_columns)
  A <- A[new_order,new_order]
  
  # Re-calculate vessels per fishery #
  if((sum(pl_collapse_self) > 0 & sum(pl_collapse_self) > 2) | (sum(pl_collapse_dcrb) > 0 & sum(pl_collapse_dcrb) > 2) ){
    vpf_op_2014 <- sum(vpf_2014[pl_metiers]); names(vpf_op_2014) <- "OTHR_POT_HKL"
    vpf_op_2015 <- sum(vpf_2015[pl_metiers]); names(vpf_op_2015) <- "OTHR_POT_HKL"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% pl_metiers)]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% pl_metiers)]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  if(length(to_collapse) > 0){
    vpf_op_2014 <- sum(vpf_2014[names(to_collapse)]); names(vpf_op_2014) <- "OTHR"
    vpf_op_2015 <- sum(vpf_2015[names(to_collapse)]); names(vpf_op_2015) <- "OTHR"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% colnames(to_collapse))]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% colnames(to_collapse))]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  return(list(A,vpf_2014,vpf_2015))
  
  
}

collapse_confidential_columns_v2_open <- function(A, vpf_2014,vpf_2015, no_drop=c("DCRB_POT","other_port","no_fishing")){
  # ID Confidential Data to Collapse #
  # grab the row which contains self-loops; columns where the total is smaller than 3
  diag_row <- matrix(diag(A),nrow=1,dimnames=list("",colnames(A)))
  row_totals <- matrix(rowSums(A),nrow=1,dimnames=list("",colnames(A))) 
  col_totals <- matrix(colSums(A),nrow=1,dimnames=list("",colnames(A))) 
  # which metiers are confidential?
  to_collapse_self <- diag_row[,which(diag_row > 0 & diag_row < 3),drop=FALSE]
  to_collapse_rows <- row_totals[,which(row_totals > 0 & row_totals < 3),drop=FALSE]
  # pull confidential metiers from columns only if rowsum == 0
  to_collapse_cols <- col_totals[,which(col_totals > 0 & col_totals < 3),drop=FALSE]
  rowzero <- row_totals[,which(row_totals == 0),drop=FALSE]
  to_collapse_cols <- to_collapse_cols[,which(colnames(to_collapse_cols) %in% colnames(rowzero)),drop=FALSE]
  # keep metiers from argument "no drop"
  if(any(no_drop %in% colnames(to_collapse_self))){to_collapse_self <- to_collapse_self[,-which(colnames(to_collapse_self) %in% no_drop),drop=FALSE]}
  if(any(no_drop %in% colnames(to_collapse_rows))){to_collapse_rows <- to_collapse_rows[,-which(colnames(to_collapse_rows) %in% no_drop),drop=FALSE]}
  if(any(no_drop %in% colnames(to_collapse_cols))){to_collapse_cols <- to_collapse_cols[,-which(colnames(to_collapse_cols) %in% no_drop),drop=FALSE]}
  # add back in metiers that are not confidential in a non-self category
  keep_rows <- c()
  for(metier in colnames(to_collapse_self)){
    if(any(A[,metier] > 3)){
      keep_rows <- c(keep_rows, metier)
    }
  }
  keep_rows_index <- which(colnames(to_collapse_self) %in% colnames(keep_rows))
  if(length(keep_rows_index)>0){to_collapse_self <- to_collapse_self[,-keep_rows_index,drop=FALSE]}
  # merge confidential metiers from cols / self
  metiers_to_collapse <- unique(c(colnames(to_collapse_rows),colnames(to_collapse_self),colnames(to_collapse_cols)))
  to_collapse <- A[,metiers_to_collapse,drop=FALSE]
  
  
  # Collapse Columns / Rows into "Other" #
  ##collapse part 1: by gear type. if there are multiple pot / hkl metiers with 3+ vessels combined, collapse those.
  pl_metiers <- unlist(lapply(colnames(to_collapse),function(x){grepl("POT",x) | grepl("HKL",x)}))
  if(!is.null(pl_metiers)){
    pl_collapse <- to_collapse[,which(pl_metiers),drop=FALSE]
    pl_diag <- pl_collapse[which(rownames(to_collapse)%in%colnames(pl_collapse)),]
    } else{pl_collapse=0; pl_diag=0}
  if(any(rowSums(pl_collapse) > 2) | sum(pl_diag) > 2){
    pl_metiers <- colnames(to_collapse)[which(unlist(lapply(colnames(to_collapse),function(x){grepl("POT",x) | grepl("HKL",x)})))]
    message("Created Other Hook & Line / Pot fishery: ", paste0(pl_metiers,collapse=", "))
    #remove these metiers from overall collapsed
    to_collapse <- to_collapse[,which(!(colnames(to_collapse) %in% pl_metiers)),drop=FALSE]
    #subset matrix and create "other pot" metier
    pl_data <- matrix(rowSums(pl_collapse), ncol=1,dimnames=list(rownames(A),c("OTHR_POT_HKL"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% pl_metiers))]; A <- cbind(A,pl_data) #subtract old metier info / add in new
    pl_data <- matrix(colSums(A[pl_metiers,]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR_POT_HKL"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% pl_metiers)),]; A <- rbind(A,pl_data) #subtract old metier info / add in new
  }
  ##collapse part 2: all else into an "other" category
  if(length(to_collapse) > 0){
    other_data <- matrix(rowSums(A[,colnames(to_collapse),drop=FALSE]), ncol=1,dimnames=list(rownames(A),c("OTHR"))) #first, take care of columns
    A <- A[,-c(which(colnames(A) %in% colnames(to_collapse)))]; A <- cbind(A,other_data) #subtract old metier info / add in new
    other_data <- matrix(colSums(A[colnames(to_collapse),,drop=FALSE]),nrow=1,ncol=dim(A)[2],dimnames=list(c("OTHR"), colnames(A)))
    A <- A[-c(which(rownames(A) %in% colnames(to_collapse))),]; A <- rbind(A,other_data) #subtract old metier info / add in new
  }
  
  # Re order matrix
  added_columns <- which(colnames(A) %in% c("OTHR_POT_HKL","OTHR"))
  end_columns <- which(colnames(A) %in% c("other_port","no_fishing"))
  start_columns <- which(!(seq(1,dim(A)[1]) %in% c(added_columns,end_columns)))
  new_order <- c(start_columns, added_columns, end_columns)
  A <- A[new_order,new_order]
  
  # Re-calculate vessels per fishery #
  if(any(rowSums(pl_collapse) > 2) | sum(pl_diag) > 2){
    vpf_op_2014 <- sum(vpf_2014[pl_metiers]); names(vpf_op_2014) <- "OTHR_POT_HKL"
    vpf_op_2015 <- sum(vpf_2015[pl_metiers]); names(vpf_op_2015) <- "OTHR_POT_HKL"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% pl_metiers)]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% pl_metiers)]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  if(length(to_collapse) > 0){
    vpf_op_2014 <- sum(vpf_2014[names(to_collapse)]); names(vpf_op_2014) <- "OTHR"
    vpf_op_2015 <- sum(vpf_2015[names(to_collapse)]); names(vpf_op_2015) <- "OTHR"
    vpf_2014 <- vpf_2014[-which(names(vpf_2014) %in% colnames(to_collapse))]; vpf_2014 <- c(vpf_2014,vpf_op_2014)
    vpf_2015 <- vpf_2015[-which(names(vpf_2015) %in% colnames(to_collapse))]; vpf_2015 <- c(vpf_2015,vpf_op_2015)
  }
  return(list(A,vpf_2014,vpf_2015))
  
  
}



### plotting with ggraph: distance from end of directed arrow to node center
end_dist <- function(sub_g){
  end_dist=c()
  for(i in seq(1,length(E(sub_g)))){
    tmp_end = ends(sub_g,es=i)[1,2]
    w=V(sub_g)$size[which(V(sub_g)$name==tmp_end)]
    if(w <= 15){
      end_dist <- c(end_dist,3)
    } else if(w <= 20){
      end_dist <- c(end_dist,5)
    }else if(w <= 100){
      end_dist <- c(end_dist,10)
    } else{
      end_dist <- c(end_dist,15)
    }
  }
  return(end_dist)
}

end_dist_vl <- function(sub_g,mini=FALSE){
  end_dist=c()
  if(mini){
    for(i in seq(1,length(E(sub_g)))){
      tmp_end = ends(sub_g,es=i)[1,2]
      print(tmp_end)
      w=V(sub_g)$size[which(V(sub_g)$name==tmp_end)]
      if(w <= 3){
        end_dist <- c(end_dist,1)
      } else if(w <=5){
        end_dist <- c(end_dist,3)
      } else if(w <= 10){
        end_dist <- c(end_dist,7)
      }else if(w <= 100){
        end_dist <- c(end_dist,10)
      } else{
        end_dist <- c(end_dist,12)
      }
    }
  } else{
    for(i in seq(1,length(E(sub_g)))){
      tmp_end = ends(sub_g,es=i)[1,2]
      print(tmp_end)
      w=V(sub_g)$size[which(V(sub_g)$name==tmp_end)]
      if(w <= 3){
        end_dist <- c(end_dist,3)
      } else if(w <=5){
        end_dist <- c(end_dist,7)
      } else if(w <= 10){
        end_dist <- c(end_dist,10)
      }else if(w <= 100){
        end_dist <- c(end_dist,15)
      } else{
        end_dist <- c(end_dist,15)
      }
    }
  }
  return(end_dist)
}

### plotting with ggraph: edge color
edge_col <- function(sub_g){
  col_list=c()
  for(i in seq(1,length(E(sub_g)))){
    tmp_end = ends(sub_g,es=i)[1,2]
    if(tmp_end == "other_port" | tmp_end == "no_fishing"){
      col_list[i] <- "wheat2"
    } else{
      col_list[i] <- "gray"
    }
  }
  return(col_list)
}




vertex_color <- function(mygraph,none=TRUE){
  vertex_cols <- c()
  if(none){  
    for(i in seq(1, vcount(mygraph))){
    tmp_node <- V(mygraph)$name[i]
    if(grepl("HKL",tmp_node) == TRUE){
      tmp_col = "darkgoldenrod1"
    } else if(grepl("NET",tmp_node) == TRUE){
      tmp_col = "cyan4"
    } else if(grepl("TLS",tmp_node) == TRUE){
      tmp_col = "chartreuse3"
    } else if(grepl("TWL",tmp_node) == TRUE){
      tmp_col = "chocolate4"
    } else if(grepl("TWS",tmp_node) == TRUE){
      tmp_col = "hotpink"
    } else if(grepl("POT",tmp_node) == TRUE){
      tmp_col = "darkorange2"
    } else{
      if(tmp_node=="other_port" | tmp_node=="no_fishing"){
        tmp_col="wheat2"
      } else if(tmp_node=="none"){
        tmp_col="gray"
      } else{
        tmp_col = "plum3"
      }
    }
    vertex_cols[i] <- tmp_col
  }
  } else{
    for(i in seq(1, vcount(mygraph))){
      tmp_node <- V(mygraph)$name[i]
      if(grepl("HKL",tmp_node) == TRUE){
        tmp_col = "darkgoldenrod1"
      } else if(grepl("NET",tmp_node) == TRUE){
        tmp_col = "cyan4"
      } else if(grepl("TLS",tmp_node) == TRUE){
        tmp_col = "chartreuse3"
      } else if(grepl("TWL",tmp_node) == TRUE){
        tmp_col = "chocolate4"
      } else if(grepl("TWS",tmp_node) == TRUE){
        tmp_col = "hotpink"
      } else if(grepl("POT",tmp_node) == TRUE){
        tmp_col = "darkorange2"
      } else{
        if(tmp_node=="other_port" | tmp_node=="no_fishing"){
          tmp_col="gray"
        } else{
          tmp_col = "plum3"
        }
      }
      vertex_cols[i] <- tmp_col
    }
  }
  return(vertex_cols)
}

vertex_color_collapsed <- function(mygraph){
  vertex_cols <- c()
  for(i in seq(1, vcount(mygraph))){
      tmp_node <- V(mygraph)$name[i]
      if(grepl("HKL",tmp_node) == TRUE & grepl("POT",tmp_node) == TRUE){
        tmp_col = "tan1"
      } else if(grepl("HKL",tmp_node) == TRUE){
        tmp_col = "darkgoldenrod1"
      } else if(grepl("NET",tmp_node) == TRUE){
        tmp_col = "cyan4"
      } else if(grepl("TLS",tmp_node) == TRUE){
        tmp_col = "chartreuse3"
      } else if(grepl("TWL",tmp_node) == TRUE){
        tmp_col = "chocolate4"
      } else if(grepl("TWS",tmp_node) == TRUE){
        tmp_col = "hotpink"
      } else if(grepl("POT",tmp_node) == TRUE){
        tmp_col = "darkorange2"
      } else{
        if(tmp_node=="other_port" | tmp_node=="no_fishing"){
          tmp_col="gray"
        } else{
          tmp_col = "plum3"
        }
      }
      vertex_cols[i] <- tmp_col
    }
  return(vertex_cols)
}



