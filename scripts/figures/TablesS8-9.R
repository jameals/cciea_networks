########## Create summary matrix of fishery participation ##########
############## Tables S8 and S9 ####################################
#
# Jan 13, 2020 - M. Fisher
#
######################################################




# Packages ----------------------------------------------------------------
library(lubridate)
library(tidyverse)
library(dplyr)
library(grid)
library(gridExtra)
library(here)



# User Inputs -------------------------------------------------------------
myports=c("CCA","ERA","BGA","BDA","SFA","MNA","MRA")



# Functions ----------------------------------------------------------------

source(here::here("R","calc_mode.R"))
source(here::here("R","collapse_confidential_matrix_rows.R"))

create_pairwise_matrix <- function(mat, start=3){
  # get names of fisheries
  fisheries <- colnames(mat)[start:dim(mat)[2]]
  # blank matrix
  A <- matrix(nrow=length(fisheries),ncol=length(fisheries),dimnames=list(fisheries,fisheries),data=0)
  # for each row in the matrix...
  for(v in seq(1,dim(mat)[1])){
    ## grab that row (a given vessel) and remove id variables
    tmp<- mat[v,start:dim(mat)[2]]
    ## get the fisheries the given vessel was participating in
    tmpdat_metiers <- colnames(tmp)[which(tmp>0)]
    nmetiers <- length(tmpdat_metiers)
    if(nmetiers > 0){
    # for each metier, add to the diagonal
    # for each pair of metiers, add to the cell
    for(i in seq(1,nmetiers)){
      m=tmpdat_metiers[i]
      A[m,m] <- A[m,m] + 1
      if(i < nmetiers){
        for(j in seq(i+1,nmetiers)){
          m2 <- tmpdat_metiers[j]
          A[m,m2] <- A[m,m2] + 1
        } # for(j)
      } # if(i)
    } # for(i)
    } # if(nmetiers)
  } # for(v)
  return(A)
}


# Early Season ------------------------------------------------------------

## read in the vessel matrices
r="closure"
for(p in myports){
  tmpdat <- read.csv(here::here("data/networks/vessel_flow",paste0(p,"_closure_Avesselmat_NA_v7_newVL.csv"))) %>%
    rename(drvid=X) %>%
    pivot_longer(-drvid, names_to="metier.2010", values_to="pcode") %>%
    filter(pcode > 0 & metier.2010 != "other_port") %>%
    mutate(pcode=ifelse(pcode==1,pcode,1),
           pcgroup=p)
  if(p==myports[1]){
    mydat=tmpdat
  }else{
    mydat <- rbind(mydat,tmpdat)
  }
}

## use the function to produce confidential cross-fishery participation matrices
for(p in myports){
  tmpmat <- mydat %>%
    filter(pcgroup==p) %>%
    pivot_wider(names_from=metier.2010,values_from=pcode)
  mymat <- create_pairwise_matrix(tmpmat)
  write.csv(mymat,here::here("output","networks","participation",paste0("crab_vessel_",p,"_",r,"_participation_matrix.csv")),row.names=TRUE)
}


## list out the fisheries to be collapsed
collapsed <- data.frame(p="CCA",
                        new="OTHR_POT_HKL",
                        original="LCOD_POT,OSRM_POT,SABL_HKL,SRFP_HKL") %>%
  rbind(data.frame(p="BGA",
                   new="OTHR",
                   original="BLGL_HKL,CBZN_HKL,CHNK_TLS,SABL_TWL")) %>%
  rbind(data.frame(p="SFA",
                   new="OTHR_POT_HKL",
                   original="BRWN_HKL,CHLB_HKL,SPRW_POT")) %>%
  rbind(data.frame(p="SFA",
                   new="OTHR",
                   original="PHRG_NET,PSDN_NET")) %>%
  rbind(data.frame(p="MNA",
                   new="OTHR_POT_HKL",
                   original="BYEL_HKL,CMCK_HKL,RCRB_POT,SPRW_POT")) %>%
  rbind(data.frame(p="MRA",
                   new="OTHR_POT_HKL",
                   original="BRWN_HKL,VRML_HKL")) %>%
  separate(original,into=c("m1","m2","m3","m4","m5"),sep=",") %>%
  pivot_longer(cols=c(m1,m2,m3,m4,m5)) %>%
  mutate(pcgroup=as.character(p)) %>%
  filter(!is.na(value)) %>%
  dplyr::select(-name,-p) 

## edit the confidential matrices using the key above
for(p in myports){
  to_collapse <- filter(collapsed,pcgroup==p)
  
  ## if there are metiers to collapse... ##
  if(dim(to_collapse)[1] > 0){
    
    ## read in the vessel matrix
    vessel_matrix <- read.csv(here::here("data/networks/vessel_flow",paste0(p,"_closure_Avesselmat_NA_v7_newVL.csv")), row.names=1)
    
    ## grab the nonconfidential metiers
    vesselmat_noncon <- vessel_matrix %>% dplyr::select(-all_of(to_collapse$value))
    
    
    for(i in seq(1,length(unique(to_collapse$new)))){
      
      ## get the metiers to collapse into new metier `i`
      tmp_collapse <- to_collapse %>% filter(new==unique(to_collapse$new)[i])
      
      
      ## grab the confidential metiers
      vesselmat_con <- vessel_matrix %>% dplyr::select(all_of(tmp_collapse$value))
      
      ## use the collapse_rowwise function to get one value per vessel for all combined metiers
      collapsed_values <- unlist(apply(vesselmat_con[,all_of(tmp_collapse$value)], 1, collapse_rowwise, 2))
      
      ## add the new non-confidential metier to the non-confidential vessel matrix
      if(as.character(unique(to_collapse$new)[i]) == "OTHR_POT_HKL"){
        vesselmat_noncon <- vesselmat_noncon %>% bind_cols("OTHR_POT_HKL" = collapsed_values)
      } else if(as.character(unique(to_collapse$new)[i]) == "OTHR"){
        vesselmat_noncon <- vesselmat_noncon %>% bind_cols("OTHR" = collapsed_values)
      } else{
        stop("ERROR: don't know what to call the new metier.")
      }
      
    } # end(i in seq(1,...))
    
    # feed the new vessel matrix into the function to create the cross-fishery participation matrix
    tmpmat2 <- vesselmat_noncon %>%
      mutate(drvid=rownames(vesselmat_noncon)) %>%
      dplyr::select(drvid, all_of(colnames(vesselmat_noncon))) %>%
      dplyr::select(-other_port)
    mymat.noncon <- create_pairwise_matrix(tmpmat2, start=2)
    
    # get rid of remaining values below 1
    mymat.noncon <- as.matrix(mymat.noncon); mymat.noncon[mymat.noncon < 3] <- 0
    # get rid of columns / rows that only have zeros
    mymat.noncon <- mymat.noncon[which(rowSums(mymat.noncon)>0),which(colSums(mymat.noncon)>0)]
    
    #### write out the new file ####
    write.csv(mymat.noncon,here::here("output","networks","participation",paste0("crab_vessel_",p,"_",r,"_participation_matrix_noncon.csv")),row.names=TRUE)
    
  } # end(dim > 0)
} # end(p in myports)





# Late Season -------------------------------------------------------------
## read in the vessel matrices
r="open"
for(p in myports){
  tmpdat <- read.csv(here::here(adjdir,"output/networks/crab_vessel",paste0(p,"_open_Avesselmat_NA_v8_newVL.csv"))) %>%
    rename(drvid=X) %>%
    pivot_longer(-drvid, names_to="metier.2010", values_to="pcode") %>%
    filter(pcode > 0 & metier.2010 != "other_port") %>%
    mutate(pcode=ifelse(pcode==1,pcode,1),
           pcgroup=p)
  if(p==myports[1]){
    mydat=tmpdat
  }else{
    mydat <- rbind(mydat,tmpdat)
  }
}

## use the function to produce confidential cross-fishery participation matrices
for(p in myports){
  tmpmat <- mydat %>%
    filter(pcgroup==p) %>%
    pivot_wider(names_from=metier.2010,values_from=pcode)
  mymat <- create_pairwise_matrix(tmpmat)
  write.csv(mymat,here::here("output","networks","participation",paste0("crab_vessel_",p,"_",r,"_participation_matrix.csv")),row.names=TRUE)
}



## list out the fisheries to be collapsed
collapsed <- data.frame(p="CCA",
                        new="OTHR_POT_HKL",
                        original="ALBC_HKL,TSRK_HKL,SABL_HKL") %>%
  rbind(data.frame(p="CCA",
                   new="OTHR",
                   original="CHNK_TLS,SABL_TWL_DOVR_TWL")) %>%
  rbind(data.frame(p="ERA",
                   new="OTHR_POT_HKL",
                   original="SABL_POT,UHAG_POT")) %>%
  rbind(data.frame(p="BDA",
                   new="OTHR_POT_HKL",
                   original="UHAG_POT,ALBC_HKL,SABL_POT,GPHR_HKL,WCRK_HKL")) %>%
  rbind(data.frame(p="BDA",
                   new="OTHR",
                   original="PSHP_TWS,ALBC_TLS,RURC_MSC,USCU_MSC")) %>%
  rbind(data.frame(p="SFA",
                   new="OTHR_POT_HKL",
                   original="OSRM_POT,DCRB_HKL,WBAS_HKL,SPRW_POT")) %>%
  rbind(data.frame(p="SFA",
                   new="OTHR",
                   original="MSQD_NET,NANC_NET,PTRL_TWL,CHLB_TLS,JMCK_NET,PSDN_NET")) %>%
  rbind(data.frame(p="MNA",
                   new="OTHR_POT_HKL",
                   original="BYEL_HKL,RCRB_POT,SABL_HKL,SPRW_POT,WBAS_HKL,SABL_POT,ALBC_HKL")) %>%
  rbind(data.frame(p="MRA",
                   new="OTHR_POT_HKL",
                   original="BRWN_HKL,CHLB_HKL,LCOD_HKL,RCRB_POT,SABL_POT,UHAG_POT")) %>%
  separate(original,into=c("m1","m2","m3","m4","m5","m6","m7","m8","m9"),sep=",") %>%
  pivot_longer(cols=c(m1,m2,m3,m4,m5,m6,m7,m8,m9)) %>%
  mutate(pcgroup=as.character(p)) %>%
  filter(!is.na(value)) %>%
  dplyr::select(-name,-p) 

## edit the confidential matrices using the key above
for(p in myports){
  to_collapse <- filter(collapsed,pcgroup==p)
  
  ## if there are metiers to collapse... ##
  if(dim(to_collapse)[1] > 0){
    
    ## read in the vessel matrix
    vessel_matrix <- read.csv(here::here("output/networks/vessel_flow",paste0(p,"_",r,"_Avesselmat_NA_v8_newVL.csv")), row.names=1)
    
    ## grab the nonconfidential metiers
    vesselmat_noncon <- vessel_matrix %>% dplyr::select(-all_of(to_collapse$value))
    
    
    for(i in seq(1,length(unique(to_collapse$new)))){
      
      ## get the metiers to collapse into new metier `i`
      tmp_collapse <- to_collapse %>% filter(new==unique(to_collapse$new)[i])
      
      
      ## grab the confidential metiers
      vesselmat_con <- vessel_matrix %>% dplyr::select(all_of(tmp_collapse$value))
      
      ## use the collapse_rowwise function to get one value per vessel for all combined metiers
      collapsed_values <- unlist(apply(vesselmat_con[,all_of(tmp_collapse$value)], 1, collapse_rowwise, 2))
      
      ## add the new non-confidential metier to the non-confidential vessel matrix
      if(as.character(unique(to_collapse$new)[i]) == "OTHR_POT_HKL"){
        vesselmat_noncon <- vesselmat_noncon %>% bind_cols("OTHR_POT_HKL" = collapsed_values)
      } else if(as.character(unique(to_collapse$new)[i]) == "OTHR"){
        vesselmat_noncon <- vesselmat_noncon %>% bind_cols("OTHR" = collapsed_values)
      } else{
        stop("ERROR: don't know what to call the new metier.")
      }
      
    } # end(i in seq(1,...))
    
    # feed the new vessel matrix into the function to create the cross-fishery participation matrix
    tmpmat2 <- vesselmat_noncon %>%
      mutate(drvid=rownames(vesselmat_noncon)) %>%
      dplyr::select(drvid, all_of(colnames(vesselmat_noncon))) %>%
      dplyr::select(-other_port)
    mymat.noncon <- create_pairwise_matrix(tmpmat2, start=2)
    
    # get rid of remaining values below 1
    mymat.noncon <- as.matrix(mymat.noncon); mymat.noncon[mymat.noncon < 3] <- 0
    # get rid of columns / rows that only have zeros
    mymat.noncon <- mymat.noncon[which(rowSums(mymat.noncon)>0),which(colSums(mymat.noncon)>0)]
    
    #### write out the new file ####
    write.csv(mymat.noncon,here::here("output","networks","participation",paste0("crab_vessel_",p,"_",r,"_participation_matrix_noncon.csv")),row.names=TRUE)
    
  } # end(dim > 0)
} # end(p in myports)


















