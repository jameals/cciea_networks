############ Summarize crab fishers' participation ############
# 
# drawing on the annual network objects, identify key fisheries
#   for crab fishers in WA / OR / N. CA / C. CA
#   M. Fisher 07/14/2021
#
###############################################################


# set-up ------------------------------------------------------------------
# libraries
library(tidyverse)
library(here)

# what is the ID number of the crab fishery?
crab = 25 

# function to identify fisheries connected to crab in the network
id_fisheries <- function(g, crab_id = 25){
  # add in row IDs
  rownames(g) <- colnames(g)
  
  # grab only crab column
  tmpg <- g %>% dplyr::select(paste0("X",crab_id))
  colnames(tmpg) <- "crab_fishery"
  
  # get nonzero row names
  fished <- rownames(tmpg %>% filter(crab_fishery > 0))
  
  # return non-crab fishery IDs
  fished <- as.vector(sapply(fished, str_remove, pattern="X"))
  return(fished[which(fished != as.character(crab_id))])
  
}


# read in data ------------------------------------------------------------
# read fishery participation information from the network objects of interest (as .csv files) into a list

pgs   <- c("Crescent City", "Eureka", "Fort Bragg")       # pick port groups
pgs   <- c("North WA Coast", "WA Coast")
yrs   <- seq(2010,2019)          # pick years


Alist <- list()
i=1
for(p in pgs){
  for(y in yrs){
    tmp <- read.csv(here::here("data", "networks", "participation_bulk", paste0("A_",p,"_",y,"_5.csv")),
                    header=TRUE)
    Alist[[i]] <- tmp
    i=i+1
  }
}




# get cross-fishery participation -----------------------------------------

# first, get a vector of all of the fisheries in which crab fishermen participated
other_fished <- c()

for(i in seq(1, length(Alist))){
  tmpg <- Alist[[i]]
  
  # get the IDs for fisheries in which crab fishermen participated for the given year / port
  f    <- id_fisheries(g=tmpg, crab_id=crab)
  
  # if those fisheries are not already in the vector, save them
  new_fished <- f[which(!(f %in% other_fished))]
  if(length(new_fished) > 0){
    other_fished <- c(other_fished, new_fished)
  }
  
}

other_fished
other_fished_cols <- paste("X",other_fished,sep="")


# then, for each fishery, get cumulative importance (from edge weights) and 
#   frequency (the proportion of networks in which the edge is present) 

fished_dat <- matrix(data=0, nrow=length(other_fished),ncol=1,dimnames=list(other_fished, c("cumuI")))

for(i in seq(1, length(Alist))){
  tmpg <- Alist[[i]]; rownames(tmpg) <- colnames(tmpg)
  # subset the data frame
  sub_other_fished_cols <- other_fished_cols[which(other_fished_cols %in% rownames(tmpg))]
  tmpI <- data.frame(fishery_id = str_remove(rownames(tmpg[sub_other_fished_cols,]),"X"),
                     tmpI = tmpg[sub_other_fished_cols,paste0("X",crab)])
  for(id in tmpI$fishery_id){
    fished_dat[id,] <- fished_dat[id,] + filter(tmpI, fishery_id==id)$tmpI
  }
}

# match to fishery names
id_key <- read.csv(here::here("data", "input", "spgrpn2_iea_key.csv")) %>%
  mutate(SPGRPN2=as.character(SPGRPN2))
fished_df <- as.data.frame(x=fished_dat) %>%
  mutate(ids=rownames(fished_dat))

fished_df <-left_join(fished_df, id_key, by=c("ids"="SPGRPN2"))

  

# Evaluate cross-fishery participation ------------------------------------

ggplot(fished_df, aes(x=species_group_2_label2, y=cumuI)) +
  geom_col() +
  xlab("fishery") + ylab("cumulative importance") +
  theme_bw()
  
  
  
  








