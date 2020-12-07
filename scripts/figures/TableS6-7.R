################## Stargazer Model Summaries ##################
#
# Tables S6 and S7
# M. Fisher
#
###############################################################


# Set up ---------------------------------------------------------------

library(stargazer)
library(lmtest)
library(sandwich)
library(tidyverse)
library(here)



# Data --------------------------------------------------------------------

# Network stats
data <- read.csv(here::here('results/statistics','2008_2017_CA_ParticipationNetworkMetrics.csv'))
colnames(data)

## select only statistics that were run through the GLMs in script 08
data <- data %>% dplyr::select(y, nc, nc_weighted, ed, mean_deg,m, m_weighted, N, period, pcgroup) %>%
  mutate(R = ifelse(pcgroup %in% c("CCA", "ERA", "BGA","BDA"), "North", "Central"))
data$R <- factor(data$R, levels=c("North","Central"))

## replace `0` mean degree values with a number very close to `0`
data <- mutate(data, mean_deg=ifelse(mean_deg > 0, mean_deg, 0.000001))


# Closure Duration
closure_data <- read.csv(here::here('data/input','DCRB_Historic_Closures_CA_updated.csv'))
## classify closure durations
closure_data <- closure_data %>%
  mutate(D = ifelse(days.closed == 0, "none", ifelse(days.closed <= 14, "low",
                                                     ifelse(days.closed < 50, "medium", "high"))))

# Combine Network and Closure Data
data <- left_join(data, closure_data, by=c("y", "pcgroup"))
data$D <- as.factor(data$D)
data$D <- factor(data$D, levels=c("none", "low", "medium", "high"))


# Split into Early and Late Season data frames
edata <- filter(data, period == "early") # early season
ldata <- filter(data, period == "late") # late season




# Models ------------------------------------------------------------------



# Edge Density, with Node predictor
eN.e <- glm(ed ~ D*R + N + pcgroup, data = edata, family = quasibinomial('logit'))
robust.se.eN.e <- sqrt(diag(vcovHC(eN.e , type="HC0")))
coeftest(eN.e, vcovHC(eN.e, type="HC0"))

eN.l <- glm(ed ~ D*R + N + pcgroup, data = ldata, family = quasibinomial('logit'))
robust.se.eN.l <- sqrt(diag(vcovHC(eN.l , type="HC0")))
coeftest(eN.l, vcovHC(eN.l, type="HC0"))


# NC weighted, with Node predictor
ncWN.e <- glm(nc_weighted ~ D*R+N, data = edata, family = quasibinomial('logit'))
robust.se.ncWN.e <- sqrt(diag(vcovHC(ncWN.e , type="HC0")))
coeftest(ncWN.e, vcovHC(ncWN.e, type="HC0"))

ncWN.l <- glm(nc_weighted ~ D*R+N+pcgroup, data = ldata, family = quasibinomial('logit'))
robust.se.ncWN.l <- sqrt(diag(vcovHC(ncWN.l , type="HC0")))
coeftest(ncWN.l, vcovHC(ncWN.l, type="HC0"))


# M weighted, with Node predictor
mWN.e <- glm(m_weighted ~ D*R + N, data = edata, family = gaussian('identity'))
robust.se.mWN.e <- sqrt(diag(vcovHC(mWN.e , type="HC0")))
coeftest(mWN.e, vcovHC(mWN.e, type="HC0"))

mWN.l <- glm(m_weighted ~ D*R + N + pcgroup, data = ldata, family = gaussian('identity'))
robust.se.mWN.l <- sqrt(diag(vcovHC(mWN.l , type="HC0")))
coeftest(mWN.l, vcovHC(mWN.l, type="HC0"))



#---

# NC
nc.e <- glm(nc ~ D*R, data = edata, family = quasibinomial('logit'))
robust.se.nc.e <- sqrt(diag(vcovHC(nc.e , type="HC0")))
coeftest(nc.e, vcovHC(nc.e, type="HC0"))

nc.l <- glm(nc ~ D*R, data = ldata, family = quasibinomial('logit'))
robust.se.nc.l <- sqrt(diag(vcovHC(nc.l , type="HC0")))
coeftest(nc.l, vcovHC(nc.l, type="HC0"))


# Mean degree, with Node predictor
deg.e <- glm(mean_deg ~ D*R + N + pcgroup, data = edata, family = Gamma('inverse'))
robust.se.deg.e <- sqrt(diag(vcovHC(deg.e , type="HC0")))
coeftest(deg.e, vcovHC(deg.e, type="HC0"))

deg.l <-  glm(mean_deg ~ D*R + N + pcgroup, data = ldata, family = Gamma('inverse'))
robust.se.deg.l <- sqrt(diag(vcovHC(deg.l , type="HC0")))
coeftest(deg.l, vcovHC(deg.l, type="HC0"))


# Modularity, with Node predictor
mN.e <- glm(m ~ D*R + N + pcgroup, data = edata, family = gaussian('identity'))
robust.se.mN.e <- sqrt(diag(vcovHC(mN.e , type="HC0")))
coeftest(mN.e, vcovHC(mN.e, type="HC0"))

mN.l <- glm(m ~ D*R + N + pcgroup, data = ldata, family = gaussian('identity'))
robust.se.mN.l <- sqrt(diag(vcovHC(mN.l , type="HC0")))
coeftest(mN.l, vcovHC(mN.l, type="HC0"))






# Stargazer, Main Paper ---------------------------------------------------------------

gaze.coeft <- function(x, col="Pr(>|z|)"){
  stopifnot(is.list(x))
  x <- lapply(x,function(y){
    coeftest(y, vcovHC(y, type="HC0"))})
  out <- lapply(x, function(y){
    y[ , col]
  })
  return(out)
}




# model output in the supplement -------------------------------------------------


# closure / early season
pvals.e=gaze.coeft(list(eN.e,  ncWN.e, mWN.e,deg.e, nc.e, mN.e))
stargazer(eN.e, ncWN.e, mWN.e,deg.e, nc.e, mN.e, 
          se=list(robust.se.eN.e,robust.se.ncWN.e,robust.se.mWN.e,robust.se.deg.e,robust.se.nc.e,robust.se.mN.e),
          p=pvals.e,
          digits=2,
          align=TRUE, type="html", model.numbers=FALSE,
          column.labels=c("D*R+P+N","D*R+N","D*R+N","D*R+P+N","D*R","D*R+P+N"),
          dep.var.labels=c("Edge Density", "Centralization","Modularity","Mean Degree","Centralization (UnW)","Modularity (UnW)"),
          dep.var.caption="",
          omit=c("pcgroupSFA","Dmedium:RCentral"),
          order=c(2,1,12,11,3,4,6,7,5,8,9,10,13),
          covariate.labels=c("Duration (high)","Duration (med.)","Duration (high) : Region (C)",
                             "Region (Central)","Node Count",
                             "Port Group (Crescent City)","Port Group (Eureka)","Port Group (Fort Bragg)","Port Group (Monterey)","Port Group (Morro Bay)",
                             "Intercept"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat=c("aic","ll"),
          model.names=FALSE,
          out=paste0(here::here(),"/results/statistics/2008_2017_CA_glm_early.html"))

pvals.l=gaze.coeft(list(eN.l, ncWN.l, mWN.l,deg.l, nc.l, mN.l))
stargazer(eN.l, ncWN.l, mWN.l, deg.l, nc.l, mN.l,
          se=list(robust.se.eN.l,robust.se.ncWN.l,robust.se.mWN.l,robust.se.deg.l,robust.se.nc.l,robust.se.mN.l),
          p=pvals.l,
          digits=2,
          align=TRUE, type="html", model.numbers=FALSE,
          column.labels=c("D*R+P+N","D*R+P+N","D*R+P+N","D*R+P+N","D*R","D*R+P+N"),
          dep.var.labels=c("Edge Density", "Centralization","Modularity","Mean Degree","Centralization (UnW)","Modularity (UnW)"),
          dep.var.caption="",
          omit=c("pcgroupSFA","Dmedium:RCentral"),
          order=c(2,1,12,11,3,4,6,7,5,8,9,10,13),
          covariate.labels=c("Duration (high)","Duration (med.)","Duration (high) : Region (C)",
                             "Region (Central)","Node Count",
                             "Port Group (Crescent City)","Port Group (Eureka)","Port Group (Fort Bragg)","Port Group (Monterey)","Port Group (Morro Bay)",
                             "Intercept"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat=c("aic","ll"),
          model.names=FALSE,
          out=paste0(here::here(),"/results/statistics/2008_2017_CA_glm_late.html"))







