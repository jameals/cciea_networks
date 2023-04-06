if(!require("tidyverse")) {install.packages("tidyverse")}
if(!require("cowplot")) {install.packages("cowplot")}
if(!require("ggrepel")) {install.packages("ggrepel")}
if(!require("PNWColors")) {devtools::install_github("jakelawler/PNWColors"); library(PNWColors)}

## directory to save figures
pngdir <- 'results/figures/gc5_ns8'

# directory to read in df
data_out_dir <- 'data/gc5_ns8'

# read in data file
plotdat_wide <- read_csv(here::here(data_out_dir,'plotdat_wide_vuln.csv'))

# create color palette
starfishpal=pnw_palette("Starfish",19)

## Edge density vs Revenue with CSVI

# main network metric plot - Salmon
fig_ma_salmon <- ggplot(plotdat_wide %>% filter(metier.cm == 'Salmon'), aes(x=(`5-Year Mean Edge Density`), y=`5-Year Mean Revenue`/10^6)) + 
  geom_point(aes(fill = CSVI, colour = CSVI), size=4) + 
  geom_text_repel(aes(label=pcgroup), #, size = 2*`Scaled Revenue` #, size = 2*(`Long-term Mean Importance`)
                  force = 0.5,
                  #nudge_x = 0.1,
                  direction = "y",
                  hjust = 1,
                  segment.size = 0.2,
                  max.overlaps = 15) +
  geom_hline(
    yintercept=
      mean(
        plotdat_wide %>% 
          filter(metier.cm  == 'Salmon') %>% 
          pull(`5-Year Mean Revenue`)/10^6,
        na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  geom_vline(
    xintercept=
      mean(plotdat_wide %>% 
             filter(metier.cm  == 'Salmon') %>% 
             pull(`5-Year Mean Edge Density`),
           na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  xlim(0,1) +
  scale_colour_gradientn(colours = starfishpal) +
  scale_fill_gradientn(colours = starfishpal) +
  labs(#subtitle = '2020-2021',
    x = "Low                                    Resilience Index                                 High\n(2016-2021)",
    y = "Economic Dependence on Salmon\n(2016-2021)"#,
    #size = 'Long-term Mean\nImportance of Salmon\nNode in Network'
  ) +
  theme_classic() 
fig_ma_salmon


# main network metric plot - Whiting
fig_ma_whiting <- ggplot(plotdat_wide %>% filter(metier.cm == 'Whiting'), aes(x=(`5-Year Mean Edge Density`), y=`5-Year Mean Revenue`/10^6)) + 
  geom_point(aes(fill = CSVI, colour = CSVI), size=4) + #geom_point(aes(size = (CSVI)) +
  geom_text_repel(aes(label=pcgroup), #, size = 2*`Scaled Revenue` #, size = 2*(`Long-term Mean Importance`)
                  force = 0.5,
                  #nudge_x = 0.1,
                  direction = "y",
                  hjust = 1,
                  segment.size = 0.2,
                  max.overlaps = 15) +
  geom_hline(
    yintercept=
      mean(
        plotdat_wide %>% 
          filter(metier.cm  == 'Whiting') %>% 
          pull(`5-Year Mean Revenue`)/10^6,
        na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  geom_vline(
    xintercept=
      mean(plotdat_wide %>% 
             filter(metier.cm  == 'Whiting') %>% 
             pull(`5-Year Mean Edge Density`),
           na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  xlim(0,1) +
  scale_colour_gradientn(colours = starfishpal) +
  scale_fill_gradientn(colours = starfishpal) +
  labs(
    x = "Low                                    Resilience Index                                 High\n(2016-2021)",
    y = "Economic Dependence on Whiting\n(2016-2021)"#,
  ) +
  theme_classic()
fig_ma_whiting

## Importance vs Revenue with CSVI

# main network metric plot - Salmon
fig_ma_salmon_imp <- ggplot(plotdat_wide %>% filter(metier.cm == 'Salmon'), aes(x=(`5-Year Mean Importance`), y=`5-Year Mean Revenue`/10^6)) + 
  geom_point(aes(fill = CSVI, colour = CSVI), size=4) + 
  geom_text_repel(aes(label=pcgroup), #, size = 2*`Scaled Revenue` #, size = 2*(`Long-term Mean Importance`)
                  force = 0.5,
                  #nudge_x = 0.1,
                  direction = "y",
                  hjust = 1,
                  segment.size = 0.2,
                  max.overlaps = 15) +
  geom_hline(
    yintercept=
      mean(
        plotdat_wide %>% 
          filter(metier.cm  == 'Salmon') %>% 
          pull(`5-Year Mean Revenue`)/10^6,
        na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  geom_vline(
    xintercept=
      mean(plotdat_wide %>% 
             filter(metier.cm  == 'Salmon') %>% 
             pull(`5-Year Mean Importance`),
           na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  xlim(0,1) +
  scale_colour_gradientn(colours = starfishpal) +
  scale_fill_gradientn(colours = starfishpal) +
  labs(#subtitle = '2020-2021',
    x = "Salmon Node Network Importance\n(2016-2021)",
    y = "Economic Dependence on Salmon\n(2016-2021)"#,
    #size = 'Long-term Mean\nImportance of Salmon\nNode in Network'
  ) +
  theme_classic() 
fig_ma_salmon_imp


# main network metric plot - Whiting
fig_ma_whiting_imp <- ggplot(plotdat_wide %>% filter(metier.cm == 'Whiting'), aes(x=(`5-Year Mean Importance`), y=`5-Year Mean Revenue`/10^6)) + 
  geom_point(aes(fill = CSVI, colour = CSVI), size=4) + #geom_point(aes(size = (CSVI)) +
  geom_text_repel(aes(label=pcgroup), #, size = 2*`Scaled Revenue` #, size = 2*(`Long-term Mean Importance`)
                  force = 0.5,
                  #nudge_x = 0.1,
                  direction = "y",
                  hjust = 1,
                  segment.size = 0.2,
                  max.overlaps = 15) +
  geom_hline(
    yintercept=
      mean(
        plotdat_wide %>% 
          filter(metier.cm  == 'Whiting') %>% 
          pull(`5-Year Mean Revenue`)/10^6,
        na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  geom_vline(
    xintercept=
      mean(plotdat_wide %>% 
             filter(metier.cm  == 'Whiting') %>% 
             pull(`5-Year Mean Edge Density`),
           na.rm=TRUE
      ), 
    colour='grey', alpha = 0.5) + 
  xlim(0,1) +
  scale_colour_gradientn(colours = starfishpal) +
  scale_fill_gradientn(colours = starfishpal) +
  labs(
    x = "Whiting Node Network Importance\n(2016-2021)",
    y = "Economic Dependence on Whiting\n(2016-2021)"#,
  ) +
  theme_classic()
fig_ma_whiting_imp

# save plots

png(here::here(pngdir, paste0("5-Year Mean Salmon Revenue versus 5-Year Mean Network Edge Density from IOPAC Fisheries Participation Networks with CSVI 2016-2021.png")),
    res=200,height=1200,width=1800)
fig_ma_salmon
dev.off()

png(here::here(pngdir, paste0("5-Year Mean Whiting Revenue versus 5-Year Mean Network Edge Density from IOPAC Fisheries Participation Networks with CSVI 2016-2021.png")),
    res=200,height=1200,width=1800)
fig_ma_whiting
dev.off()

png(here::here(pngdir, paste0("5-Year Mean Salmon Revenue versus 5-Year Mean Salmon Node Network Importance from IOPAC Fisheries Participation Networks with CSVI 2016-2021.png")),
    res=200,height=1200,width=1800)
fig_ma_salmon_imp
dev.off()

png(here::here(pngdir, paste0("5-Year Mean Whiting Revenue versus 5-Year Mean Whiting Node Network Importance from IOPAC Fisheries Participation Networks with CSVI 2016-2021.png")),
    res=200,height=1200,width=1800)
fig_ma_whiting_imp
dev.off()