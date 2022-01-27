# Combine Coos Bay 2013 v 2021 with node strength v revenue for non-DTS groundfish
library(tidyverse)
library(ggmap)
library(maps)
library(ggthemes)
library(ggrepel)
library(ggimage)
library(cowplot)
library(here)

## output directory for network viz
pngdir_ports = 'data/networks/participation_vessel_ports/plots/comparable/2020 only/.'

#input directory for network node images
fig_path
thisyearsESRimages <- list.files(here::here(pngdir_ports), "2020_circular_compare")

fig_path_2020 <- here::here('data/networks/participation_vessel_ports/plots/comparable/')

#input directory for nonDTS figure
pngdir <- 'results/figures'

# load files

cb_ESRimages <- list.files(fig_path_2020, "Coos Bay")

img_cb_2020 <- magick::image_read(
  here::here(wa_fig_path,
             cb_ESRimages[grep("Coos Bay_2020", cb_ESRimages)])
)

img_cb_2012 <- magick::image_read(
  here::here(wa_fig_path,
             cb_ESRimages[grep("Coos Bay_2012", cb_ESRimages)])
)

img_nonDTS <- magick::image_read(
  here::here(pngdir, "Scatter Plot of Non-DTS Groundfish Node Strength in Fisheries Participation Networks and Aggregate Revenue 2016-2021, by Region for IOPAC Port Groups.png")
  )

# DTS figure
p_nonDTS <- ggplot()+ #data.frame(x = 1:2, y = 1:2), aes(x, y)) +
  #geom_point(size = 1, colour=NA) +
  draw_image(img_nonDTS, scale = 1.1) +
  theme_void()

# Coos Bay figure
p_cb_2012 <- ggplot() +
  draw_image(img_cb_2012, scale = 1.1) +
  theme_void() 

p_cb_2020_2 <- ggplot() +
  draw_image(img_cb_2020, scale = 1.1) +
  theme_void()

p_cb_2012_v_2020 <- plot_grid(p_cb_2012, p_cb_2020_2,
                              labels = c('A) Coos Bay 2013', 'B) Coos Bay 2021'), 
                              label_size = 12, label_y = c(1, 1),
                              label_x = c(0, 0), align = "hv",
                              rel_widths = c(1, 1), ncol = 2, vjust=1
) #label_x = 0.4, c(0.35, 0.35, 0.35, 0.35, 0.35, 0.35)

p_cb_2012_v_2020

# make the combined plot
p_cb_nonDTS <- plot_grid(p_cb_2012_v_2020, p_nonDTS,
                         align = "hv", rel_widths = c(1, 1, 2), 
                         rel_heights=c(1, 1.2), nrow = 2, labels = c('','C)'), vjust=2.5
) #label_x = 0.4, c(0.35, 0.35, 0.35, 0.35, 0.35, 0.35),  

p_cb_nonDTS

ggsave(here::here(pngdir_ports,'ESR 2022 figure.png'),
       p_cb_nonDTS,
       width = 7, height = 5, dpi =600
)

