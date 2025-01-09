library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggimage)
library(cowplot)
library(here)

## output directory for network viz
pngdir_ports = 'data/networks/participation_vessel_ports/plots/comparable/'
fig_path = here::here(pngdir_ports)

#load network node images

thisyear <- 2018

thisyearsESRimages <- list.files(here::here(pngdir_ports), paste0(thisyear,"_circular_compare"))

img_wc <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("^WA Coast", thisyearsESRimages)])
)

img_ast <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Astoria", thisyearsESRimages)])
) 

img_nwp <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Newport", thisyearsESRimages)])
)

p_wc <- ggplot(data.frame(x = 1:2, y = 1:2), aes(x, y)) +
  geom_point(size = 1, colour=NA) +
  draw_image(img_wc , x = 1, y = 1, scale = 1.3) +
  theme_void()

p_ast <- ggplot(data.frame(x = 1:2, y = 1:2), aes(x, y)) +
  geom_point(size = 1, colour=NA) +
  draw_image(img_ast , x = 1, y = 1, scale = 1.3) +
  theme_void()

p_nwp <- ggplot(data.frame(x = 1:2, y = 1:2), aes(x, y)) +
  geom_point(size = 1, colour=NA) +
  draw_image(img_nwp , x = 1, y = 1, scale = 1.3) +
  theme_void()
  
p_harv <- plot_grid(p_wc, p_ast, p_nwp,
                    labels = c('WA Coast\n2018-19', 'Astoria\n2018-19', 'Newport\n2018-19'),
                    label_size = 12,
                    label_x = c(0,0,0)
)

p_harv
  
ggsave(here::here('data/gc5_ns8/WA Coast, Astoria, and Newport FPNs 2018-2019.png'), 
       p_harv, 
       width = 9, height = 5, dpi =600
)
