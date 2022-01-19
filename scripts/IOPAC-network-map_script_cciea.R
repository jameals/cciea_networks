library(tidyverse)
library(ggmap)
library(maps)
library(ggthemes)
library(ggrepel)
library(ggimage)
library(cowplot)
library(here)

## output directory for network viz
pngdir_ports = 'data/networks/participation_vessel_ports/plots/comparable/2020 only'

#load network node images
thisyearsESRimages <- list.files(here::here(pngdir_ports), "2020_circular_compare")

#wa
img_nwc <- magick::image_read(
  here::here(pngdir_ports,
  thisyearsESRimages[grep("North WA Coast", thisyearsESRimages)])
  )
img_wc <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("^WA Coast", thisyearsESRimages)])
)
img_ps <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Puget Sound", thisyearsESRimages)])
)

#or
img_ast <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Astoria", thisyearsESRimages)])
) 
img_tl <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Tillamook", thisyearsESRimages)])
)
img_nwp <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Newport", thisyearsESRimages)])
)
img_cb <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Coos Bay", thisyearsESRimages)])
)
img_brk <-  magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Brookings", thisyearsESRimages)])
)
img_cr <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Columbia River", thisyearsESRimages)])
)

#nca
img_cc <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Crescent City", thisyearsESRimages)])
)
img_eu <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Eureka", thisyearsESRimages)])
)
img_fb <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Fort Bragg", thisyearsESRimages)])
)
img_bb <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Bodega Bay", thisyearsESRimages)])
)
img_sf <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("San Francisco", thisyearsESRimages)])
)
img_mt <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Monterey", thisyearsESRimages)])
)

#sca
img_sd <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("San Diego", thisyearsESRimages)])
)
img_sb <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Santa Barbara", thisyearsESRimages)])
)
img_la <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Los Angeles", thisyearsESRimages)])
)
img_mb <- magick::image_read(
  here::here(pngdir_ports,
             thisyearsESRimages[grep("Morro Bay", thisyearsESRimages)])
)


#load spreadsheet with lat-long coordinates for boxes
box <- read_csv(here::here('data','input','node_box_ref.csv'))


##### coastwide figure #####

#identify ports of interest to include in the boxes
poi <- c('North WA Coast', 'Astoria', 'Fort Bragg', 'Morro Bay')


#function to create the grey base-map for maps
base_map_iopac <- function(state, xlim, ylim) {
  states <- map_data("state")
  west_coast <- subset(states, region %in% state)
  base_map <- ggplot(data = west_coast) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "gray80", alpha = 0.5, color = "gray85") +
    coord_map(xlim = xlim, ylim = ylim) +
    theme_map()
  return(base_map)
  }

# filter function for boxes to include
box_filter <- function(region) {
  box <- box %>% filter(map_version == region)
  return(box)
  }

# function to create iopac map with boxes
box_iopac <- function(basemap, box_filter, region) {
  iopac_rect <- basemap + 
  geom_point(data = subset(box, map_version %in% region), aes(x=PRIMARY_LONGITUDE, y=PRIMARY_LATITUDE, fill = STATE), colour="black", pch=21, size = 2) +
  geom_text_repel(aes(x=PRIMARY_LONGITUDE, y=PRIMARY_LATITUDE, label= IOPAC), data = subset(box, map_version %in% region), size = 2.15, 
                  box.padding = 0, point.padding = 0.25, hjust = -.25) +
  geom_rect(data = box_filter, aes(xmax = xmax, ymax = ymax, xmin = xmin, ymin=ymin), fill=NA, colour="darkgray") +
  geom_segment(data = subset(box_filter, segment_line == 'upper_right'), aes(x=PRIMARY_LONGITUDE, y=PRIMARY_LATITUDE, xend = xmax, yend = ymax), color = 'darkgray') +
  geom_segment(data = subset(box_filter, segment_line == 'lower_right'), aes(x=PRIMARY_LONGITUDE, y=PRIMARY_LATITUDE, xend = xmax, yend = ymin), color = 'darkgray') +
  geom_segment(data = subset(box_filter, segment_line == 'upper_left'), aes(x=PRIMARY_LONGITUDE, y=PRIMARY_LATITUDE, xend = xmin, yend = ymax), color = 'darkgray') +
  geom_segment(data = subset(box_filter, segment_line == 'lower_left'), aes(x=PRIMARY_LONGITUDE, y=PRIMARY_LATITUDE, xend = xmin, yend = ymin), color = 'darkgray') +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c('gray', 'black', 'white'))
  return(iopac_rect)
}


  



##### Combining IOPAC and Node images #####

#create add node images to IOPAC map--CW
CW_map <- ggdraw(box_iopac(base_map_iopac(c("washington", "oregon", "california"), c(-130, -117), c(NA, NA)), 
                            box_filter('CW'), 
                            c('WA', 'OR', 'NCA', 'SCA'))) + 
  draw_image(img_nwc, x = .58, y = .92, hjust = 1, vjust = 1, width = 0.34, height = 0.34) +
  draw_image(img_ast, x = .58, y = .75, hjust = 1, vjust = 1, width = 0.34, height = 0.34) +
  draw_image(img_fb, x = .58, y = .56, hjust = 1, vjust = 1, width = 0.34, height = 0.34) +
  draw_image(img_mb, x = .73, y = .39, hjust = 1, vjust = 1, width = 0.34, height = 0.34) 

CW_map


##WA 


WA_map <- ggdraw(box_iopac(base_map_iopac("washington", c(-130, -117), c(NA, NA)), 
                             box_filter('WA'), 
                             'WA')) + 
  draw_image(img_ps, x = .69, y = .97, hjust = 1, vjust = 1, width = 0.32, height = 0.32) +
  draw_image(img_nwc, x = .45, y = .62, hjust = 1, vjust = 1, width = 0.32, height = 0.32) +
  draw_image(img_wc, x = .7, y = .35, hjust = 1, vjust = 1, width = 0.32, height = 0.32) 
  
WA_map

OR_map <- ggdraw(box_iopac(base_map_iopac("oregon", c(-130, -117), c(NA, NA)), 
                             box_filter('OR'), 
                             'OR')) + 
  draw_image(img_cr, x = .95, y = 1, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_ast, x = .65, y = .97, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_tl, x = .35, y = .89, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_nwp, x = .42, y = .62, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_cb, x = .41, y = .35, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_brk, x = .83, y = .3, hjust = 1, vjust = 1, width = 0.3, height = 0.3)

OR_map

NCA_map <- ggdraw(box_iopac(base_map_iopac("california", c(-130, -117), c(45.5, 36)), 
                            box_filter('NCA'), 
                            'NCA')) + 
  draw_image(img_cc, x = .45, y = .92, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_eu, x = .82, y = .91, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_fb, x = .4, y = .62, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_bb, x = .94, y = .62, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_sf, x = .45, y = .35, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_mt, x = .94, y = .35, hjust = 1, vjust = 1, width = 0.3, height = 0.3)

NCA_map

SCA_map <- ggdraw(box_iopac(base_map_iopac("california", c(-130, -117), c(36, 29)), 
                            box_filter('SCA'), 
                            'SCA')) + 
  draw_image(img_mb, x = .46, y = .82, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_sb, x = .72, y = .74, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_la, x = .68, y = .48, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_sd, x = .96, y = .48, hjust = 1, vjust = 1, width = 0.3, height = 0.3) 

SCA_map

#save figures
ggsave(here::here(pngdir_ports,'iopac-node_full-coast_rect_font.png'), CW_map, width = 6, height = 10, dpi =600)
ggsave(here::here(pngdir_ports,'iopac-node_WA_rect.png'), WA_map, width = 6, height = 6, dpi =600)
ggsave(here::here(pngdir_ports,'iopac-node_OR_rect.png'), OR_map, width = 6, height = 6, dpi =600)
ggsave(here::here(pngdir_ports,'iopac-node_NCA_rect.png'), NCA_map, width = 6, height = 6, dpi =600)
ggsave(here::here(pngdir_ports,'iopac-node_SCA_rect.png'), SCA_map, width = 6, height = 6, dpi =600)

#all network graphs
png(here::here(pngdir_ports, paste0("All Fisheries Participation Networks 2020-2021, by IOPAC Port Group.png")),
    res=200, height=1600,width=1600)  # ,height=1400,width=1600

ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
  geom_point(size = 1, colour=NA) +
  draw_image(img_nwc , x = 1, y = 9, scale = 2) +
  draw_image(img_wc , x = 3, y = 9, scale = 2) +
  draw_image(img_ps , x = 5, y = 9, scale = 2) +
  draw_image(img_ast , x = 7, y = 9, scale = 2) +
  draw_image(img_tl , x = 9, y = 9, scale = 2) +
  draw_image(img_cr , x = 1, y = 7, scale = 2) +
  draw_image(img_nwp , x = 3, y = 7, scale = 2) +
  draw_image(img_brk , x = 5, y = 7, scale = 2) +
  draw_image(img_cb , x = 7, y = 7, scale = 2) +
  draw_image(img_cc , x = 9, y = 7, scale = 2) +
  draw_image(img_eu , x = 1, y = 5, scale = 2) +
  draw_image(img_fb , x = 3, y = 5, scale = 2) +
  draw_image(img_bb , x = 5, y = 5, scale = 2) +
  draw_image(img_sf , x = 7, y = 5, scale = 2) +
  draw_image(img_mt , x = 9, y = 5, scale = 2) +
  draw_image(img_mb , x = 1, y = 3, scale = 2) +
  draw_image(img_sb, x = 3, y = 3, scale = 2) + 
  draw_image(img_la , x = 5, y = 3, scale = 2) +
  draw_image(img_sd , x = 7, y = 3, scale = 2) + 
  theme_void()

dev.off()
