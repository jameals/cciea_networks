library(tidyverse)
library(ggmap)
library(maps)
library(ggthemes)
library(ggrepel)
library(ggimage)
library(cowplot)
library(here)

## directory for network viz
mydir = 'data/networks/participation_bulk/plots/2019_only'

## To avoid inclusion of rare or minimal fishing activity, a given fishery must contribute to at least contr_cutoff% of a port's seasonal revenue to be included in the network data. This cutoff value can be increased if more stringent fishery inclusion criteria are preferred, and decreased (i.e. for summaries over shorter temporal / smaller spatial scales).
contr_cutoff = 0.1

#load network node images
#wa
img_nwc <- magick::image_read(here::here(mydir,
                                         paste0("North WA Coast_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                         )
)
img_wc <- magick::image_read(here::here(mydir,
                                        paste0("WA Coast_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_ps <- magick::image_read(here::here(mydir,
                                        paste0("Puget Sound_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)

#or
img_ast <- magick::image_read(here::here(mydir,
                                         paste0("Astoria_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                         )
)
img_tl <- magick::image_read(here::here(mydir,
                                        paste0("Tillamook_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_nwp <- magick::image_read(here::here(mydir,
                                         paste0("Newport_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                         )
)
img_cb <- magick::image_read(here::here(mydir,
                                        paste0("Coos Bay_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_brk <- magick::image_read(here::here(mydir,
                                         paste0("Brookings_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                         )
)
img_cr <- magick::image_read(here::here(mydir,
                                        paste0("Columbia River_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)

#nca
img_cc <- magick::image_read(here::here(mydir,
                                        paste0("Crescent City_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_eu <- magick::image_read(here::here(mydir,
                                        paste0("Eureka_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_fb <- magick::image_read(here::here(mydir,
                                        paste0("Fort Bragg_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_bb <- magick::image_read(here::here(mydir,
                                        paste0("Bodega Bay_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_sf <- magick::image_read(here::here(mydir,
                                        paste0("San Francisco_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_mt <- magick::image_read(here::here(mydir,
                                        paste0("Morro Bay_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)

#sca
img_sd <- magick::image_read(here::here(mydir,
                                        paste0("San Diego_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_sb <- magick::image_read(here::here(mydir,
                                        paste0("Santa Barbara_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_la <- magick::image_read(here::here(mydir,
                                        paste0("Los Angeles_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)
img_mb <- magick::image_read(here::here(mydir,
                                        paste0("Morro Bay_2019_circular_cciea_bulk_",contr_cutoff*100, ".png")
                                        )
)


#load spreadsheet with lat-long coordinates for boxes
box <- read_csv(here::here('data',
                           'input',
                           'node_box_ref.csv'
                           )
)


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


NCA_map <- ggdraw(box_iopac(base_map_iopac("california", c(-130, -117), c(45.5, 36)), 
                            box_filter('NCA'), 
                            'NCA')) + 
  draw_image(img_cc, x = .45, y = .92, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_eu, x = .82, y = .91, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_fb, x = .4, y = .62, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_bb, x = .94, y = .62, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_sf, x = .45, y = .35, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_mt, x = .94, y = .35, hjust = 1, vjust = 1, width = 0.3, height = 0.3)

SCA_map <- ggdraw(box_iopac(base_map_iopac("california", c(-130, -117), c(36, 29)), 
                            box_filter('SCA'), 
                            'SCA')) + 
  draw_image(img_mb, x = .46, y = .82, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_sb, x = .72, y = .74, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_la, x = .68, y = .48, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(img_sd, x = .96, y = .48, hjust = 1, vjust = 1, width = 0.3, height = 0.3) 



#save figures
#cw
ggsave(here::here(mydir,
                  paste0("iopac-node_full-coast_rect_font_",contr_cutoff*100, ".png")), 
       CW_map, width = 6, height = 10, dpi =600)
ggsave(here::here(mydir,
                  paste0("iopac-node_WA_rect_",contr_cutoff*100, ".png")), 
       WA_map, width = 6, height = 6, dpi =600)
ggsave(here::here(mydir,
                  paste0("iopac-node_OR_rect_",contr_cutoff*100, ".png")), 
       OR_map, width = 6, height = 6, dpi =600)
ggsave(here::here(mydir,
                  paste0("iopac-node_NCA_rect_",contr_cutoff*100, ".png")), 
       NCA_map, width = 6, height = 6, dpi =600)
ggsave(here::here(mydir,
                  paste0("iopac-node_SCA_rect_",contr_cutoff*100, ".png")), 
       SCA_map, width = 6, height = 6, dpi =600)
