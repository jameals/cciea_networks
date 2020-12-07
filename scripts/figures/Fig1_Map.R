############## Figure 1. Map #############
#
# 1/28/2019 - M. Fisher
#
##########################################


# Set up ------------------------------------------------------------------

library(maps)
library(tidyverse)
library(here)


# Basemap -----------------------------------------------------------------
ca_df <- map_data("state") %>%
  filter(region ==("california"))


# Coordinates -------------------------------------------------------------
# the latitude is an average of all port of landing locations within a port group
#   the longitude was set manually to be a point along the coast at the given latitude
pcgroup_coords <- read.csv(here::here('data','input','pcgroup_mean_coordinates.csv'))
colnames(pcgroup_coords)



# Map, Small Figure Area ---------------------------------------------------------------------
pcgroup_coords[3,"Lon_label"] <- pcgroup_coords[3,"Lon_label"] + 0.59 #CCA
pcgroup_coords[4,"Lon_label"] <- pcgroup_coords[4,"Lon_label"] - 0.06 #ERA
pcgroup_coords[2,"Lon_label"] <- pcgroup_coords[2,"Lon_label"] + 0.32 #BGA
pcgroup_coords[1,"Lon_label"] <- pcgroup_coords[1,"Lon_label"] + 0.47 #BDA
pcgroup_coords[1,"Lat_label"] <- pcgroup_coords[1,"Lat_label"] + 0.1 #BDA
pcgroup_coords[7,"Lon_label"] <- pcgroup_coords[7,"Lon_label"] + 0.69 #SFA
pcgroup_coords[7,"Lat_label"] <- pcgroup_coords[7,"Lat_label"] -0.15 #SFA
pcgroup_coords[6,"Lon_label"] <- pcgroup_coords[6,"Lon_label"] + 0.28 #MRA
pcgroup_coords[5,"Lon_label"] <- pcgroup_coords[5,"Lon_label"] + 0.18 #MNA


# map without legend, basic grey fill
myplot <- ggplot() +
  geom_polygon(data=ca_df, aes(x=long, y=lat, group=group), fill="grey67",linetype=1, color = "grey97") +
  geom_point(data=pcgroup_coords, aes(x=Lon, y=Lat, col=dcrb_between, pch=region), size = c(rep(5,4),rep(4,3))) +
  geom_point(data=pcgroup_coords, aes(x=Lon, y=Lat), pch=c(rep(1,4),rep(0,3)), col="black", size = c(rep(5,4),rep(4,3))) +
  geom_text(data=pcgroup_coords, aes(x=Lon_label, y=Lat_label, label=port_group_name),size=4) +
  theme_void() +
  scale_color_continuous(low="palegoldenrod", high="darkorange3", limits=c(0,1)) +
  scale_shape_manual(values=c(15,16)) +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.position="none") +
  coord_cartesian(xlim = c(-125, -114), ylim=c(33, 46))
myplot

png(here::here('results/figures','fig1/Figure1_map.png'), bg="transparent",width = 4, height = 5, units = 'in', res = 300)
myplot # Make plot
dev.off()

# map without legend, topographic fill


library(raster)
dem.raster <- getData("SRTM", lat = 38, lon = -120, download = TRUE)
dem.m  <-  rasterToPoints(dem.raster)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")
slope.raster <- terrain(dem.raster, opt='slope')
aspect.raster <- terrain(dem.raster, opt='aspect')
hill.raster <- hillShade(slope.raster, aspect.raster, 40, 270)
hill.m <- rasterToPoints(hill.raster)
hill.df <-  data.frame(hill.m)
colnames(hill.df) <- c("lon", "lat", "hill")


myplot <- ggplot() +
  geom_polygon(data=ca_df, aes(x=long, y=lat, group=group), fill="grey67",linetype=1, color = "grey97") +
  geom_raster(data = hill.df, aes(lon, lat, fill = hill), alpha = .45) +
  geom_point(data=pcgroup_coords, aes(x=Lon, y=Lat, col=dcrb_between, pch=region), size = c(rep(5,4),rep(4,3))) +
  geom_point(data=pcgroup_coords, aes(x=Lon, y=Lat), pch=c(rep(1,4),rep(0,3)), col="black", size = c(rep(5,4),rep(4,3))) +
  geom_text(data=pcgroup_coords, aes(x=Lon_label, y=Lat_label, label=port_group_name),size=4) +
  theme_void() +
  scale_color_continuous(low="palegoldenrod", high="darkorange3", limits=c(0,1)) +
  scale_shape_manual(values=c(15,16)) +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.position="none") +
  coord_cartesian(xlim = c(-125, -114), ylim=c(33, 46))
myplot




# legend, white text for dark background
plot_leg <- ggplot(data=pcgroup_coords, aes(x=Lon, y=Lat, col=dcrb_between)) +
  geom_point() +
  scale_color_continuous(low="palegoldenrod", high="darkorange3", name="Dungeness\n crab\nCentrality", limits=c(0,1)) +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.text=element_text(color='white', size=16),
        legend.title=element_text(color='white', size=18),
        legend.background = element_rect(fill='transparent', color=NA))
plot_leg

png(here::here('results/figures/fig1','Figure1_legend.png'), bg="transparent",width = 4, height = 5, units = 'in', res = 400)
plot_leg # Make plot
dev.off()















# Map, Large Figure Area ---------------------------------------------------------------------

# some manual adjustments to the label locations. these may need to be changed depending
#   on the size / resolution
pcgroup_coords[3,"Lon_label"] <- pcgroup_coords[3,"Lon_label"] + 0.4 #CCA
pcgroup_coords[4,"Lon_label"] <- pcgroup_coords[4,"Lon_label"] - 0.15 #ERA
pcgroup_coords[2,"Lon_label"] <- pcgroup_coords[2,"Lon_label"] + 0.15 #BGA
pcgroup_coords[1,"Lon_label"] <- pcgroup_coords[1,"Lon_label"] + 0.3 #BDA
pcgroup_coords[1,"Lat_label"] <- pcgroup_coords[1,"Lat_label"] + 0.1 #BDA
pcgroup_coords[7,"Lon_label"] <- pcgroup_coords[7,"Lon_label"] + 0.5 #SFA
pcgroup_coords[7,"Lat_label"] <- pcgroup_coords[7,"Lat_label"] -0.15 #SFA
pcgroup_coords[6,"Lon_label"] <- pcgroup_coords[6,"Lon_label"] + 0.15 #MRA
pcgroup_coords[5,"Lon_label"] <- pcgroup_coords[5,"Lon_label"] + 0.05 #MNA

# map without legend
myplot <- ggplot() +
  geom_polygon(data=ca_df, aes(x=long, y=lat, group=group), fill="grey67",linetype=1, color = "grey97") +
  geom_point(data=pcgroup_coords, aes(x=Lon, y=Lat, col=dcrb_between, pch=region), size = c(rep(5,4),rep(4,3))) +
  geom_point(data=pcgroup_coords, aes(x=Lon, y=Lat), pch=c(rep(1,4),rep(0,3)), col="black", size = c(rep(5,4),rep(4,3))) +
  geom_text(data=pcgroup_coords, aes(x=Lon_label, y=Lat_label, label=port_group_name),size=4) +
  theme_void() +
  scale_color_continuous(low="palegoldenrod", high="darkorange3", limits=c(0,1)) +
  scale_shape_manual(values=c(15,16)) +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.position="none") +
  coord_cartesian(xlim = c(-125, -114), ylim=c(33, 46))
myplot

png(here::here('results/figures','fig1/Figure1_map.png'), bg="transparent",width = 4, height = 5, units = 'in', res = 300)
myplot # Make plot
dev.off()


# legend, black text for light background
plot_leg <- ggplot(data=pcgroup_coords, aes(x=Lon, y=Lat, col=dcrb_between)) +
  geom_point() +
  scale_color_continuous(low="palegoldenrod", high="darkorange3", name="Dungeness\n   crab\nCentrality", limits=c(0,1)) +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.text=element_text(color='black', size=14),
        legend.title=element_text(color='black', size=14),
        legend.background = element_rect(fill='transparent', color=NA))
plot_leg

png(here::here('results/fig1/Figure1_legend.png'), bg="transparent",width = 4, height = 5, units = 'in', res = 300)
plot_leg # Make plot
dev.off()















