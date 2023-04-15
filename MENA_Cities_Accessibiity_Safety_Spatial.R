install.packages('devtools')
install.packages('ggsn')
install.packages("maps")
install.packages("mapdata")
install.packages("osmdata")
install.packages("basemaps")
install.packages("mapedit")
install.packages('patchwork')
install.packages("MetBrewer")
install.packages("kableExtra")

devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(viridis)
library(tidyverse)
library(ggsn)
library(maps)
library(mapdata)
library(osmdata)
library(ggmap)
library(patchwork)
library(geojsonsf)
library(stringr)
library(MetBrewer)
library(grid)
library(gridExtra)
library(kableExtra)
library(gtable)

options(java.parameters = '-Xmx4G')


setwd("C:/Users/Tamara Kerzhner/Box Sync/Research/WB MENA Gender/3City HH Survey/Maps")

#importing data sources

Bhhs <- fread("ID_Coordinates_hh_Beirut.csv")
Bhhs <- Bhhs %>% mutate(id=ID_crossSurvey)
Bhhs$id <- as.numeric(Bhhs$id)

Ahhs <- fread("ID_Coordinates_hh_Amman.csv")
Ahhs <- Ahhs %>% mutate(id=ID_crossSurvey)
Ahhs$id <- as.numeric(Ahhs$id)

Chhs <- fread("ID_Coordinates_hh_Cairo.csv")
Chhs <- Chhs %>% mutate(id=ID_crossSurvey)
Chhs$id <- as.numeric(Chhs$id)


#import safety audit locations

Cauds <- fread("Cairo_safelocs.csv") %>% select(c('lat','lon')) %>% mutate(type="Safety Audits")
Bauds <- fread("Beirut_safelocs.csv") %>% select(c('lat','lon')) %>% mutate(type="Safety Audits")
Aauds <- fread("Amman_safelocs.csv") %>% select(c('lat','lon')) %>% mutate(type="Safety Audits")

#intercept locations

Cints <- fread("Cairo_intercepts.csv") %>% select(c('lat','lon')) %>% mutate(type="Intercept Surveys")
Aints <- fread("Amman_Intercepts.csv")%>% select(c('lat','lon')) %>% mutate(type="Intercept Surveys")
Bints <- fread("Beirut_Intercepts.csv")%>% select(c('lat','lon')) %>% mutate(type="Intercept Surveys")

#import and join accessibility results to original household location coordinates

Beirut <- fread("Beirut_hhs_access.csv") %>% full_join(Bhhs, by="id")
Amman <- fread("Amman_hhs_access.csv") %>% full_join(Ahhs, by="id")
Cairo <- fread("Cairo_hhs_access.csv") %>% full_join(Chhs, by="id")

#import jobs distribution hexagon grids shapefiles

Ahex <- st_read("Amman_Employment_Hex.shp") %>% st_transform(crs = 4326)
Bhex <- st_read("Beirut_Employmet_Hex.shp")
Chex <- st_read("Cairo_Employment_Hex.shp")

#import results of Accessibiity analysis by HEX (not by household) for visualization, add columns at 100 multiplication,

B_HexAccess_45 <- fread("Beirut_hexsm60_access_45.csv") %>% full_join(Bhex, by="grid_id") %>% mutate(pct50=fifty*100) %>% mutate(pct90=ninety*100) %>% mutate(pct10=ten*100)
B_HexAccess_60 <- fread("Beirut_hexsm60_access_60.csv") %>% full_join(Bhex, by="grid_id") %>% mutate(pct50=fifty*100) %>% mutate(pct90=ninety*100) %>% mutate(pct10=ten*100)

A_HexAccess_45 <- fread("Amman_hexsm60_access_45.csv") %>% full_join(Ahex, by="grid_id") %>% mutate(pct50=fifty*100) %>% mutate(pct90=ninety*100) %>% mutate(pct10=ten*100)
A_HexAccess_60 <- fread("Amman_hexsm60_access_60.csv") %>% full_join(Ahex, by="grid_id") %>% mutate(pct50=fifty*100) %>% mutate(pct90=ninety*100) %>% mutate(pct10=ten*100)

C_HexAccess_45 <- fread("Cairo_hexsm60_access_45.csv")  %>% full_join(Chex, by="grid_id") %>% mutate(pct50=fifty*100) %>% mutate(pct90=ninety*100) %>% mutate(pct10=ten*100)
C_HexAccess_60 <- fread("Cairo_hexsm60_access_60.csv")  %>% full_join(Chex, by="grid_id") %>% mutate(pct50=fifty*100) %>% mutate(pct90=ninety*100) %>% mutate(pct10=ten*100)



#import results of proximity analysis to bus stops and join to hhs file

B_prox <- fread("B_proximity.csv")
Beirut <- full_join(Beirut,B_prox,by="id")


#create a r5r transit network for each city 

bdata <- "C:/Users/Tamara Kerzhner/Box Sync/Research/WB MENA Gender/3City HH Survey/Maps/Beirut"
bcore <- setup_r5(data_path = bdata, verbose = FALSE)


cdata <- "C:/Users/Tamara Kerzhner/Box Sync/Research/WB MENA Gender/3City HH Survey/Maps/Cairo"
ccore <- setup_r5(data_path = cdata, verbose = FALSE)
#For cairo - import shapefile of metro lines
C_metro <- st_read("Cairo_metro.shp")


adata <- "C:/Users/Tamara Kerzhner/Box Sync/Research/WB MENA Gender/3City HH Survey/Maps/Amman"
acore <- setup_r5(data_path = adata, verbose = FALSE)


#extract the stops from the r5r transit network 

B_stops <- btransit$stops
B_stops <- B_stops %>% separate (col = stop_id, into = c("type", "GStopID"), sep = ":")

#bus stops by frequency of operation:

B_stops_8AM <- fread("Beirut_prox_8AM.csv") %>% full_join(B_stops, by="GStopID")
B_stops_12PM <- fread("Beirut_prox_12PM.csv") %>% full_join(B_stops, by="GStopID")

#proximity analysis to stops/runs at level of entire hex grid

B_grid_prox_all <- fread("B_grid_prox_all.csv") %>% mutate(grid_id=id)
C_grid_prox_all <- fread("Cairo_grid_proximity.csv") %>% mutate(grid_id=id)
A_grid_prox_all <- fread("Amman_grid_proximity.csv")


#import safety data table - hex cells matched to nearest three safety audit locations (in ArcGiS)

B_Safety <- fread("Safety_Data_Beirut.csv")
B_safe_sumd <- B_Safety %>%
                    group_by(grid_id) %>%
                    summarise_at(vars(index_saf_1_7), list(safe_index = mean))
B_safe_sumd <- full_join(B_safe_sumd, Bhex, by="grid_id")


C_Safety <- fread("Cairo_safety_neartable.csv")
C_safe_sumd <- C_Safety %>%
  group_by(grid_id) %>%
  summarise_at(vars(index_saf_1_7), list(safe_index = mean))
C_safe_sumd <- full_join(C_safe_sumd, Chex, by="grid_id")


A_Safety <- fread("Amman_safety_neartable1.csv")
A_safe_sumd <- A_Safety %>%
  group_by(grid_id) %>%
  summarise_at(vars(index_saf_1_7), list(safe_index = mean))
A_safe_sumd <- full_join(A_safe_sumd, Ahex, by="grid_id")



# get shapes for bounding different city extents

Cairo_bound <- st_read("Cairo_mask.shp")
Beirut_bound <- st_read("Beirut_mask.shp")
Amman_bound <- st_read("Amman_bound.shp")

print(ggplot()+
       geom_sf(data=Cairo_bound, aes(geometry=geometry)))



############################################################################################



#Cairo access with white background and BE boundaries
#join 45 min and 60 min travel time data and organize for a 6-way facet
C_HexAccess_45L <- as.data.frame(C_HexAccess_45 %>% pivot_longer(cols=c("ten","fifty","ninety"), names_to="Frequency", values_to="acc_level"))
C_HexAccess_60L <- as.data.frame(C_HexAccess_60 %>% pivot_longer(cols=c("ten","fifty","ninety"), names_to="Frequency", values_to="acc_level"))

C_acc_data <- rbind(C_HexAccess_45L,C_HexAccess_60L) %>% mutate(acc_pct=acc_level*100)  %>% mutate(freq=recode(Frequency,
                                                                                                                "ten"="10% Reliability", 
                                                                                                                "fifty"="50% Reliability",
                                                                                                                "ninety"="90% Reliability"))


colnames(Cpop_acc_tbl) <- c("Travel Time", "Reliability 10%","Reliability 50%","Reliability 90%")
Ctbl <- tableGrob(Cpop_acc_tbl, rows = NULL)


Cairo_acc <- ggplot(C_acc_data)+
  geom_sf(aes(geometry=geometry, fill=acc_level), color='NA', alpha=1)+
  geom_sf(data=Cairo_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 80), oob = scales::squish, option="C")+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+  
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20), 
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  facet_grid(cutoff~~factor(freq, levels=c('10% Reliability', "50% Reliability", "90% Reliability")), switch="y", labeller=labeller(cutoff=c("45"="45 Minutes Travel Time", "60"="60 Minutes Travel Time")))+
  labs(fill = "Percentage of Jobs Reachable from a Location")+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Accessibility to Job Opportunity Locations, Cairo")+
  coord_sf(xlim = c(30.8, 31.8), ylim = c(30.35, 29.5))

ggsave("Cairo_acc.jpg", Cairo_acc, height=20, width=16)
print(Cairo_acc)

C_acc_wtable <- Cairo_acc/Ctbl
ggsave("Cairo_accwtable.jpg", C_acc_wtable, height=20, width=16)






B_HexAccess_45L <- B_HexAccess_45 %>% pivot_longer(cols=c("ten","fifty","ninety"), names_to="Frequency", values_to="acc_level")
B_HexAccess_60L <- B_HexAccess_60 %>% pivot_longer(cols=c("ten","fifty","ninety"), names_to="Frequency", values_to="acc_level")

B_acc_data <- union(B_HexAccess_45L,B_HexAccess_60L) %>% mutate(acc_pct=acc_level*100) %>% mutate(freq=recode(Frequency,
                                                                                                                  "ten"="10% Reliability", 
                                                                                                                  "fifty"="50% Reliability",
                                                                                                                  "ninety"="90% Reliability"))

Beirut_acc <- ggplot(B_acc_data)+
  geom_sf(aes(geometry=geometry, fill=acc_pct), color='NA', alpha=1)+
  geom_sf(data=Beirut_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 80), oob = scales::squish, option="C")+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+  
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20), 
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="Serif"))+
  facet_grid(cutoff~~factor(freq, levels=c('10% Reliability', "50% Reliability", "90% Reliability")), switch="y", labeller=labeller(cutoff=c("45"="45 Minutes Travel Time", "60"="60 Minutes Travel Time")))+
  labs(fill = "Percentage of Jobs Reachable from a Location")+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Accessibility to Job Opportunity Locations, Beirut")+
  coord_sf(xlim = c(35.4, 35.66), ylim = c(34.0, 33.62))


ggsave("Beirut_acc.jpg", Beirut_acc, height=20, width=16)
print(Beirut_acc)





A_HexAccess_45L <- A_HexAccess_45 %>% pivot_longer(cols=c("ten","fifty","ninety"), names_to="Frequency", values_to="acc_level")
A_HexAccess_60L <- A_HexAccess_60 %>% pivot_longer(cols=c("ten","fifty","ninety"), names_to="Frequency", values_to="acc_level")
A_HexAccess_60L <- subset(A_HexAccess_60L, select = -c(freq_diff, freq_pct_diff))

A_acc_data <- union(A_HexAccess_45L,A_HexAccess_60L)  %>% mutate(freq=recode(Frequency,
                                                            "ten"="10% Reliability", 
                                                            "fifty"="50% Reliability",
                                                            "ninety"="90% Reliability"))
                                                                 
Amman_acc <- ggplot(A_acc_data)+
  geom_sf(aes(geometry=geometry, fill=acc_level), color='NA', alpha=1)+
  geom_sf(data=Amman_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 80), oob = scales::squish, option="C")+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+  
  theme(panel.background = element_rect(fill="grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="Serif"))+
  facet_grid(cutoff~factor(freq, levels=c('10% Reliability', "50% Reliability", "90% Reliability")), switch="y", labeller=labeller(cutoff=c("45"="45 Minutes Travel Time", "60"="60 Minutes Travel Time")))+
  labs(fill = "Percentage of Jobs Reachable from a Location")+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Accessibility to Job Opportunity Locations, Amman")+
  coord_sf(xlim = c(35.74, 36.18), ylim = c(31.77, 32.11), crs = 4326)

ggsave("Amman_accgrey.jpg", Amman_acc, height=20, width=16)
print(Amman_acc)


############################################################################################

#map out the proximity to stops at level of entire hex grid
B_grid_prox_all_geo <- B_grid_prox_all %>% select(grid_id,runs_12_10,runs_12_20,runs_8_20,runs_8_10) %>% full_join(Bhex, by="grid_id") 
B_grid_prox_all_facet <- B_grid_prox_all_geo %>% pivot_longer(cols=c(runs_12_10,runs_12_20,runs_8_20,runs_8_10), names_to="vars", values_to="prox_runs") %>% separate(col = vars, into = c("runs", "hour", "distance"), sep = "_")
B_grid_prox_all_facet <- B_grid_prox_all_facet %>% mutate(normd = prox_runs/60)


labelsdist <- c("10" = "10 Minutes Walk Time", "20" = "20 Minutes Walk Time")
labelshour <- c("8" = "8 to 9 AM", "12" = "12 to 13 PM")


B_grid_prox_map<- ggplot()+
  geom_sf(data=B_grid_prox_all_facet, aes(geometry=geometry, fill=normd), color='NA', inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 1), oob = scales::squish, option="mako")+
  geom_sf(data=btransit$routes, aes(geometry=geometry, color="any"), size=0.5, inherit.aes = FALSE)+
  scale_color_manual(values = c("white"), labels=c(""))+
  geom_sf(data=Beirut_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  facet_grid(hour~distance, switch="y", labeller=labeller(distance=labelsdist, hour=labelshour))+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(fill = "Stop Proximity Index")+
  labs(color = "Public Transport Routes")+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(1,40,1,1),
        legend.box.margin=margin(20,20,20,20),
        legend.key.width= unit(5, 'cm'),
        legend.key.height = unit(1.5, 'cm'),
        legend.key = element_rect(fill = "#3B3969"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="Serif"))+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Proximity by Number of Buses per Hour")+
  coord_sf(xlim = c(35.4, 35.66), ylim = c(34.0, 33.62))


ggsave("Beirut_prox.jpg", B_grid_prox_map, height=20, width=16)
print(B_grid_prox_map)


C_grid_prox_all_geo <- C_grid_prox_all %>% select(grid_id,runs_12_10,runs_12_20,runs_8_20,runs_8_10) 
C_grid_prox_all_geo$grid_id <- as.character(C_grid_prox_all_geo$grid_id)


C_grid_prox_all_facet <- C_grid_prox_all_geo %>% pivot_longer(cols=c(runs_12_10,runs_12_20,runs_8_20,runs_8_10), names_to="vars", values_to="prox_runs") %>% separate(col = vars, into = c("runs", "hour", "distance"), sep = "_")
C_grid_prox_all_facet <- C_grid_prox_all_facet %>% mutate(normd = prox_runs/60)
C_grid_prox_all_facet <- C_grid_prox_all_facet %>% full_join(Chex, by="grid_id")


labelsdist <- c("10" = "10 Minutes Walk Time", "20" = "20 Minutes Walk Time")
labelshour <- c("8" = "8 to 9 AM", "12" = "12 to 13 PM")


C_grid_prox_map<- ggplot()+
  geom_sf(data=C_grid_prox_all_facet, aes(geometry=geometry, fill=normd), color='NA', inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 1), oob = scales::squish, option="mako")+
  geom_sf(data=ctransit$routes, aes(geometry=geometry, color="any"), size=0.07, inherit.aes = FALSE)+
  scale_color_manual(values = c("white"), labels=c(""))+
  geom_sf(data=Cairo_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  facet_grid(hour~distance, switch="y", labeller=labeller(distance=labelsdist, hour=labelshour))+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(fill = "Stop Proximity Index")+
  labs(color = "Public Transport Routes")+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(1,40,1,1),
        legend.box.margin=margin(20,20,20,20),
        legend.key.width= unit(5, 'cm'),
        legend.key.height = unit(1.5, 'cm'),
        legend.key = element_rect(fill = "#3B3969"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="Serif"))+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Proximity by Number of Buses per Hour")+
  coord_sf(xlim = c(30.8, 31.8), ylim = c(30.35, 29.5))

ggsave("Cairo_prox.jpg", C_grid_prox_map, height=20, width=16)
print(C_grid_prox_map)



A_grid_prox_all_geo <- A_grid_prox_all %>% select(grid_id,runs_12_10,runs_12_20,runs_8_20,runs_8_10) 
A_grid_prox_all_geo$grid_id <- as.character(A_grid_prox_all_geo$grid_id)
A_grid_prox_all_geo <- Ahex %>% full_join(A_grid_prox_all_geo, by="grid_id")


A_grid_prox_all_facet <- A_grid_prox_all_geo %>% pivot_longer(cols=c(runs_12_10,runs_12_20,runs_8_20,runs_8_10), names_to="vars", values_to="prox_runs") %>% separate(col = vars, into = c("runs", "hour", "distance"), sep = "_")
A_grid_prox_all_facet <- A_grid_prox_all_facet %>% mutate(normd = prox_runs/60)


labelsdist <- c("10" = "10 Minutes Walk Time", "20" = "20 Minutes Walk Time")
labelshour <- c("8" = "8 to 9 AM", "12" = "12 to 13 PM")


A_grid_prox_map<- ggplot()+
  geom_sf(data=A_grid_prox_all_facet, aes(geometry=geometry, fill=normd), color='NA', inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 1), oob = scales::squish, option="mako")+
  geom_sf(data=atransit$routes, aes(geometry=geometry, color="any"), size=0.07, inherit.aes = FALSE)+
  scale_color_manual(values = c("white"), labels=c(""))+
  geom_sf(data=Amman_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  facet_grid(hour~distance, switch="y", labeller=labeller(distance=labelsdist, hour=labelshour))+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(fill = "Stop Proximity Index")+
  labs(color = "Public Transport Routes")+
  theme(panel.background = element_rect(fill="grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Proximity by Number of Buses per Hour")+
  coord_sf(xlim = c(35.74, 36.18), ylim = c(31.77, 32.11), crs = 4326)

ggsave("Amman_proxgrey.jpg", A_grid_prox_map, height=20, width=16)
print(A_grid_prox_map)



############################################################################################

#map of underlying jobs distribution at grid
B_jobs<- ggplot()+
  geom_sf(data=Bhex, aes(geometry=geometry, fill=pred_relat), color='NA', inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 1.25), oob = scales::squish, option="viridis")+
  geom_sf(data=Beirut_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(fill = "Distribution of Jobs (percentage per cell)")+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="Serif"))+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Employment Opportunities Distribution")+

print(B_jobs)
ggsave("Beirut_jobs.jpg", B_jobs, height=20, width=16)


C_jobs<- ggplot()+
  geom_sf(data=Chex, aes(geometry=geometry, fill=pred_relat), color='NA', inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 0.3), oob = scales::squish, option="viridis")+
  geom_sf(data=Cairo_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(fill = "Distribution of Jobs (percentage per cell)")+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Employment Opportunities Distribution")+
  coord_sf(xlim = c(30.8, 31.8), ylim = c(30.35, 29.5))

ggsave("Cairo_jobs.jpg", C_jobs, height=20, width=16)
print(C_jobs)


A_jobs<- ggplot()+
  geom_sf(data=Ahex, aes(geometry=geometry, fill=pred_relat), color='NA', inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 0.5), oob = scales::squish, option="viridis")+
  geom_sf(data=Amman_bound, aes(geometry=geometry), color='black', fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(fill = "Distribution of Jobs (percentage per cell)")+
  theme(panel.background = element_rect(fill="grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Employment Opportunities Distribution")+
  coord_sf(xlim = c(35.74, 36.18), ylim = c(31.77, 32.11), crs = 4326)



ggsave("Amman_jobs.jpg", A_jobs, height=20, width=16)
print(A_jobs)



############################################################################################

#map of roads and transit

.jinit()
btransit <- transit_network_to_sf(bcore)
bstreets <- street_network_to_sf(bcore)


  
B_transpo<- ggplot()+
  geom_sf(data=bstreets$edges, color="grey", size=0.3, inherit.aes=FALSE)+
  geom_sf(data=btransit$routes, aes(color=btransit$routes$agency_id, size=btransit$routes$agency_id), inherit.aes=FALSE)+
  scale_color_discrete(name = "Public Transport Modes", labels = c(bei_pri_bus_02 = "Private Bus", bei_pub_bus_01="Public Bus",bei_van_03="Van"))+
  scale_size_manual(values = c(bei_pri_bus_02 = 3, bei_pub_bus_01 = 2, bei_van_03 = 1), labels = c(bei_pri_bus_02 = "Private Bus", bei_pub_bus_01="Public Bus",bei_van_03="Van"))+
  geom_sf(data=Beirut_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(color="Public Transport Modes")+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        legend.text = element_text(size=25),
        legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(3, 'cm'),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(3, 2, 1))))+
  guides(size = "none")+
  ggtitle("Road and Transport Network")+
  coord_sf(xlim = c(35.4, 35.66), ylim = c(34.0, 33.62))


ggsave("Beirut_transport.jpg", B_transpo, height=20, width=16)
print(B_transpo)


.jinit()
ctransit <- transit_network_to_sf(ccore)
cstreets <- street_network_to_sf(ccore)

#recategorize 10 modes into fewer modes...

C_metro <- C_metro %>% mutate(mapmode="Metro")
C_bus <- ctransit$routes
C_bus <- C_bus %>% mutate(mapmode = ifelse(agency_id == "AGY", "Bus Company",
                                    ifelse(agency_id == "MSR","Bus Company",
                                    ifelse(agency_id == "GRN","Bus Company",
                                    ifelse(agency_id == "BOX","Minibus",
                                    ifelse(agency_id == "COOP","Minibus",
                                    ifelse(agency_id == "P_B_8","Minibus",
                                    ifelse(agency_id == "CTA_M","Minibus",
                                    ifelse(agency_id == "P_O_14", "Minibus",
                                    ifelse(agency_id == "CTA", "Cairo Transit Authority Bus", "no"))))))))))

C_transpo<- ggplot()+
  geom_sf(data=cstreets$edges, color="grey", size=0.2, inherit.aes=FALSE)+
  geom_sf(data=C_bus, aes(color=mapmode, size=mapmode), inherit.aes=FALSE)+
  scale_color_viridis_d(name = "Public Transport Modes")+
  scale_color_met_d("Egypt") +
  geom_sf(data=C_metro, aes(color=mapmode), size=1, inherit.aes=FALSE)+
  scale_size_manual(values = c("Bus Company" = 4, "Cairo Transit Authority Bus" = 1.5, "Metro" = 2.5, "Minibus" = 0.5))+
  geom_sf(data=Cairo_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(color="Bus Modes")+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(40,20,20,20),
        legend.text = element_text(size=20),
        legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(3, 'cm'),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, ncol=2, override.aes = list(size = c(4, 1.5, 2.5, 0.5))))+
  guides(size = guide_legend(override.aes = list(size = 5)))+
  ggtitle("Road and Transport Network")+
  guides(size = "none")+
  coord_sf(xlim = c(30.8, 31.8), ylim = c(30.35, 29.5))


ggsave("Cairo_transport.jpg", C_transpo, height=20, width=16)
print(C_transpo)


atransit <- transit_network_to_sf(acore)
astreets <- street_network_to_sf(acore)



A_transpo<- ggplot()+
  geom_sf(data=astreets$edges, color="grey", size=0.3, inherit.aes=FALSE)+
  geom_sf(data=atransit$routes, aes(color=atransit$routes$agency_id, size=atransit$routes$agency_id), inherit.aes=FALSE)+
  geom_sf(data=Amman_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  labs(color="Public Transport Modes")+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        legend.text = element_text(size=25),
        legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(3, 'cm'),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(4.5, 2.5, 1, 0.5))))+
  guides(size = "none")+
  ggtitle("Road and Transport Network")+
  scale_colour_viridis_d(labels = c(amm_brt_01 = "BRT", amm_bus_02 = "Bus", amm_lar_03 = "Paratransit", amm_ser_05="Service"), option="B")+
  #scale_color_discrete(name = "Public Transport Modes", labels = c(amm_brt_01 = "BRT", amm_bus_02 = "Bus", amm_lar_03 = "Paratransit", amm_ser_05="Service"))+
  scale_size_manual(values = c(amm_brt_01 = 4.5, amm_bus_02 = 2.5, amm_lar_03 = 1, amm_ser_05=0.5), labels = c(amm_brt_01 = "BRT", amm_bus_02 = "Bus", amm_lar_03 = "Paratransit", amm_ser_05="Service"))+
  coord_sf(xlim = c(35.74, 36.18), ylim = c(31.77, 32.11), crs = 4326)

ggsave("Amman_transport.jpg", A_transpo, height=20, width=16)
print(A_transpo)



#########################################################################################################
#safety audit results

Beirut_safe <- ggplot(B_safe_sumd)+
  geom_sf(aes(geometry=geometry, fill=safe_index), color='NA', alpha=1)+
  geom_sf(data=Beirut_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 1), oob = scales::squish, option="A", breaks=c(0,0.5,1),
                       labels = c("0 - Least Safe", "0.5", "1 - Safest"))+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  theme(legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="Serif"))+
  labs(fill = "Safety Index")+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Safety Audit Estimation, Beirut")+
  coord_sf(xlim = c(35.4, 35.66), ylim = c(34.0, 33.62))


ggsave("Beirut_safe.jpg", Beirut_safe, height=20, width=16)
print(Beirut_safe)



Cairo_safe <- ggplot(C_safe_sumd)+
  geom_sf(aes(geometry=geometry, fill=safe_index), color='NA', alpha=1)+
  geom_sf(data=Cairo_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 1), oob = scales::squish, option="A", breaks=c(0,0.5,1),
                       labels = c("0 - Least Safe", "0.5", "1 - Safest"))+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  theme(legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="Serif"))+
  labs(fill = "Safety Index")+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Safety Audit Estimation, Cairo")+
  coord_sf(xlim = c(30.8, 31.8), ylim = c(30.35, 29.5))

ggsave("Cairo_safe.jpg", Cairo_safe, height=20, width=16)
print(Cairo_safe)

Amman_safe <- ggplot()+
  geom_sf(data=A_safe_sumd, aes(geometry=geometry, fill=safe_index), color='NA', alpha=1)+
  geom_sf(data=Amman_bound, aes(geometry=geometry), color=NA, fill="white", inherit.aes = FALSE)+
  coord_sf(xlim = c(35.74, 36.18), ylim = c(31.77, 32.11),crs = 4326 )+
  scale_fill_viridis_c(limits = c(0, 1), oob = scales::squish, option="A", breaks=c(0,0.5,1),
                       labels = c("0 - Least Safe", "0.5", "1 - Safest"))+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  theme(panel.background = element_rect(fill="grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(20,20,20,20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  labs(fill = "Safety Index")+
  guides(fill = guide_colorbar(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75))+
  ggtitle("Safety Audit Estimation, Amman")

    
ggsave("Amman_safe.jpg", Amman_safe, height=20, width=16)
print(Amman_safe)
  



############################################################################
########   Survey locations

# Create table with households  and audits for mappping

Chhs2 <- Chhs %>% select(c('lat','lon')) %>% mutate(type="Households")

C_hhs_auds <- union(Cauds, Chhs2)

C_h_a<- ggplot()+
  geom_sf(data=cstreets$edges, color="grey", size=0.2, inherit.aes=FALSE)+
  geom_point(data=C_hhs_auds, aes(x=lon, y=lat, color=type, size=type), inherit.aes=FALSE)+
  scale_color_met_d("Egypt") +
  scale_size_manual(values = c("Safety Audits"=4, "Households"=1), breaks = waiver())+
  geom_sf(data=Cairo_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  guides(color = guide_legend(barwidth=20,  barheight=3, title.vjust = 0.75, ncol=2))+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(40,20,20,20),
        legend.text = element_text(size=20),
        legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(3, 'cm'),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  ggtitle("Household and Safety Audit Locations")+
  guides(size = "none")+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(1,4))))+
  coord_sf(xlim = c(30.8, 31.8), ylim = c(30.35, 29.5))


ggsave("Cairo_hhs.jpg", C_h_a, height=20, width=16)
print(C_h_a)





Ahhs2 <- Ahhs %>% select(c('lat','lon')) %>% mutate(type="Households")

A_hhs_auds <- union(Aauds, Ahhs2)

A_h_a<- ggplot()+
  geom_sf(data=astreets$edges, color="grey", size=0.2, inherit.aes=FALSE)+
  geom_point(data=A_hhs_auds, aes(x=lon, y=lat, color=type, size=type), inherit.aes=FALSE)+
  scale_color_met_d("Egypt") +
  scale_size_manual(values = c("Safety Audits"=4, "Households"=1), breaks = waiver())+
  geom_sf(data=Amman_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  guides(color = guide_legend(barwidth=20,  barheight=3, title.vjust = 0.75, ncol=2))+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(40,20,20,20),
        legend.text = element_text(size=20),
        legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(3, 'cm'),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  ggtitle("Household and Safety Audit Locations")+
  guides(size = "none")+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(1,4))))+
  coord_sf(xlim = c(35.74, 36.18), ylim = c(31.77, 32.11),crs = 4326 )

ggsave("Amman_hhs.jpg", A_h_a, height=20, width=16)
print(A_h_a)


Bhhs2 <- Bhhs %>% select(c('lat','lon')) %>% mutate(type="Households")

B_hhs_auds <- union(Bauds, Bhhs2)

B_h_a<- ggplot()+
  geom_sf(data=bstreets$edges, color="grey", size=0.2, inherit.aes=FALSE)+
  geom_point(data=B_hhs_auds, aes(x=lon, y=lat, color=type, size=type), inherit.aes=FALSE)+
  scale_color_met_d("Egypt") +
  scale_size_manual(values = c("Safety Audits"=4, "Households"=1), breaks = waiver())+
  geom_sf(data=Beirut_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  guides(color = guide_legend(barwidth=20,  barheight=3, title.vjust = 0.75, ncol=2))+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(40,20,20,20),
        legend.text = element_text(size=20),
        legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(3, 'cm'),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  ggtitle("Household and Safety Audit Locations")+
  guides(size = "none")+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(1,4))))+
  coord_sf(xlim = c(35.4, 35.66), ylim = c(34.0, 33.62))

ggsave("Beirut_hhs.jpg", B_h_a, height=20, width=16)
print(B_h_a)




# Create table with households, intercepts and audits for mappping


C_hhs_auds_ints <- union(C_hhs_auds, Cints)

C_h_a_i<- ggplot()+
  geom_sf(data=cstreets$edges, color="grey", size=0.2, inherit.aes=FALSE)+
  geom_point(data=C_hhs_auds_ints, aes(x=lon, y=lat, color=type, size=type), inherit.aes=FALSE)+
  scale_color_met_d("Lakota") +
  scale_size_manual(values = c("Safety Audits"=4, "Intercept Surveys" = 2,"Households"=1), breaks = waiver())+
  geom_sf(data=Cairo_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  guides(color = guide_legend(barwidth=20,  barheight=3, title.vjust = 0.75, ncol=2))+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(40,20,20,20),
        legend.text = element_text(size=20),
        legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(3, 'cm'),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  ggtitle("Household, Intercept Surveys and Safety Audit Locations")+
  guides(size = "none")+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(1,2,4))))+
  coord_sf(xlim = c(30.8, 31.8), ylim = c(30.35, 29.5))


ggsave("Cairo_hhs2.jpg", C_h_a_i, height=20, width=16)
print(C_h_a_i)






A_hhs_auds_ints <- union(A_hhs_auds,Aints)

A_h_a_i<- ggplot()+
  geom_sf(data=astreets$edges, color="grey", size=0.2, inherit.aes=FALSE)+
  geom_point(data=A_hhs_auds_ints, aes(x=lon, y=lat, color=type, size=type), inherit.aes=FALSE)+
  scale_color_met_d("Lakota") +
  scale_size_manual(values = c("Safety Audits"=4, "Intercept Surveys" = 2,"Households"=1), breaks = waiver())+
  geom_sf(data=Amman_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  guides(color = guide_legend(barwidth=20,  barheight=3, title.vjust = 0.75, ncol=2))+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(40,20,20,20),
        legend.text = element_text(size=20),
        legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(3, 'cm'),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  ggtitle("Household, Intercept Surveys and Safety Audit Locations")+
  guides(size = "none")+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(1,2,4))))+
  coord_sf(xlim = c(35.74, 36.18), ylim = c(31.77, 32.11),crs = 4326 )

ggsave("Amman_hhs2.jpg", A_h_a_i, height=20, width=16)
print(A_h_a_i)



B_hhs_auds_ints <- union(B_hhs_auds,Bints)

B_h_a_i<- ggplot()+
  geom_sf(data=bstreets$edges, color="grey", size=0.2, inherit.aes=FALSE)+
  geom_point(data=B_hhs_auds_ints, aes(x=lon, y=lat, color=type, size=type), inherit.aes=FALSE)+
  scale_color_met_d("Lakota") +
  #scale_size_manual(values = c("Safety Audits" = 4, "Intercept SUrveys"= 2, "Households"= 1), breaks = waiver())+
  geom_sf(data=Beirut_bound, aes(geometry=geometry), color="black", size=0.3, fill="white", inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering, location='br')+
  annotation_scale(location = 'tl', width_hint = 0.4, height= unit(15, "points"), text_cex = 1.2)+
  guides(color = guide_legend(barwidth=20,  barheight=3, title.vjust = 0.75, ncol=2))+
  theme(panel.background = element_rect(fill="white"),
        legend.position="top",
        legend.justification = 'left',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(40,20,20,20),
        legend.text = element_text(size=20),
        legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(3, 'cm'),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 25, family="serif"))+
  ggtitle("Household, Intercept Surveys and Safety Audit Locations")+
  guides(size = "none")+
  guides(color = guide_legend(title.position = "top", barwidth=20,  barheight=3, title.vjust = 0.75, override.aes = list(size = c(1,2,4))))+
  coord_sf(xlim = c(35.4, 35.66), ylim = c(34.0, 33.62))

ggsave("Beirut_hhs2.jpg", B_h_a_i, height=20, width=16)
print(B_h_a_i)


Bintsplot <- ggplot()+
  geom_point(data=Bints, aes(x=lon, y=lat, color=type, size=type), inherit.aes=FALSE)
  
print(Bintsplot)
######################################AMMAN
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################



##################                  Older Maps Drafts             ##########################



#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

#collect background stamen map tiles and set boundaries for maps for each city - not in use in white-background maps


leb_zoom <- get_stamenmap(bbox=c(left=35.42, right=35.65, bottom=33.75, top=34),
                          maptype = "terrain", 
                          crop = TRUE,
                          zoom = 12)


leb_big <- get_stamenmap(bbox=c(left=35.35, right=35.77, bottom=33.53, top=34.3),
                         maptype = "terrain", 
                         crop = TRUE,
                         zoom = 10)


jor_zoom <- get_stamenmap(bbox=c(left=35.42, right=35.65, bottom=33.75, top=34),
                          maptype = "terrain", 
                          crop = TRUE,
                          zoom = 11)


jor_big <- get_stamenmap(bbox=c(left=35.5, right=36.3, bottom=31.53, top=32.3),
                         maptype = "terrain", 
                         crop = TRUE,
                         zoom = 10)


egy_zoom <- get_stamenmap(bbox=c(left=30.85, right=31.35, bottom=29.8, top=30.2),
                          maptype = "terrain", 
                          crop = TRUE,
                          zoom = 12)


egy_big <- get_stamenmap(bbox=c(left=30.6, right=32, bottom=29.3, top=30.51),
                         maptype = "terrain", 
                         crop = TRUE,
                         zoom = 10)


leb_zoom_w <- get_stamenmap(bbox=c(left=35.42, right=35.65, bottom=33.75, top=34),
                            maptype = "watercolor", 
                            crop = TRUE,
                            zoom = 11)


leb_big_w <- get_stamenmap(bbox=c(left=35.35, right=35.77, bottom=33.53, top=34.3),
                           maptype = "watercolor", 
                           crop = TRUE,
                           zoom = 10)



#Previous maps:
  
  
  ###Background Maps of Jobs *distribution*
  
  leb <- ggmap(leb_zoom)+
  geom_sf(data=Bhex, aes(geometry=geometry, fill=pred_relat), color=NA, alpha=0.8, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 0.75), oob = scales::squish)+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(family = "Verdana"))+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Beirut - City Employment Distribution")
print(leb)

lebb <- ggmap(leb_big)+
  geom_sf(data=Bhex, aes(geometry=geometry, fill=pred_relat), color=NA, alpha=0.8, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 0.75), oob = scales::squish)+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(family = "Verdana"))+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Beirut - Metro Employment Distribution")
print(lebb)


l <- leb+lebb
print(l)
ggsave("leb_jobmaps.jpg", l, height=14, width=12)



jorb <- ggplot(jor_big)+
  geom_sf(data=Ahex, aes(geometry=geometry), color='blue', alpha=0, inherit.aes = TRUE)+
  scale_fill_viridis_c(limits = c(0,1), oob = scales::squish)+
  geom_point(data=Amman, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(family = "Verdana"))+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Amman - Metro Employment Distribution")
print(jorb)


egy <- ggmap(egy_zoom)+
  geom_sf(data=Chex, aes(geometry=geometry, fill=pred_relat), color='NA', alpha=0.8, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 0.15), oob = scales::squish)+
  geom_point(data=Cairo, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(family = "Verdana"))+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Cairo - Metro Employment Distribution")
print(egy)

egyb <- ggmap(egy_big)+
  geom_sf(data=Chex, aes(geometry=geometry, fill=pred_relat), color='NA', alpha=0.8, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 0.15), oob = scales::squish)+
  geom_point(data=Cairo, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(family = "Verdana"))+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Cairo - City Employment Distribution")
print(egyb)

e <- egy+egyb
print(e)
ggsave("egy_jobmaps.jpg", e, height=12, width=12)



############################################################################################

###Background Maps of roads+transit stops and networks



btransit <- transit_network_to_sf(bcore)
bstreets <- street_network_to_sf(bcore)

ltransit <- ggmap(leb_zoom)+
  geom_sf(data=Bhex, fill="white", color='NA', alpha=0.9, inherit.aes = FALSE)+
  geom_sf(data=bstreets$edges, color="grey", inherit.aes=FALSE)+
  geom_sf(data=btransit$stops, inherit.aes=FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Beirut - Streets and Transit Stops")
print(ltransit)


ltransitbig <- ggmap(leb_big)+
  geom_sf(data=Bhex, fill="white", color='NA', alpha=0.9, inherit.aes = FALSE)+
  geom_sf(data=bstreets$edges, color="grey", inherit.aes=FALSE)+
  geom_sf(data=btransit$stops, inherit.aes=FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("Beirut - Streets and Transit Stops")
print(ltransitbig)


ltransits <- ltransit+ltransitbig
print(ltransits)
ggsave("leb_transitmaps.jpg", ltransits, height=14, width=12)



############################################################################################

###Accessibility at 45 min and 60 min threshold at the grid level:


lacc45 <- ggmap(leb_big)+
  geom_sf(data=B_HexAccess_45, aes(geometry=geometry, fill=pct50), color='NA', alpha=0.9, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 40), oob = scales::squish, option="C")+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs REACHABLE from Location")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))
print(lacc45)


lacc45zoom <- ggmap(leb_zoom)+
  geom_sf(data=B_HexAccess_45, aes(geometry=geometry, fill=pct50), color='NA', alpha=0.9, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 40), oob = scales::squish, option="C")+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs REACHABLE from Location")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Beirut - Access to Jobs, at 45 Minutes, 50% Reliability")
print(lacc45zoom)


lacc_45_50 <- lacc45zoom+lacc45
print(lacc_45_50)
ggsave("leb_access_45_50.jpg", lacc_45_50, height=14, width=12)



# Accessibility and 60 minutes and 45 minutes


lacc45zoom <- ggmap(leb_zoom)+
  geom_sf(data=B_HexAccess_45, aes(geometry=geometry, fill=pct50), color='NA', alpha=0.9, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 60), oob = scales::squish, option="C")+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs REACHABLE from Location")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("Beirut - Access to Jobs, at 45 Minutes")
print(lacc45zoom)


lacc60zoom <- ggmap(leb_zoom)+
  geom_sf(data=B_HexAccess_60, aes(geometry=geometry, fill=pct50), color='NA', alpha=0.9, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 60), oob = scales::squish, option="C")+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs REACHABLE from Location")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("60 Minutes, 50% Reliability")
print(lacc60zoom)


lacc_45and60_compare_50 <- lacc45zoom+lacc60zoom
print(lacc_45and60_compare_50)
ggsave("lacc_45and60_compare_5.jpg", lacc_45and60_compare_50, height=10, width=12)



############################################################################################

#map out the proximity to stops


B_Proxmap <- ggmap(leb_zoom)+
  geom_point(data=Beirut, aes(x=lon, y=lat, color=runs_8AM_20min), size=4, inherit.aes = FALSE)+
  scale_color_viridis_c(option="A")+
  annotation_north_arrow(style = north_arrow_orienteering)+
  labs(color = "within 20 min walk, 8 to 9 AM")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(color = guide_colorbar(title.position = "top", barwidth=10, title.vjust = 1))+
  ggtitle("Households, local transit services")
print(B_Proxmap)
ggsave("B_proxmap_8AM.jpg", B_Proxmap, height=14, width=12)


B_Stop8AM<- ggmap(leb_zoom)+
  geom_sf(data=B_stops_8AM, aes(geometry=geometry, color=Beirut_8AM_NumRuns), size=2, inherit.aes = FALSE)+
  scale_color_viridis_c(option="E")+
  annotation_north_arrow(style = north_arrow_orienteering)+
  labs(color = "8 to 9 AM")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(color = guide_colorbar(title.position = "top", barwidth=10, title.vjust = 1))+
  ggtitle("Bus Stops, Number of Buses per Hour")
print(B_Stop8AM)
ggsave("B_stops_8AM.jpg", B_Stop8AM, height=14, width=12)

B_Stop12PM<- ggmap(leb_zoom)+
  geom_sf(data=B_stops_12PM, aes(geometry=geometry, color=Beirut_12PM_NumRuns), size=2, inherit.aes = FALSE)+
  scale_color_viridis_c(option="E")+
  annotation_north_arrow(style = north_arrow_orienteering)+
  labs(color = "12 PM to 13 PM")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(color = guide_colorbar(title.position = "top", barwidth=10, title.vjust = 1))
print(B_Stop12PM)
ggsave("B_stops_12PM.jpg", B_Stop12PM, height=14, width=12)


Bstopruns <- B_Stop8AM+B_Stop12PM
ggsave("stoprunsBeirut.jpg", Bstopruns, height=12, width=12)



# Accessibility maps by frequency/reliability at the 60 minute range

lacc60zoom_50 <- ggmap(leb_zoom)+
  geom_sf(data=B_HexAccess_60, aes(geometry=geometry, fill=pct50), color='NA', alpha=0.9, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 60), oob = scales::squish, option="C")+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs REACHABLE from Location")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("50% Reliability")
print(lacc60zoom)

lacc60zoom_90 <- ggmap(leb_zoom)+
  geom_sf(data=B_HexAccess_60, aes(geometry=geometry, fill=pct90), color='NA', alpha=0.9, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 60), oob = scales::squish, option="C")+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs REACHABLE from Location")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("60 minutes, 90% Reliability")
print(lacc60zoom_90)

lacc60zoom_10 <- ggmap(leb_zoom)+
  geom_sf(data=B_HexAccess_60, aes(geometry=geometry, fill=pct10), color='NA', alpha=0.9, inherit.aes = FALSE)+
  scale_fill_viridis_c(limits = c(0, 60), oob = scales::squish, option="C")+
  geom_point(data=Beirut, aes(x=lon,y=lat), color="white", size=0.15, inherit.aes = FALSE)+
  annotation_north_arrow(style = north_arrow_orienteering)+
  annotation_scale(location = 'tl', width_hint = 0.4)+
  labs(fill = "% of all jobs REACHABLE from Location")+
  theme(legend.position="top",
        legend.justification = 'left',
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1))+
  ggtitle("10% Reliability")
print(lacc60zoom_10)


Bei_freqcompare_60 <- lacc60zoom_90+lacc60zoom_50+lacc60zoom_10
print(Bei_freqcompare_60)
ggsave("Bei_freqcompare_60.jpg", Bei_freqcompare_60, height=10, width=12)

stop_r5(bcore)
stop_r5(ccore)
