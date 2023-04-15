remove.packages("pillar")
install.packages("pillar")
install.packages("dplyr")
install.packages("pollster")
install.packages("treemapify")
install.packages("ggbreak")
install.packages("forcats")

library("tidyr")
library("ggplot2")
library(basemaps)
library(ggmap)
library(lubridate)
library(MetBrewer)
library(patchwork)
library(pillar)
library(dplyr)
library(sf)
library(pollster)
library(treemapify)
library(ggbreak) 
library(forcats)




kmp <- get_stamenmap(bbox=c(left=32.57, right=32.64, bottom=0.28, top=0.39),
                     maptype = "terrain", 
                     crop = TRUE,
                     zoom = 13)



#analysis of the on board passenger survey

setwd("C:/Users/Tamara Kerzhner/Box Sync/Research/Kampala New Routes/Data/Analysis - GPS Counts")




#import table
onbus <- read.csv("Passenger On-Bus Survey_WIDE(38).csv")

onbus$starttime <- strptime(onbus$starttime,
                                 format = "%b %d, %Y %I:%M:%S %p")
onbus$SubmissionDate <- strptime(onbus$SubmissionDate,
                            format = "%b %d, %Y %I:%M:%S %p")

onbus <- onbus %>% mutate(hour=hour(starttime)) %>% mutate(date=as.factor(date(starttime)))
onbus <- onbus %>%  filter(gender=="woman" | gender=="man")  %>%  filter (starttime > "2023-02-06 00:00:00")

route <- st_read("Komamboga_to_Bukasa.shp")

onbus <- onbus %>% mutate(board_num = ifelse(where_board == "bukasa","01 - Bukasa",
                      ifelse(where_board == "namuwongo", "02 - Namuwongo",
                               ifelse(where_board == "industrial", "03 - Industrial Area",
                                    ifelse(where_board == "jinja_lugogo", "05 - Lugogo Bypass",
                                           ifelse(where_board == "nakawa", "04 - Nakawa Market",
                                                  ifelse(where_board == "kira", "06 - Kira Road",
                                                         ifelse(where_board == "bukoto", "07 - Bukoto",
                                                                ifelse(where_board == "kisaasi", "08 - Kisaasi",
                                                                       ifelse(where_board == "bahai", "09 - Bahai Temple",
                                                                              ifelse(where_board == "komamboga", "10 - Komamboga", 99)))))))))))

onbus <- onbus %>% mutate(depart_num = ifelse(where_depart == "bukasa","01 - Bukasa",
                                             ifelse(where_depart == "namuwongo", "02 - Namuwongo",
                                                    ifelse(where_depart == "industrial", "03 - Industrial Area",
                                                           ifelse(where_depart == "jinja_lugogo", "05 - Lugogo Bypass",
                                                                  ifelse(where_depart == "nakawa", "04 - Nakawa Market",
                                                                         ifelse(where_depart == "kira", "06 - Kira Road",
                                                                                ifelse(where_depart == "bukoto", "07 - Bukoto",
                                                                                       ifelse(where_depart == "kisaasi", "08 - Kisaasi",
                                                                                              ifelse(where_depart == "bahai", "09 - Bahai Temple",
                                                                                                     ifelse(where_depart == "komamboga", "10 - Komamboga", 99)))))))))))






onbus <- onbus %>% mutate(lat_board = ifelse(where_board == "bukasa",0.28969735355642856,
                                             ifelse(where_board == "namuwongo", 0.304567560066427,
                                                    ifelse(where_board == "industrial", 0.31399270511626853,
                                                           ifelse(where_board == "jinja_lugogo", 0.331898753757582,
                                                                  ifelse(where_board == "nakawa", 0.329893837639375, 
                                                                         ifelse(where_board == "kira", 0.3429502293012055,
                                                                                ifelse(where_board == "bukoto", 0.35206198580014963,
                                                                                       ifelse(where_board == "kisaasi", 0.3693074649458633,
                                                                                              ifelse(where_board == "bahai", 0.3640262423088344,
                                                                                                     ifelse(where_board == "komamboga", 0.38535203481458086, 99)))))))))))


onbus <- onbus %>% mutate(lon_board = ifelse(where_board == "bukasa",32.6276067287487,
                                       ifelse(where_board == "namuwongo", 32.613977579224255,
                                              ifelse(where_board == "industrial", 32.61103787839846,
                                                     ifelse(where_board == "jinja_lugogo",  32.60226069843045,
                                                            ifelse(where_board == "nakawa", 32.612138742001875,
                                                                   ifelse(where_board == "kira", 32.59552863602753,
                                                                          ifelse(where_board == "bukoto", 32.59777809126297,
                                                                                 ifelse(where_board == "kisaasi", 32.59678376493919,
                                                                                        ifelse(where_board == "bahai", 32.58586250060269,
                                                                                               ifelse(where_board == "komamboga", 32.588163064095234, 99)))))))))))



onbus <- onbus %>% mutate(lat_board = ifelse(where_board == "bukasa",0.28969735355642856,
                                             ifelse(where_board == "namuwongo", 0.304567560066427,
                                                    ifelse(where_board == "industrial", 0.31399270511626853,
                                                           ifelse(where_board == "jinja_lugogo", 0.331898753757582,
                                                                  ifelse(where_board == "nakawa", 0.329893837639375, 
                                                                         ifelse(where_board == "kira", 0.3429502293012055,
                                                                                ifelse(where_board == "bukoto", 0.35206198580014963,
                                                                                       ifelse(where_board == "kisaasi", 0.3693074649458633,
                                                                                              ifelse(where_board == "bahai", 0.3640262423088344,
                                                                                                     ifelse(where_board == "komamboga", 0.38535203481458086, 99)))))))))))


onbus <- onbus %>% mutate(lon_board = ifelse(where_board == "bukasa",32.6276067287487,
                                             ifelse(where_board == "namuwongo", 32.613977579224255,
                                                    ifelse(where_board == "industrial", 32.61103787839846,
                                                           ifelse(where_board == "jinja_lugogo",  32.60226069843045,
                                                                  ifelse(where_board == "nakawa", 32.612138742001875,
                                                                         ifelse(where_board == "kira", 32.59552863602753,
                                                                                ifelse(where_board == "bukoto", 32.59777809126297,
                                                                                       ifelse(where_board == "kisaasi", 32.59678376493919,
                                                                                              ifelse(where_board == "bahai", 32.58586250060269,
                                                                                                     ifelse(where_board == "komamboga", 32.588163064095234, 99)))))))))))


onbus <- onbus %>% mutate(lat_depart = ifelse(where_depart == "bukasa",0.28969735355642856,
                                             ifelse(where_depart == "namuwongo", 0.304567560066427,
                                                    ifelse(where_depart == "industrial", 0.31399270511626853,
                                                           ifelse(where_depart == "jinja_lugogo", 0.331898753757582,
                                                                  ifelse(where_depart == "nakawa", 0.329893837639375, 
                                                                         ifelse(where_depart == "kira", 0.3429502293012055,
                                                                                ifelse(where_depart == "bukoto", 0.35206198580014963,
                                                                                       ifelse(where_depart == "kisaasi", 0.3693074649458633,
                                                                                              ifelse(where_depart == "bahai", 0.3640262423088344,
                                                                                                     ifelse(where_depart == "komamboga", 0.38535203481458086, 99)))))))))))


onbus <- onbus %>% mutate(lon_depart = ifelse(where_depart == "bukasa",32.6276067287487,
                                             ifelse(where_depart == "namuwongo", 32.613977579224255,
                                                    ifelse(where_depart == "industrial", 32.61103787839846,
                                                           ifelse(where_depart == "jinja_lugogo",  32.60226069843045,
                                                                  ifelse(where_depart == "nakawa", 32.612138742001875,
                                                                         ifelse(where_depart == "kira", 32.59552863602753,
                                                                                ifelse(where_depart == "bukoto", 32.59777809126297,
                                                                                       ifelse(where_depart == "kisaasi", 32.59678376493919,
                                                                                              ifelse(where_depart == "bahai", 32.58586250060269,
                                                                                                     ifelse(where_depart == "komamboga", 32.588163064095234, 99)))))))))))


od <- onbus %>% filter(board_num<99) %>% count(which_way, date, board_num, depart_num, sort = FALSE)  %>% mutate(freq=n/sum(n))
od_or <- onbus %>% filter(board_num<99) %>% count(board_num, sort = FALSE)  %>% mutate(freq=n/sum(n))
od_de <- onbus %>% filter(board_num<99) %>% count(depart_num, sort = FALSE)  %>% mutate(freq=n/sum(n))
od_dir <- onbus %>% filter(board_num<99) %>% count(which_way, board_num, depart_num, sort = FALSE)  %>% mutate(freq=n/sum(n))



odmat <- ggplot(od)+ #%>% filter(date=="2023-02-20") %>% ggplot() +
  geom_tile(aes(depart_num, board_num, fill=freq))+
  geom_text(aes(depart_num, board_num, label = n), color="white")+
  geom_abline(intercept = 11, slope = -1, color="grey50")+
  scale_fill_met_c("Homer2")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~date)+
  scale_x_discrete(limits = rev(unique(sort(od$depart_num))))
  
print(odmat)


od2 <- onbus %>% filter(board_num<99) %>% 
  count(board_num, depart_num, sort = FALSE)  %>% 
  mutate(freq=n/sum(n)) %>%
  mutate(labs=freq*100) %>% mutate(labs=round(labs, 1)) %>% 
  mutate(labs2="%") %>% 
  mutate(name = paste(labs, labs2, sep = ""))

odmat2 <- ggplot(od2) +
  geom_tile(aes(depart_num, board_num, fill=n))+
  geom_abline(intercept = 11, slope = -1, color="grey50")+
  geom_text(aes(depart_num, board_num, label = name),color="white", size=3, fontface="bold")+
  scale_fill_met_c("Homer2")+
  theme(axis.text.x = element_text(angle = 50, vjust = 1.0, hjust=1),
        axis.text.y = element_text(angle = 50, vjust = 1.0, hjust=1),
        legend.position="NA")+
  scale_x_discrete(limits = rev(unique(sort(od2$depart_num))))+
  labs(title="New Paratransit Route Passengers",
       x ="Alighting", y = "Boarding")
  

print(odmat2)

print(odmat+odmat2)

odmat3 <- ggplot(od2) +
  geom_tile(aes(depart_num, board_num, fill=n))+
  geom_abline(intercept = 11, slope = -1, color="grey50")+
  geom_text(aes(depart_num, board_num, label = name),color="white", size=3, fontface="bold")+
  scale_fill_met_c("Homer2")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="NA")+
  scale_x_discrete(limits = rev(unique(sort(od2$depart_num))))+
  geom_point(data = od_de, aes(x=depart_num, y=0, color = n), size = 14, shape = 19)+
  geom_text(data=od_de, aes(x=depart_num, y=0, label = n), color="white")+
  geom_text(data=od_de, aes(x=depart_num, y=-0.5, label = n), color="white", alpha=0)+
  geom_point(data = od_or, aes(x=0, y=board_num, color = n), size = 14, shape = 19)+
  geom_text(data= od_or, aes(x=0, y=board_num, label = n), color="white")+
  geom_text(data=od_or, aes(x=-0.5, y=board_num, label = n), color="white", alpha=0)+
  scale_color_met_c("Homer2")+
  labs(title="New Paratransit Route Passengers",
       x ="Departure Location", y = "Boarding Location")

time_count <- onbus %>%  count(hour, date, sort = FALSE)  %>% mutate(freq=n/sum(n))
hour_count <- onbus %>% count(hour, sort = FALSE)  %>% mutate(freq=n/sum(n))
day_count <- onbus %>% count(date, sort = FALSE)  %>% mutate(freq=n/sum(n))



count_otime <- ggplot(time_count) +
  geom_tile(aes(hour, date, fill=freq))+
  geom_text(aes(hour, date, label = n), color="white")+
  scale_fill_met_c("Tam")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_point(data = hour_count, aes(x=hour, y="2023-02-05", color = n), size = 13, shape = 19)+
  geom_text(data=hour_count, aes(x=hour, y="2023-02-05", label = n), color="white")+
  geom_point(data = day_count, aes(x=24, y=date, color = n), size = 13, shape = 19)+
  geom_text(data= day_count, aes(x=24, y=date, label = n), color="white")
  

print(count_otime)



odcount <- ggplot(odgen) +
  geom_tile(aes(x=board_num, y=depart_num, fill=freq))+
  #facet_wrap(~board_num)+
  scale_fill_met_c("VanGogh1")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+

print(odcount)


oddatemat <- ggplot(odgen) +
  geom_tile(aes(x=board_num, y=depart_num, fill=freq))+
  scale_fill_met_c("Johnson")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(oddatemat)


wo_B_N <- onbus %>% filter(!(board_num == "02 - Namuwongo" & depart_num == "01 - Bukasa")) %>% filter(!(depart_num == "02 - Namuwongo" & board_num == "01 - Bukasa"))
odwbn <- wo_B_N %>% filter(board_num<99) %>% count(date, gender, board_num, depart_num, lat_board, lon_board, sort = FALSE) 
odwbn <- odwbn %>% mutate(freq=n/sum(n)) %>% group_by(gender) %>% mutate(freq_gender=n/sum(n))


od_wbn <- ggplot(odwbn) +
  geom_tile(aes(x=board_num, y=depart_num, fill=freq))+
  scale_fill_met_c("Johnson")+
  geom_text(aes(hour, date, label = n), color="white")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


print(od_wbn)

odmap <- onbus %>% filter(board_num<99) %>% count(gender, board_num, depart_num, lat_board, lon_board, lat_depart, lon_depart, sort = FALSE) 
od_nogen <- onbus %>% filter(board_num<99) %>% count(board_num, depart_num, lat_board, lon_board, lat_depart, lon_depart, sort = FALSE) 



xmen <- odmap %>% filter(gender=="man") %>% select(lat_board,lon_board,lat_depart,lon_depart,n)
xmen[, -1] <- xmen[, -1] + rnorm(cumprod(dim(xmen[, -1]))[-1], sd = 0.0001) # You can change the sd valu


xwmn<- odmap %>% filter(gender=="woman") %>% select(lat_board,lon_board,lat_depart,lon_depart,n)
xwmn[, -1] <- xwmn[, -1] + rnorm(cumprod(dim(xwmn[, -1]))[-1], sd = 0.0001) # You can change the sd valu

x<- od_nogen %>% select(lat_board,lon_board,lat_depart,lon_depart,n) 
x[, -1] <- x[, -1] + rnorm(cumprod(dim(x[, -1]))[-1], sd = 0.0001)

stations <- onbus %>% count(lat_depart, lon_depart, where_depart)
stations$where_depart[stations$where_depart == 'jinja_lugogo'] <- 'lugogo'
  



places <- ggmap(kmp) +
  geom_curve(data = x, aes(x = lon_board, y = lat_board, xend = lon_depart, yend = lat_depart, color=n, linewidth=n),
             arrow = arrow(length = unit(0.02, "npc")), lineend="round", curvature=-0.6)+
  scale_color_met_c("Homer2")+
  geom_point(data=stations, x=stations$lon_depart, y=stations$lat_depart, size=6, shape=1)+
  geom_text(data=stations, x=stations$lon_depart, y=stations$lat_depart, 
            label=stations$where_depart, vjust = 2.2, nudge_x = 0, size=3, fontface = "bold")+
  theme(axis.title.x=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),
        axis.title.y=element_blank(),axis.line=element_blank(),
        legend.position="none",
        legend.title=element_blank())+
  coord_cartesian()+
  geom_sf(data=route, linewidth=0.8, mapping=aes(geometry=geometry), inherit.aes = FALSE)+
  coord_sf()


print(odmat2+places)

odtiles <- odmat2+places
ggsave("odmap2.jpg", odtiles, width=9)


placemen<- ggmap(kmp) +
  geom_curve(data = xmen, aes(x = lon_board, y = lat_board, xend = lon_depart, yend = lat_depart, color=n*2, linewidth=n*2),
             arrow = arrow(length = unit(0.02, "npc")), lineend="round", curvature=0.5)+
  scale_color_met_c("Homer2")+
  theme(axis.title.x=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),axis.line=element_blank(),
        legend.position="none",
        legend.title=element_blank())+
  ggtitle("Men - Origins and Destinations")+
  coord_cartesian()


placewmn<- ggmap(kmp) +
  geom_curve(data = xwmn, aes(x = lon_board, y = lat_board, xend = lon_depart, yend = lat_depart, color=n*2, linewidth=n*2),
             arrow = arrow(length = unit(0.02, "npc")), lineend="round", curvature=0.5)+
  scale_color_met_c("Homer2")+
  theme(axis.title.x=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),axis.line=element_blank(),
        legend.position="none",
        legend.title=element_blank())+
  coord_cartesian()+
  ggtitle("Women - Origins and Destinations")

odmaps <- print(placemen+placewmn)
ggsave("OD_gender_maps.jpg", odmaps)
matrix_map <- print(odmat2+places)

ggsave("matmap.jpg", matrix_map, width=10)


odmap_long <- onbus %>% 
  filter(board_num<99) %>% 
  count(gender, board_num, depart_num, lat_board, lon_board, lat_depart, lon_depart, sort = FALSE)%>% 
  filter(!(board_num == "02 - Namuwongo" & depart_num == "01 - Bukasa")) %>% 
  filter(!(depart_num == "02 - Namuwongo" & board_num == "01 - Bukasa")) %>%
  filter(!(board_num == "07 - Bukoto" & depart_num == "08 - Kisaasi")) %>% 
  filter(!(depart_num == "07 - Bukoto" & board_num == "08 - Kisaasi"))


x_far<- odmap_long %>% select(gender, lat_board,lon_board,lat_depart,lon_depart,n, Occupation) 
x_far[, -1] <- x_far[, -1] + rnorm(cumprod(dim(x_far[, -1]))[-1], sd = 0.00001)


places_far_wm <- x_far %>% filter(gender=="woman")
places_far_mn <- x_far %>% filter(gender=="man") 

wom_map <- ggmap(kmp) +
  geom_curve(data = places_far_wm, aes(x = lon_board, y = lat_board, xend = lon_depart, yend = lat_depart, color=n, linewidth=n),
             arrow = arrow(length = unit(0.02, "npc")), lineend="round", curvature=-0.6)+
  scale_color_met_c("Homer2")+
  theme(axis.title.x=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),axis.line=element_blank(),
        legend.position="none",
        legend.title=element_blank())+
  scale_size_continuous(range = c(-1, 10), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  coord_cartesian()

man_map <- ggmap(kmp) +
  geom_curve(data = places_far_mn, aes(x = lon_board, y = lat_board, xend = lon_depart, yend = lat_depart, color=n, linewidth=n),
             arrow = arrow(length = unit(0.02, "npc")), lineend="round", curvature=-0.6)+
  scale_color_met_c("Homer2")+
  theme(axis.title.x=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),axis.line=element_blank(),
        legend.position="none",
        legend.title=element_blank())+
  scale_size_continuous(range = c(-1, 10), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  coord_cartesian()

print(wom_map + man_map)

ggsave("odmap.jpg", places, height=8)

st2 <- bind_rows(stations, stations) 


far_map <- ggmap(kmp) +
  geom_curve(data = x_far, aes(x = lon_board, y = lat_board, xend = lon_depart, yend = lat_depart, color=n, linewidth=n),
             arrow = arrow(length = unit(0.02, "npc")), lineend="round", curvature=-0.6)+
  scale_color_met_c("Homer2")+
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.title.y=element_blank(),axis.line=element_blank(),
        legend.position="none",
        legend.title=element_blank())+
  geom_sf(data=route, linewidth=0.8, mapping=aes(geometry=geometry), inherit.aes = FALSE)+
  geom_point(data=st2, mapping=aes(x=lon_depart, y=lat_depart), size=6, shape=1)+
  geom_text(data=st2, mapping=aes(x=lon_depart, y=lat_depart, 
            label=where_depart), vjust = 2.2, nudge_x = 0, size=3, fontface = "bold")+
  coord_cartesian()+
  coord_sf()+
  facet_wrap(~gender)

print(far_map)

occmap_long <- onbus %>% 
  filter(board_num<99) %>% 
  count(occupation, board_num, depart_num, lat_board, lon_board, lat_depart, lon_depart, sort = FALSE)%>% 
  filter(occupation %in% c("Business", "Student", "Market vendor", "Casual work", "Nurse", "Teacher", "X", "Technician"))
  


o_far<- occmap_long %>% select(occupation, lat_board,lon_board,lat_depart,lon_depart,n) 
o_far[, -1] <- o_far[, -1] + rnorm(cumprod(dim(o_far[, -1]))[-1], sd = 0.00001)

occ_map <- ggmap(kmp) +
  geom_curve(data = o_far, aes(x = lon_board, y = lat_board, xend = lon_depart, yend = lat_depart, color=n, linewidth=n),
             arrow = arrow(length = unit(0.02, "npc")), lineend="round", curvature=-0.6)+
  scale_color_met_c("Homer2")+
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.title.y=element_blank(),axis.line=element_blank(),
        legend.position="none",
        legend.title=element_blank())+
  geom_sf(data=route, linewidth=0.8, mapping=aes(geometry=geometry), inherit.aes = FALSE)+
  geom_point(data=st2, mapping=aes(x=lon_depart, y=lat_depart), size=6, shape=1)+
  geom_text(data=st2, mapping=aes(x=lon_depart, y=lat_depart, 
                                  label=where_depart), vjust = 2.2, nudge_x = 0, size=3, fontface = "bold")+
  coord_cartesian()+
  coord_sf()+
  facet_wrap(~occupation, ncol=4)

print(occ_map)


purpose <- ggplot(data=onbus)+
  geom_bar(x=why_go, stat="identity")
print(purpose)

pax_hour <- ggplot(data=hours, aes(x=date, y=sum_board))+
  geom_bar(stat="identity", aes(fill=hour))
print(pax_hour)



time_pur <- ggplot(onbus) +
  geom_bar(aes(x=hour, y=trips, group=gender, fill=gender), position="dodge2")
print(time_pur)


#converting to longer table with each instance of former-mode choice in a single column
mds_rep <- onbus %>% pivot_longer(cols=starts_with("how_go_"), names_to="Replaced_Mode", values_to="Rep_Mode_Num",
                                  values_transform = list(Rep_Mode_Num = as.character))
mds_rep$Rep_Mode_Num <- mds_rep$Rep_Mode_Num %>% as.numeric()
mds_rep$Rep_Mode_Num [is.na(mds_rep$Rep_Mode_Num )] <- 0
mds_rep <- mds_rep %>% filter(Rep_Mode_Num==1)
mds_rep <- mds_rep %>% mutate(rep_mode_name = ifelse(Replaced_Mode == "how_go_boda", "Boda-Boda",
                                                     ifelse(Replaced_Mode == "how_go_car", "Private Car",
                                                            ifelse(Replaced_Mode == "how_go_minibuses", "Other Minibus(es)",
                                                                   ifelse(Replaced_Mode == "how_go_walk", "Walking",
                                                                          ifelse(Replaced_Mode == "how_go_would_not", "Would Not Make Trip",99))))))


                                                 

replace <- ggplot(mds_rep) +
  geom_bar(aes(x= rep_mode_name, y = after_stat(prop), group=gender, fill=gender), 
           linewidth=1, position="dodge2")+
  scale_fill_met_d("Java")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position=c(0.8, 0.8),
        legend.title=element_blank())+
  ggtitle("Previous to this route, \nhow would you make this trip?")+
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1))

print(replace)
 ggsave("replace.jpg", replace, height=8)

replace_wo_short <- mds_rep %>% 
  filter(!(board_num == "02 - Namuwongo" & depart_num == "01 - Bukasa")) %>% 
  filter(!(depart_num == "02 - Namuwongo" & board_num == "01 - Bukasa")) %>%
  filter(!(board_num == "07 - Bukoto" & depart_num == "08 - Kisaasi")) %>% 
  filter(!(depart_num == "08 - Kisaasi" & board_num == "07 - Bukoto"))

replace_only_preex <- mds_rep %>% 
  filter(board_num %in% c("02 - Namuwongo", "01 - Bukasa", "07 - Bukoto", "08 - Kisaasi")) %>%
  filter(depart_num %in% c("02 - Namuwongo", "01 - Bukasa", "07 - Bukoto", "08 - Kisaasi")) %>%
  filter(!(board_num == "07 - Bukoto" & depart_num == "01 - Bukasa")) %>% 
  filter(!(board_num == "07 - Bukoto" & depart_num == "02 - Namuwongo")) %>%
  filter(!(board_num == "08 - Kisaasi" & depart_num == "01 - Bukasa")) %>% 
  filter(!(board_num == "08 - Kisaasi" & depart_num == "02 - Namuwongo"))%>%
  filter(!(board_num == "01 - Bukasa" & depart_num == "08 - Kisaasi")) %>% 
  filter(!(board_num == "01 - Bukasa" & depart_num == "07 - Bukoto")) %>%
  filter(!(board_num == "02 - Namuwongo" & depart_num == "08 - Kisaasi")) %>% 
  filter(!(board_num == "02 - Namuwongo" & depart_num == "07 - Bukoto"))
  

replace_wo <- ggplot(replace_wo_short) +
  geom_bar(aes(x= rep_mode_name, y = after_stat(prop), group=gender, fill=gender), 
           linewidth=1, position="dodge2")+
  scale_fill_met_d("Degas")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="NONE",
        legend.title=element_blank())+
  labs(title="Previous to this route, how would you make this trip?",
       subtitle="only passengers making trips in new locations")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))

print(replace_wo)
ggsave("replace.jpg", replace_wo, height=6)

replace_only <- ggplot(replace_only_preex) +
  geom_bar(aes(x= rep_mode_name, y = after_stat(prop), group=gender, fill=gender), 
           linewidth=1, position="dodge2")+
  scale_fill_met_d("Java")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position=c(0.8, 0.8),
        legend.title=element_blank())+
  ggtitle("Previous to this route, \nhow would you make this trip?\n(only passengers making trips in OLD locations)")+
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1))

print(replace_only)

ggsave("replace.jpg", replace_wo, height=8, width=5)


print(replace_wo+replace)





why_rep <- onbus %>% pivot_longer(cols=starts_with("why_go_"), names_to="Why_Go", values_to="Why_Go_Num",
                                  values_transform = list(Why_Go_Num = as.character))
why_rep$Why_Go_Num <- why_rep$Why_Go_Num %>% as.numeric()
why_rep$Why_Go_Num [is.na(why_rep$Why_Go_Num )] <- 0
why_rep <- why_rep %>% filter(Why_Go_Num==1) 
why_rep <- why_rep %>% mutate(Why_Go = ifelse(Why_Go == "why_go_connects", "Connectivity",
                                              ifelse(Why_Go == "why_go_hassle", "Hassle",
                                                     ifelse(Why_Go == "why_go_other", "Other",
                                                            ifelse(Why_Go == "why_go_price", "Price",
                                                                   ifelse(Why_Go == "why_go_safety", "Safety",
                                                                          ifelse(Why_Go == "why_go_time", "Time", 99)))))))
                                                     

why_wo_short <- why_rep %>% 
  filter(!(board_num == "02 - Namuwongo" & depart_num == "01 - Bukasa")) %>% 
  filter(!(depart_num == "02 - Namuwongo" & board_num == "01 - Bukasa")) %>%
  filter(!(board_num == "07 - Bukoto" & depart_num == "08 - Kisaasi")) %>% 
  filter(!(depart_num == "08 - Kisaasi" & board_num == "07 - Bukoto"))


why_new <- ggplot(why_rep) +
  geom_bar(aes(x= Why_Go, y = after_stat(prop), group=gender, fill=gender), linewidth=1, position="dodge2")
print(why_new)


why_new_woshort <- ggplot(why_wo_short) +
  geom_bar(aes(x= Why_Go, y = after_stat(prop), group=gender, fill=gender), linewidth=1, position="dodge2")
print(why_new_woshort)

print(why_new_woshort+why_new)






#prices

price <- onbus %>% filter(pay>0)

priceod <- price %>% filter(board_num<99) %>% 
  count(board_num, depart_num, pay, sort = FALSE)  %>% 
  mutate(freq=n/sum(n)) %>%
  mutate(labs=freq*100) %>% mutate(labs=round(labs, 1)) %>% 
  mutate(labs2="%") %>% 
  mutate(name = paste(labs, labs2, sep = ""))%>%
  mutate()

price_plot<- ggplot(priceod) +
  geom_tile(aes(depart_num, board_num, fill=n))+
  geom_abline(intercept = 11, slope = -1, color="grey50")+
  geom_text(aes(depart_num, board_num, label = n), color="white", size=3, fontface="bold")+
  scale_fill_met_c("Lakota")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="top")+
  facet_wrap(~pay)+
  scale_x_discrete(limits = rev(unique(sort(od2$depart_num))))+
  labs(title="New Paratransit Route Passengers - Fare",
       x ="Departure Location", y = "Boarding Location")

print(price_plot)


price_sum <- ggplot()+
  geom_bar(data=price, mapping=aes(x=pay, y = after_stat(count), group=gender, fill=gender))+
  geom_vline(xintercept = mean(price$pay), color="blue")

print(price_sum)

price2 <- price %>% filter(board_num<99) %>% 
  select (board_num, depart_num, pay)  %>%  group_by(board_num, depart_num) %>% mutate(avpay = mean(pay))
 

price_plot2<- ggplot(price2) +
  geom_tile(aes(depart_num, board_num, fill=avpay))+
  geom_abline(intercept = 11, slope = -1, color="grey50")+
  geom_text(aes(depart_num, board_num, label = avpay), color="white", size=3, fontface="bold")+
  scale_fill_met_c("Homer2")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="top")+
  scale_x_discrete(limits = rev(unique(sort(od2$depart_num))))+
  scale_y_discrete(limits = unique(sort(od2$depart_num)))+
  labs(title="New Paratransit Route Passengers - Fare",
       x ="Departure Location", y = "Boarding Location")

print(price_plot2)


when <- onbus %>% select(when_first, date) %>% count(date, when_first)

when_first <- ggplot()+
  geom_point(when, mapping=aes(x=when_first, y = date, size=n))
  
print(when_first)


child <- select(onbus, gender, w_child) %>% mutate(sex=ifelse(gender=="woman", 0, 1))
childt <- table(child$sex, child$w_child)

prop.table(childt, 1)

aget <- select(onbus, gender, age) %>% filter (age>17) 

ages<- aget %>% group_by(gender) %>% summarize(ageave = mean(age))

prop.table(aget,1)

chi_cro <- crosstab(wchild, row.vars = "w_child", col.vars = "sex", type="r")
chi2 <- table(wchild$sex)



occ <- read.csv("occ_count.csv") #%>% filter(Occ!="X") %>% count(gender, Occ) %>% mutate(freq=n/sum(n))

occbar <- occ %>% mutate(occ2=ifelse())
occm <- occ %>% filter(gender=="man")
occw <- occ %>% filter(gender=="woman")


occs<- occ %>% summarize(occp=frequency(Occ))


occtreew <- ggplot(occw, aes(area = n, fill = Occ,
               label = paste(n, Occ, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")

print(occtreew)

occtreem <- ggplot(occm, aes(area = n, fill = Occ,
                            label = paste(n, Occ, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")

print(occtreem)

print(occtreem+occtreew)

occtree <- ggplot(occ, aes(area = n, fill = Occ, subgroup=gender,
                             label = paste(round(freq, 3)*100, Occ, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 12, 
                    fontface = "bold") +
  theme(legend.position = "none")+
  geom_treemap_subgroup_text(place = "centre", angle=40, grow = TRUE,
                             alpha = 0.15, colour = "black",
                             fontface = "italic") +
  scale_fill_manual(values = met.brewer("Degas", 83))+
  theme(text = element_text(family = "Times New Roman"))

print(occtree)

ggsave("occtree.jpg", occtree, width=18)

occ <- occ %>% mutate(name = fct_relevel(occ2, 
                                 "Business", "Market vendor", "Fruit and Vegetable Vendor", 
                                 "Casual work", "Nurse", "Teacher", "Technician", 
                                 "Student", "Not in Employment", "Other"))

occplot <- ggplot()+
  geom_bar(data=occ, aes(x=name, y = after_stat(count), group=gender, fill=gender), position="dodge2")+
  theme(axis.text.x = element_text(angle = 20, vjust =1, hjust=1),
        axis.title.x = element_blank(),
        legend.position=c(0.4, 0.8),
        legend.direction="horizontal")+
  scale_fill_met_d("Degas")+
  labs(title="Passenger Occupation")
  

print(occplot)
ggsave("occplot.jpg", occplot, height=8, width=5)



plots <- occplot/replace_wo


why_new_woshort <- ggplot(why_wo_short) +
  geom_bar(aes(x= Why_Go, y = after_stat(prop), group=gender, fill=gender), linewidth=1, position="dodge2")+
  theme(axis.text.x = element_text(angle = 20, vjust =1, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y= element_blank(),
        legend.position="NONE",
        legend.direction="horizontal")+
  scale_fill_met_d("Degas")+
  labs(title="Why did you choose this route?")
print(why_new_woshort)

replace_wo <- ggplot(replace_wo_short) +
  geom_bar(aes(x= rep_mode_name, y = after_stat(prop), group=gender, fill=gender), 
           linewidth=1, position="dodge2")+
  scale_fill_met_d("Degas")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="NONE",
        legend.title=element_blank())+
  labs(title="Previously, how would you make this trip?",
       subtitle="only passengers making trips in new locations")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))

print(replace_wo)
ggsave("replace.jpg", replace_wo, height=6)

replace_wo_nogender <- ggplot(replace_wo_short) +
  geom_bar(aes(x= rep_mode_name), 
           linewidth=1)+
  scale_fill_met_d("Degas")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="NONE",
        legend.title=element_blank())+
  labs(title="Previously, how would you make this trip?",
       subtitle="only passengers making trips in new locations")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1))

print(replace_wo_nogender)


ggsave("replace.jpg", replace_wo, height=6)


plots <- occplot/(replace_wo+why_new_woshort)
print(plots)
ggsave("plots.jpg", plots, width=9)



