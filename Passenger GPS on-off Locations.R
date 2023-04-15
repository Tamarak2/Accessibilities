install.packages("basemaps")
install.packages("transformr")
install.packages("gganimate")
install.packages("recipes")
install.packages("dplyr")

library("dplyr")
library("tidyr")
library("ggplot2")
library(basemaps)
library(ggmap)
library(lubridate)
library(MetBrewer)
library(patchwork)
library(gganimate)
library(transformr)
library(recipes)
library(sf)
library(mapview)
library(tmap)



setwd("C:/Users/Tamara Kerzhner/Box Sync/Research/Kampala New Routes/Data/Analysis - GPS Counts")

kmp <- get_stamenmap(bbox=c(left=32.57, right=32.64, bottom=0.28, top=0.4),
                     maptype = "terrain", 
                     crop = TRUE,
                     zoom = 13)

my_colors <- met.brewer("Tam", 6)

#import table, add easy lat lon columns
tbl <- read.csv("GPS PAX Count2_WIDE(45).csv") %>% mutate(lat=location.Latitude) %>% mutate(lon=location.Longitude)

tbl$cleantime <- strptime(tbl$starttime,
                            format = "%b %d, %Y %I:%M:%S %p")

tbl <- tbl %>% mutate(hour=hour(cleantime))

tbl <- tbl %>% 
  filter (cleantime > "2023-02-06 00:00:00") %>% 
  mutate(hour=hour(cleantime)) %>%
  mutate(minute=minute(cleantime)) %>%
  mutate(date=as.factor(date(cleantime)))

tbl<- tbl %>% mutate(an_time = format(strptime(starttime,
                                               format = "%b %d, %Y %I:%M:%S %p"),'%H%M'))

z <- as.POSIXlt(tbl$an_time, tz = "", format,
           tryFormats = c("%Y-%m-%d %H:%M:%OS",
                          "%Y/%m/%d %H:%M:%OS",
                          "%Y-%m-%d %H:%M",
                          "%Y/%m/%d %H:%M",
                          "%Y-%m-%d",
                          "%Y/%m/%d",
                          "%H%M"),
           optional = FALSE)

  


tbl <- tbl %>% mutate(tenmin=ifelse(minute < 10, 00,
                                     ifelse(minute < 20, 10, 
                                     ifelse(minute < 30, 20,
                                     ifelse(minute < 40, 30, 
                                     ifelse(minute < 50, 40,
                                      ifelse(minute <60, 50, 99)))))))

tbl <- tbl %>% unite(hour_tenmin,c("hour","tenmin"))

#set passenger count columns
boardings <- c(tbl$num_women, tbl$num_child, tbl$num_men)

#replace NAs with 0

tbl <- tbl %>% 
  mutate_at(c('num_women','num_men', 'num_child','woman_dep','Men_dep','Child_dep'), ~replace_na(.,0))

#add sum of all boardings/deps per line
tbl <- tbl %>% mutate(sum_board=num_women+num_child+num_men) %>% mutate(sum_dep=woman_dep+Men_dep+Child_dep)
tbl <- tbl %>% mutate(all_wm=num_women+woman_dep) %>% mutate(all_men=Men_dep+num_men)

tblmap <- tbl %>% filter(is.na(lat)==FALSE)

write.csv(tblmap, "gpstable.csv")


tbl <- tbl %>% mutate(fades=as.integer(2))

map <- ggmap(kmp) +
  geom_point(data=tbl, aes(x=lon,y=lat, size=sum_board), color=my_colors[1], alpha=1)+
  geom_point(data=tbl,aes(x=lon, y=lat, size=sum_dep), color=my_colors[4], alpha=1)+
  scale_size_continuous(range = c(-1, 15), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  transition_events(start = hour,   ###
                    end = hour, ###
                    enter_length = fades,     ###
                    exit_length = fades)+
  enter_fade() +
  exit_fade()+
  labs(title = "{frame_time}:00")


animate(map, fps = 10, nframes = 200)

map_s <- ggmap(kmp) +
  geom_point(data=tbl,aes(x=lon, y=lat, size=sum_dep, color="Alighting"),  alpha=0.6)+
  geom_point(data=tbl, aes(x=lon,y=lat, size=sum_board, color="Boarding"), alpha=0.5)+
  scale_size_continuous(range = c(-1, 15), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_met_d("Java")+
  guides(size = "none", colour = "legend")+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position = c(0.8, 0.9),
        legend.title=element_blank(),
        legend.background = element_rect(fill="transparent",
                                         size=0.5, linetype="solid"),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.text = element_text(size=15))+
  guides(colour = guide_legend(override.aes = list(size=8)))#change legend text font size)

print(map_s)
ggsave("Map2.jpg",map_s)

##############
 
print(map)

ggsave("paxMap1.jpg", map)






wmn <- ggmap(kmp) +
  geom_point(data=tbl, aes(x=lon, y=lat, size=num_women),color=my_colors[1], alpha=0.5, position="jitter")+
  geom_point(data=tbl, aes(x=lon, y=lat, size=woman_dep),color=my_colors[2], alpha=0.5, position="jitter")+
  scale_size_continuous(range = c(-1, 17), breaks=c(1,2,3,4,5,6,7,8,9,10))+
  ggtitle("Women Boarding")+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none")
print(wmn)

men <- ggmap(kmp) +
  geom_point(data=tbl, aes(x=lon,y=lat, size=num_men),  color=my_colors[5], alpha=0.5, position="jitter")+
  geom_point(data=tbl, aes(x=lon, y=lat, size=Men_dep),color=my_colors[6], alpha=0.5, position="jitter")+
  scale_size_continuous(range = c(-1, 17), breaks=c(1,2,3,4,5,6,7,8,9,10))+
  ggtitle("Men Boarding")+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none")
print(men)

print(replace+wmn+men)

ggsave("Newroutemap1.jpg", replace+wmn+men)

ggsave("genderMap1.jpg", gender)

#check num of boardings in Bukasa-Namuwongo area vs rest of route
sum(tbl$sum_board)
bukasa <- tbl %>% filter(lat<0.31)
sum(bukasa$sum_board)
ratio <- (sum(bukasa$sum_board)/sum(tbl$sum_board))

#check ratio of boardings to departures by day
inc_dep <- tbl %>% filter(formdef_version == 2302101330 | formdef_version== 2302121537 | formdef_version == 2302081025)
bd <- (sum(inc_dep$sum_board)/sum(inc_dep$sum_dep))

days <- tbl %>% mutate(day = ifelse(starttime < "2023-02-06 00:00:00", 0,
                                ifelse(starttime < "2023-02-07 00:00:00", 1,
                                    ifelse(starttime < "2023-02-08 00:00:00", 2,
                                        ifelse(starttime < "2023-02-09 00:00:00", 3,
                                           ifelse(starttime < "2023-02-10 00:00:00", 4,
                                              ifelse(starttime < "2023-02-11 00:00:00", 5,
                                                 ifelse(starttime < "2023-02-13 00:00:00", 6,
                                                    ifelse(starttime < "2023-02-20 00:00:00", 7, 
                                                        ifelse(starttime < "2023-02-21 00:00:00", 8,
                                                             ifelse(starttime < "2023-02-22 00:00:00", 9,
                                                                 ifelse(starttime < "2023-02-23 00:00:00", 10,
                                                                        ifelse(starttime < "2023-02-24 00:00:00", 11,
                                                               99)))))))))))))






days <- days%>% select(sum_board, day)
days <- days %>% group_by(day)%>% summarise_all(sum)

pax_day <- ggplot(data=days, aes(x=day, y=sum_board))+
  geom_bar(stat="identity")

print(pax_day)




hours <- tbl%>% arrange(cleantime) %>% select(sum_board, sum_dep, cleantime, device_info) %>%
  mutate(hour=hour(cleantime)) %>% mutate(date=as.factor(date(cleantime))) %>% 
  filter (cleantime > "2023-02-06 00:00:00")


pax_off <- ggplot(data=hours)+
  geom_bar(stat="identity", aes(x=date, y=sum_board, fill=hour))+
  scale_fill_met_c("Tam")+
  geom_bar(stat="identity", aes(x=date, y=sum_dep), color="black")
print(pax_off)



pax_on <- ggplot(data=hours)+
  geom_bar(stat="identity", aes(x=date, y=sum_board), color="black")
print(pax_on)

print(pax_on + pax_off)







enum_by_day <- tbl %>% filter(deviceid != "272d9eaa891dbeb6") %>%
  count(date, deviceid, sum_board, sort = FALSE)  %>% 
  mutate(sum_sum=n*sum_board) %>%
  group_by(deviceid, date) %>% 
  summarise(sum_by_device = sum(sum_sum),
            .groups = 'drop')


enum_day <- ggplot(enum_by_day) +
  geom_tile(aes(deviceid, date,  fill=sum_by_device))+
  scale_fill_met_c("Tam")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(enum_day)

tbl <- tbl %>% mutate(datetime_pos=as.POSIXct(cleantime))  
brd_tbl <- tbl %>% filter(sum_board > 0)

enum_time <- ggplot(brd_tbl)+ 
  geom_point(aes(x=hour, y=date, size=sum_board, color=sum_board))+
  scale_size_continuous(range = c(1, 12))+
  #facet_wrap(~date, ncol=1, scales = "free_x")+
  scale_color_met_c("VanGogh2")

print(enum_time)
ggsave("enum_time.jpg", enum_time, height=25)




#calcuate distances between origins and destinations

stop_dists <- recipe(~., data = onbus) %>%
  update_role(name, new_role = "location") %>%
  step_geodist(
    lat = latitude, lon = longitude, log = FALSE,
    ref_lat = 38.8986312, ref_lon = -77.0062457,
    is_lat_lon = TRUE
  ) %>%
  prep(training = onbus)

bake(near_station, new_data = NULL) %>%
  arrange(geo_dist)




gps <- tbl %>% filter(lat>0)
