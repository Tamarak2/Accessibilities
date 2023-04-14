

install.packages("sf")
install.packages("terra")
install.packages("tidyverse")
install.packages("exactextractr")
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")


library(sf)
library(terra)
library(spDataLarge)
library(tidyverse)

setwd("C:/Users/Tamara Kerzhner/Box Sync/Research/WB MENA Gender/3City HH Survey/Maps")

######################## Intersecting population and access for city-wide average metrics #####################




#import population raster, grid, and accessibility results. Attach grid to accessibility

Bpop <- rast("lbn_ppp_2020_constrained.tif")
Apop <- rast("jor_ppp_2020_constrained.tif") 
Cpop <- rast("egy_ppp_2020_constrained.tif")

Bhex <- st_read("Beirut_Employment_Hex.shp")
Ahex <- st_read("Amman_Employment_Hex.shp") %>% st_transform(crs = 4326)
Chex <- st_read("Cairo_Employment_Hex.shp")
`
B_acc_45 <- fread("Beirut_hexsm60_access_45.csv") %>% mutate(fifty=fifty*100) %>% mutate(ninety=ninety*100) %>% mutate(ten=ten*100)
B_acc_60 <- fread("Beirut_hexsm60_access_60.csv") %>% mutate(fifty=fifty*100) %>% mutate(ninety=ninety*100) %>% mutate(ten=ten*100)
A_acc_45 <- fread("Amman_hexsm60_access_45.csv") 
A_acc_60 <- fread("Amman_hexsm60_access_60.csv") 
C_acc_45 <- fread("Cairo_hexsm60_access_45.csv")
C_acc_60 <- fread("Cairo_hexsm60_access_60.csv") 


#import proximity  by grid:


#mask larger population raster to the accessibility grid vector

Bpop_crop <- Bpop %>% crop(Bhex)
Apop_crop <- Apop %>% crop(Ahex)
Cpop_crop <- Cpop %>% crop(Chex)

#create intersect of the vector and raster layers and extract the raster values (population) for polygon of the vector grid (accessibility). 

#insert right data sources:
pophex <- terra::extract(Cpop_crop, Chex, exact=TRUE) %>% mutate(poprt=egy_ppp_2020_constrained*fraction) 

hex <- Chex

acc_45 <- C_acc_45
acc_60 <- C_acc_60

#generic function taking those inputs:

popbyhex <- pophex %>% replace(is.na(.), 0) %>%
  group_by(ID) %>%
  summarise(hex_pop = sum(poprt))   #### creates a table of population per hex cell

pop_acc45 <- hex %>% mutate(ID=fid) %>% full_join(popbyhex, by="ID") %>% full_join(acc_45, by="grid_id") 
pop_acc60 <- hex %>% mutate(ID=fid) %>% full_join(popbyhex, by="ID") %>% full_join(acc_60, by="grid_id") 

Cpop_acc45 <- pop_acc45 %>% mutate(pop50=fifty*hex_pop) %>% mutate(pop10=ten*hex_pop) %>% mutate(pop90=ninety*hex_pop)
Cpop_acc60 <- pop_acc60 %>% mutate(pop50=fifty*hex_pop) %>% mutate(pop10=ten*hex_pop) %>% mutate(pop90=ninety*hex_pop)


#export

Cpop_acc45 <- Cpop_acc45 %>% mutate(geometry="Hello")
Cpop_acc60 <- Cpop_acc60 %>% mutate(geometry="Hello")

write.csv(Cpop_acc45, "CairoPopAcc_45.csv")
write.csv(Cpop_acc60, "CairoPopAcc_60.csv")


#run and export, name whatever: 

pop_acc_tbl <- data.frame(Cutoff = c("45 Minutes", "60 Minutes"),
            "Reliability Ten Percent" = c(mean(sum(pop_acc45$pop10)/sum(pop_acc45$hex_pop)), mean(sum(pop_acc60$pop10)/sum(pop_acc60$hex_pop))),
            "Reliability Fifty Percent" = c(mean(sum(pop_acc45$pop50)/sum(pop_acc45$hex_pop)), mean(sum(pop_acc60$pop50)/sum(pop_acc60$hex_pop))),
            "Reliability Ninety Percent" = c(mean(sum(pop_acc45$pop90)/sum(pop_acc45$hex_pop)), mean(sum(pop_acc60$pop90)/sum(pop_acc60$hex_pop)))
)

write.csv(pop_acc_tbl, "Amman Pop Weighted Access.csv")





#find average population weighted "proximity"

Aprox <- A_grid_prox_all %>% select(grid_id,runs_12_10,runs_12_20,runs_8_20,runs_8_10) 
Ahex_prox <- Ahex %>% mutate(ID=fid) %>% full_join(Apopbyhex, by="ID") %>% full_join(Aprox, by="grid_id") 
Ahex_prox <- Ahex_prox %>% mutate(norm8_10=runs_8_10/60) %>% mutate(norm12_10=runs_12_10/60) %>%mutate(norm8_20=runs_8_20/60) %>%mutate(norm12_20=runs_12_20/60)
Ahex_prox$norm8_10[Ahex_prox$norm8_10>1] <- 1
Ahex_prox$norm8_20[Ahex_prox$norm8_20>1] <- 1
Ahex_prox$norm12_10[Ahex_prox$norm12_10>1] <- 1
Ahex_prox$norm12_20[Ahex_prox$norm12_20>1] <- 1
Ahex_prox <- Ahex_prox %>% mutate(pop8_10=norm8_10*hex_pop) %>% 
                           mutate(pop8_20=norm8_20*hex_pop) %>% 
                            mutate(pop12_10=norm12_10*hex_pop) %>% 
                            mutate(pop12_20=norm12_20*hex_pop)
  



Apop_prox_tbl <- data.frame(Cutoff = c("10 Minutes", "20 Minutes"),
                           "At 8 AM" = c(mean(sum(Ahex_prox$pop8_10)/sum(Ahex_prox$hex_pop)), mean(sum(Ahex_prox$pop8_20)/sum(Ahex_prox$hex_pop))),
                           "At 12 Noon" = c(mean(sum(Ahex_prox$pop12_10)/sum(Ahex_prox$hex_pop)), mean(sum(Ahex_prox$pop12_20)/sum(Ahex_prox$hex_pop)))
                      )

write.csv(Apop_prox_tbl, "Amman Pop Weighted Proximity Index.csv")






Bprox <- B_grid_prox_all %>% select(grid_id,runs_12_10,runs_12_20,runs_8_20,runs_8_10) 
Bhex_prox <- Bhex %>% mutate(ID=fid) %>% full_join(Bpopbyhex, by="ID") %>% full_join(Bprox, by="grid_id") 
Bhex_prox <- Bhex_prox %>% mutate(norm8_10=runs_8_10/60) %>% mutate(norm12_10=runs_12_10/60) %>%mutate(norm8_20=runs_8_20/60) %>%mutate(norm12_20=runs_12_20/60)
Bhex_prox$norm8_10[Bhex_prox$norm8_10>1] <- 1
Bhex_prox$norm8_20[Bhex_prox$norm8_20>1] <- 1
Bhex_prox$norm12_10[Bhex_prox$norm12_10>1] <- 1
Bhex_prox$norm12_20[Bhex_prox$norm12_20>1] <- 1
Bhex_prox <- Bhex_prox %>% mutate(pop8_10=norm8_10*hex_pop) %>% 
  mutate(pop8_20=norm8_20*hex_pop) %>% 
  mutate(pop12_10=norm12_10*hex_pop) %>% 
  mutate(pop12_20=norm12_20*hex_pop)




Bpop_prox_tbl <- data.frame(Cutoff = c("10 Minutes", "20 Minutes"),
                            "At 8 AM" = c(mean(sum(Bhex_prox$pop8_10)/sum(Bhex_prox$hex_pop)), mean(sum(Bhex_prox$pop8_20)/sum(Bhex_prox$hex_pop))),
                            "At 12 Noon" = c(mean(sum(Bhex_prox$pop12_10)/sum(Bhex_prox$hex_pop)), mean(sum(Bhex_prox$pop12_20)/sum(Bhex_prox$hex_pop)))
)

write.csv(Bpop_prox_tbl, "Beirut Pop Weighted Proximity Index.csv")




Cprox <- C_grid_prox_all %>% select(grid_id,runs_12_10,runs_12_20,runs_8_20,runs_8_10) 
Chex_prox <- Chex %>% mutate(ID=fid) %>% full_join(Cpopbyhex, by="ID") %>% full_join(Cprox, by="grid_id") 
Chex_prox <- Chex_prox %>% mutate(norm8_10=runs_8_10/60) %>% mutate(norm12_10=runs_12_10/60) %>%mutate(norm8_20=runs_8_20/60) %>%mutate(norm12_20=runs_12_20/60)
Chex_prox$norm8_10[Chex_prox$norm8_10>1] <- 1
Chex_prox$norm8_20[Chex_prox$norm8_20>1] <- 1
Chex_prox$norm12_10[Chex_prox$norm12_10>1] <- 1
Chex_prox$norm12_20[Chex_prox$norm12_20>1] <- 1
Chex_prox <- Chex_prox %>% mutate(pop8_10=norm8_10*hex_pop) %>% 
  mutate(pop8_20=norm8_20*hex_pop) %>% 
  mutate(pop12_10=norm12_10*hex_pop) %>% 
  mutate(pop12_20=norm12_20*hex_pop)




Cpop_prox_tbl <- data.frame(Cutoff = c("10 Minutes", "20 Minutes"),
                            "At 8 AM" = c(mean(sum(Chex_prox$pop8_10)/sum(Chex_prox$hex_pop)), mean(sum(Chex_prox$pop8_20)/sum(Chex_prox$hex_pop))),
                            "At 12 Noon" = c(mean(sum(Chex_prox$pop12_10)/sum(Chex_prox$hex_pop)), mean(sum(Chex_prox$pop12_20)/sum(Chex_prox$hex_pop)))
)

write.csv(Cpop_prox_tbl, "Cairo Pop Weighted Proximity Index.csv")




#Safety weighted by population
C_hex_safe <- Chex %>% mutate(ID=fid) %>% full_join(Cpopbyhex, by="ID") %>% full_join(C_safe_sumd, by="grid_id") %>% mutate(pop_safe=safe_index*hex_pop)
A_hex_safe <- Ahex %>% mutate(ID=fid) %>% full_join(Apopbyhex, by="ID") %>% full_join(A_safe_sumd, by="grid_id") %>% mutate(pop_safe=safe_index*hex_pop)
B_hex_safe <- Bhex %>% mutate(ID=fid) %>% full_join(Bpopbyhex, by="ID") %>% full_join(B_safe_sumd, by="grid_id") %>% mutate(pop_safe=safe_index*hex_pop)


AGS <-mean(sum(A_hex_safe$pop_safe)/sum(A_hex_safe$hex_pop))
BGS <-mean(sum(B_hex_safe$pop_safe)/sum(B_hex_safe$hex_pop)) 
CGS <-mean(sum(C_hex_safe$pop_safe)/sum(C_hex_safe$hex_pop)) 

Safety_grids <- data.frame(City = c("Amman","Beirut","Cairo"),
                           "pop weighted average safety" = c(AGS, BGS, CGS))
                                                                                                                           
                                                        

write.csv(Safety_grids, "Safety pop Weighted Mean - 3 cities.csv")
