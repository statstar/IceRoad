library(sf)
library(ggplot2)
library(rgdal)
library(raster)
library(dplyr)

road_sf <- read_sf("data/pilot/pilot.shp")
road <- readOGR("data/pilot/pilot.shp")
kor <- getData('GADM', country='KOR', level=1)

plot(kor)
lines(road, add=T, col="blue", lwd=2)

road2 <- fortify(road, by="ID")
road$ID %>% unique -> linkid

road2 %>% 
  mutate(id=as.numeric(id)) %>% 
  group_by(id) %>% 
  summarise(p1 = quantile(order, p=0.2) %>% as.integer, 
            p2 = quantile(order, p=0.4) %>% as.integer,
            p3 = quantile(order, p=0.6) %>% as.integer,
            p4 = quantile(order, p=0.8) %>% as.integer) %>% 
  arrange(id) -> road3

for(i in 1:520){
  road3$id[i] <- linkid[i]   
  road2[as.numeric(road2$id) ==i-1,"id"]=linkid[i]   
}

library(tidyr)
road3 %>% pivot_longer(2:5, values_to="order") -> road4

head(road2)
head(road4)

road4 %>% 
  inner_join(road2, by=c("id","order")) %>% 
  data.frame %>% 
  dplyr::select(id, order, long, lat)-> road5

road5 %>% dim


