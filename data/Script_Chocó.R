#install.packages("rgdal")

library(rgdal)
library(Rcpp)
library(devtools)
library(sp)
library(gstat)
library(readxl)
library(raster)
library(tidyverse)
library(maptools)
library(maps)
library(ggplot2)
library(mapproj)
library(ggspatial)
library(sf)
library(cowplot)
library(rgeos)
library(ggrepel)

rm(list = ls())

#-------------------------------------------------
#----------MAPA DEPARTAMENTO DE CHOCO-------------
#-------------------------------------------------

data_choco <- read_excel("data/Data_Choco.xlsx")
map_choco <- readOGR(dsn ="shapes", layer = "MGN_MPIO_POLITICO")

map <- fortify(map_choco, region = "MPIO_CCDGO")
theme_set(
  theme_bw() + 
    theme(legend.position = "bottom")
)

data_choco$id <- as.character(data_choco$id)
map <- inner_join(map,data_choco, by="id")


size_text <- 25
p1 <- ggplot() + 
  geom_map(data=map, map=map, 
           aes(x=long, y=lat, map_id=id, group=group, fill= population),  
           color="black", size=1, alpha = .8) + 
  geom_polygon(data=map,
               aes(long, lat, group = group), fill = NA, color = " black") +
  #scale_fill_continuous(low="green", high="red") +
  scale_fill_viridis_c(direction = -1, option = "E", na.value = "grey50", 
                       alpha = .8,
                       breaks = c(3220, 30000, 60000, 130825)) +
  coord_sf(xlim = c(-75.8, - 78.2), 
           ylim = c(3.5, 9), expand = FALSE) + 
  cowplot::theme_nothing(size_text) +
  theme(legend.key.width=unit(2.8,"cm"),legend.key.height=unit(2.8,"cm"),legend.position = c(-0.1,.3)) +
  labs(title="Choco") 



p1

ggsave("maps/Choco.png", width=20, height=20)

