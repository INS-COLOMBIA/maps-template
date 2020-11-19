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
shp_choco <- readOGR(dsn ="shapes", layer = "MGN_MPIO_POLITICO")

map_data <- fortify(shp_choco, region = "MPIO_CCDGO")
data_choco$id <- as.character(data_choco$id)
map_data_merged <- inner_join(map_data,data_choco, by="id")


size_text <- 10

theme_set(theme_bw() + theme(legend.position = "bottom"))

p1 <- 
  ggplot() + 
  geom_map(data=map_data_merged, map=map_data_merged, 
           aes(x=long, y=lat, map_id=id, group=group, fill= population),  
           color="black", size=1, alpha = .8) + 
  geom_polygon(data=map,
               aes(long, lat, group = group), fill = NA, color = " black") +
  #scale_fill_continuous(low="green", high="red") +
  scale_fill_viridis_c(direction = -1, option = "E", na.value = "grey50",
                       alpha = .8,
                       breaks = c(3220, 30000, 60000, 130825)) +
  # coord_sf(xlim = c(-75.8, - 78.2), 
  #          ylim = c(3.5, 9), expand = FALSE) + 
  coord_cartesian(xlim = c(-75.8, - 78.2),
           ylim = c(3.5, 9), expand = TRUE) +
  cowplot::theme_nothing(size_text) +
  theme(legend.key.width=unit(2.8,"cm"),legend.key.height=unit(2.8,"cm"),legend.position = c(-0.1,.3)) +
  labs(title="Choco") 



p1

ggsave("maps/Choco2.png", width=15, height=20)





