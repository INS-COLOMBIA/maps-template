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

map_points <- map %>% group_by(Dept,Mun,population) %>% 
  dplyr::summarise(municipios= length(population))

map1 <- coordinates(map_choco)
map_point1 <- as.data.frame(map_points)
map_point2 <- as.data.frame(map1)
choco_points <- cbind(map_point1, map_point2)
colnames(choco_points)<- c("Dept","name", "population", "Code", "X", "Y")

mun_list <- unique(choco_points$name)
'%out%' <- function(x,y)!('%in%'(x,y))
#special_location <- c("Acandí","Alto Baudó","Atrato","Bagadó","Bahía Solano","Bajo Baudó",
#                      "Bojayá", "Carmen del Darién","Cértegui","Condoto","El Cantón del San Pablo",
#                      "El Carmen de Atrato","El Litoral del San Juan","Istmina","Juradó",
#                      "Lloró","Medio Atrato","Medio Baudó","Medio San Juan","Nóvita",
#                      "Nuquí","Quibdó","Río Iró","Río Quito","Riosucio","San José del Palmar",
#                      "Sipí","Tadó","Unguía","Unión Panamericana")

special_location <- c("Alto Baudó","Bagadó","Bahía Solano","Bajo Baudó",
                      "Bojayá", "Carmen del Darién","Condoto","El Cantón del San Pablo",
                      "El Carmen de Atrato","El Litoral del San Juan","Istmina",
                      "Lloró","Medio Atrato","Medio Baudó","Medio San Juan","Nóvita",
                      "Nuquí","Quibdó","Río Iró","Río Quito","Riosucio","San José del Palmar",
                      "Tadó","Unión Panamericana")

special_location <- c("Alto Baudó","Bagadó")

choco_points_ok <- choco_points %>% filter(name %out% special_location)
choco_points_nudge <- choco_points %>% 
  filter(name %in% special_location) %>% mutate(name = factor(name))
choco_points_nudge$name

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
  geom_text(data= choco_points_ok,
            aes(x=X, y=Y, label=name),
            color = "black",
            check_overlap = FALSE, size = size_text/2) +
  #geom_text_repel(data = choco_points_nudge,
  #                aes(x = X, y = Y, label = name),
  #                # fontface = "bold",
  #                nudge_x = c(-12, -20, -10, -10, 10, 8, -10, -10, 25, 12, 
  #                            10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
  #                            10, 10, 10, 10),
  #                nudge_y = c( -3, 6,-5,-1, 10, 6,-5,-3,-8, 2,
  #                             -5,8,-5, 10, 10, 10, 10, 10, 10, 10,
  #                             10, 10,10, 10),
  #                size = size_text/2) +
  #geom_text_repel(data = choco_points_nudge,
  #                aes(x = X, y = Y, label = name),
  #                nudge_x = c(-40,-40,-40,-40,-40,-40,-40,-40,-40,-40,
  #                            10,10,10,10,10,10,10,10,10,10,
  #                            -80,-80,-80,-80),
  #                nudge_y = c(-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,
  #                            8,8,8,8,8,8,8,8,8,8,
  #                            15,15.15,15),
  #                size = size_text/2) +
  geom_text_repel(data = choco_points_nudge,
                  aes(x = X, y = Y, label = name),
                  nudge_x = c( 28, -28),
                  nudge_y = c(0.5, 0.1),
                  size = size_text/2) +
  theme(legend.key.width=unit(2.8,"cm"),legend.key.height=unit(2.8,"cm"),legend.position = c(-0.1,.3)) +
  labs(title="Choco")

p1
#ggsave("maps/Choco.png", width=20, height=20)
ggsave("maps/Choco_names1.png", width=20, height=20)

