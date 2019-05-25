#181125 Introdução aos mapas em R

#https://rstudio-pubs-static.s3.amazonaws.com/176768_ec7fb4801e3a4772886d61e65885fbdd.html

library(maps) #mapas simples, eixos, escala, cidades 
library(mapdata) #base de dados WorldHires e rios
library(rworldmap) #outra base de dados de mapas do mundo
library(maptools) #Ler ESRI shapefiles 
library(mapproj) #Projeções e grids
library(ggmap) #Gmaps, OSM + mapas baseados em ggplot2
library(rgdal)

library(maps)
par(mar=c(1,1,1,1))
map("worldHires","Brazil")

par(mar=c(1,1,1,1))
map("world","Brazil")
map.axes()

map("world","Brazil")
map.axes()
map.scale(ratio = F, cex = 0.7) #tentem ratio = T

map("world","Brazil", fill=T, col="grey90")
map(,,add=T)
map.axes()
map.scale(ratio=F, cex=0.7)
abline(h=0, lty = 2)
map.cities(country = "Brazil",minpop = 2000000,pch=19, cex=1.2)# pacote maps

m <- map("world","Brazil", fill=T, col="grey95")
map(,,add=T)
map.axes()
map.scale(ratio=F, cex=0.7)
abline(h=0, lty = 2)
map.grid(m, nx = 5, ny = 5, col="grey50", font=1, cex=0.7 , pretty = T)#library(mapproj)

#library(rgdal)
# maptools::readShapePoly
br <- readShapePoly("./shapefiles/brasil/BRA_adm_shp/BRA_adm0.shp") # 0: país, 1: estados, 2: municípios
estados1 <- readShapePoly("./shapefiles/brasil/BRA_adm_shp/BRA_adm1.shp") # 0: país, 1: estados, 2: municípios
cidades <- readShapePoly("./shapefiles/brasil/BRA_adm_shp/BRA_adm2")
biomas <- readShapePoly("./shapefiles/brasil/BR_BIOMAS_IBGE.shp")

# rgdal::readOGR
estados <- readOGR("./shapefiles/brasil/BRA_adm_shp/", "BRA_adm1")


