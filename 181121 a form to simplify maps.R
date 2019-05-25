# 181121 test of form of simplify maps

library(sp)
library(spdep)
library(maptools)
library(readxl)  
library(lmtest)
library(ggplot2)
library(geojsonio)
library(rmapshaper)
library(sf)


data(states)
head(states)
dim(states)
class(states)
states_json <- geojson_json(states, geometry = "polygon", group = "group")
class(states_json)
## For ease of illustration via plotting, we will convert to a `SpatialPolygonsDataFrame`:
states_sp <- geojson_sp(states_json)
class(states_sp)
## Plot the original
plot(states_sp)

## Now simplify using default parameters, then plot the simplified states
states_simp <- ms_simplify(states_sp)
class(states_simp)
plot(states_simp)

states_very_simp <- ms_simplify(states_sp, keep = 0.001)
plot(states_very_simp)

#Compare this to the output using rgeos::gSimplify, where overlaps and gaps are evident:
library(rgeos)
states_gsimp <- gSimplify(states_sp, tol = 1, topologyPreserve = TRUE)
plot(states_gsimp)




