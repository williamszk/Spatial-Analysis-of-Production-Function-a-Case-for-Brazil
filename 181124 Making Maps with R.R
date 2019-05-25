# 181124 Making Maps with R
#http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

library(ggmap)
library(maps)
library(ggplot2)
library(devtools)
library(dplyr)
library(stringr)
library(mapdata)
library(geojsonio)


usa <- map_data("usa")
dim(usa)
head(usa)
usa_sp <- geojson_sp(geojson_json(usa, geometry = "polygon", group = "group"))
plot(usa_sp)

france <- map_data('france')
france_json <- geojson_json(france, geometry = "polygon", group = "group")
france_sp <- geojson_sp(france_json)
plot(france_sp)
ggplot() + geom_polygon(data = france, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.4)



w2hr <- map_data("world2Hires")
#w2hr_sp <- geojson_sp(geojson_json(w2hr, geometry = "polygon", group = "group"))
#dim(w2hr)
#class(w2hr_sp)
class(w2hr)
head(w2hr) #this map is too big and it does not work properly, takes too much time
#ggplot() + geom_polygon(data = w2hr, aes(x=long, y = lat, group = group)) + 
#  coord_fixed(1.4)



usa <- map_data("usa") # we already did this, but we can do it again

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.5) #coord_fixed control the proportion in x and y

ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), 
               fill = NA, color = "red") + 
  coord_fixed(1.3)


#Here is violet fill, with a blue line.
gg1 <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), 
               fill = "violet", color = "blue") + 
  coord_fixed(1.3)
gg1


#Adding points to the map
labs <- data.frame(
  long = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("SWFSC-FED", "NWFSC"),
  stringsAsFactors = FALSE
)  

gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4)

gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5)

#See how important the group aesthetic is
ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat), fill = "violet", color = "blue") + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4) +
  coord_fixed(1.3)


#State maps
#We can also get a data frame of polygons that tell us above state boundaries:
states <- map_data("state")
dim(states)
head(states)

#Plot all the states, all colored a little differently

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend

#Plot just a subset of states in the contiguous 48:
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black") 

#Man that is ugly!!
ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

#Zoom in on California and look at counties
ca_df <- subset(states, region == "california")
head(ca_df)

#Now, let’s also get the county lines there
counties <- map_data("county")
head(counties)
ca_county <- subset(counties, region == "california")
head(ca_county)

#theme nothing
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base  #see the difference
ca_base + theme_nothing()

#Now plot the county boundaries in white:
ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

#Watch this regex fun:
# make a data frame
x <- readLines("ca-counties-wikipedia.txt")
pop_and_area <- str_match(x, 
  "^([a-zA-Z ]+)County\t.*\t([0-9,]{2,10})")[, -1] %>%
  na.omit() %>%
  str_replace_all(",", "") %>% 
  str_trim() %>%
  tolower() %>%
  as.data.frame(stringsAsFactors = FALSE)
pop_and_area

# give names and make population and area numeric
names(pop_and_area) <- c("subregion", "population", "area")
pop_and_area$population <- as.numeric(pop_and_area$population)
pop_and_area$area <- as.numeric(pop_and_area$area)

head(pop_and_area)
#>   subregion population area
#> 1   alameda    1578891  738
#> 2    alpine       1159  739
#> 3    amador      36519  593
#> 4     butte     222090 1640
#> 5 calaveras      44515 1020
#> 6    colusa      21358 1151


##########################


