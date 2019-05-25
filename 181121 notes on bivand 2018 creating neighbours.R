# 181121 bivand 2018 creating neighbours

library(rgdal)
library(maptools)
library(spdep)


NY8 <- readOGR(system.file("shapes/NY8_utm18.shp", package = "spData"))
plot(NY8)


NY_nb <- read.gal(system.file("weights/NY_nb.gal", package = "spData"),
                  region.id = row.names(NY8))

summary(NY_nb)

mydata <- slot(NY8,'data')
head(mydata)

names(mydata)
table(mydata$AREANAME)

Syracuse <- NY8[NY8$AREANAME == "Syracuse city", ]
slot(Syracuse,'data')
plot(Syracuse)
Sy0_nb <- subset(NY_nb, NY8$AREANAME == "Syracuse city")
summary(Sy0_nb)

class(Syracuse)
Sy1_nb <- poly2nb(Syracuse)

#poly2nb(pl, row.names = NULL, snap = sqrt(.Machine$double.eps),
#queen=TRUE, useC=TRUE, foundInBox=NULL)
#snap=boundary points less than snap distance apart are considered to 
#indicate contiguity
#we can use this on the maps and see how this changes the results







