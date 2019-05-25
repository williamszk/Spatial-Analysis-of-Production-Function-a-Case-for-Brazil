#181125 bivan chp 3 visualizing spatial data

library(sp)
data(meuse)
coordinates(meuse) <- c("x", "y")
plot(meuse)
title("points")
class(meuse)


cc <- coordinates(meuse)
m.sl <- SpatialLines(list(Lines(list(Line(cc)), "line1")))
plot(m.sl)
class(m.sl)
title("lines")



data(meuse.riv)
class(meuse.riv)
head(meuse.riv)
class(Polygon(meuse.riv))
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)),"meuse.riv"))
meuse.pol <- SpatialPolygons(meuse.lst)
plot(meuse.pol, col = "blue")
title("polygons")


data(meuse.grid)
class(meuse.grid)
head(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
spplot(meuse.grid)
plot(meuse.grid)
meuse.grid <- as(meuse.grid, "SpatialPixels")
image(meuse.grid, col = "grey")
title("grid")

image(meuse.grid, col = "lightgrey")
plot(meuse.pol, col = "grey", add = TRUE)
plot(meuse, add = TRUE)

 layout(matrix(c(1, 2), 1, 2))
 plot(meuse.pol, axes = TRUE)
 plot(meuse.pol, axes = FALSE)
 axis(1, at = c(178000 + 0:2 * 2000), cex.axis = 0.7)
 axis(2, at = c(326000 + 0:3 * 4000), cex.axis = 0.7)
 box()

 oldpar = par(no.readonly = TRUE)
 layout(matrix(c(1, 2), 1, 2))
 plot(meuse, axes = TRUE, cex = 0.6)
 plot(meuse.pol, add = TRUE)
  title("Sample locations")
  par(mar = c(0, 0, 0, 0) + 0.1)
  plot(meuse, axes = FALSE, cex = 0.6)
   plot(meuse.pol, add = TRUE)
   box()
   par(oldpar)


plot(meuse)
plot(meuse.pol, add = TRUE)
plot(meuse)
SpatialPolygonsRescale(layout.scale.bar(), offset = locator(1),scale = 1000, fill = c("transparent", "black"), plot.grid = FALSE)
text(locator(1), "0")
text(locator(1), "1 km")
SpatialPolygonsRescale(layout.north.arrow(), offset = locator(1),scale = 400, plot.grid = FALSE)

library(maptools)
library(maps)
wrld <- map("world", interior = FALSE, xlim = c(-179,179), ylim = c(-89, 89), plot = FALSE)

wrld_p <- pruneMap(wrld, xlim = c(-179, 179))
> llCRS <- CRS("+proj=longlat +ellps=WGS84")
> wrld_sp <- map2SpatialLines(wrld_p, proj4string = llCRS)
> prj_new <- CRS("+proj=moll")
> library(rgdal)
> wrld_proj <- spTransform(wrld_sp, prj_new)
> wrld_grd <- gridlines(wrld_sp, easts = c(-179, seq(-150,
                                                     + 150, 50), 179.5), norths = seq(-75, 75, 15), ndiscr = 100)
> wrld_grd_proj <- spTransform(wrld_grd, prj_new)
> at_sp <- gridat(wrld_sp, easts = 0, norths = seq(-75,
                                                   + 75, 15), offset = 0.3)
> at_proj <- spTransform(at_sp, prj_new)
> plot(wrld_proj, col = "grey60")
> plot(wrld_grd_proj, add = TRUE, lty = 3, col = "grey70")
> text(coordinates(at_proj), pos = at_proj$pos, offset = at_proj$offset,
       + labels = parse(text = as.character(at_proj$labels)),
       + cex = 0.6)



