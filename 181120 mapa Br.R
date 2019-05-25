

#181120 mapa Br

library(sp)
library(spdep)
library(maptools)
library(readxl)  
library(lmtest)
library(ggplot2)
library(ape)
library(ggmap)
library(plyr)
library(rgdal)
library(gridExtra)
library(grid)

setwd("C:/Users/willi/Desktop/working/Projects/RAW_DATA")

#import data of municipalities' gdp
pib <- read_excel("ipeadata_munic_pib_2000.xlsx")
pib <- as.data.frame(pib[,c(2,4)]) 
names(pib) <- c("cod_ibge","pib2000" ) #gdp 2000 prices, thousands
summary(pib$pib2000) #without log
pib$pib2000 <- log(pib$pib2000) #use log of pib
pib$cod_ibge <- as.character(pib$cod_ibge) 

#import data about human capital
hc <- read_excel("ipeadata_2000_capital_humano.xlsx")
hc <- as.data.frame(hc[,c(2,4)]) 
names(hc) <- c("cod_ibge","hc2000" )
summary(hc$hc2000) #without log
hc$hc2000 <- log(hc$hc2000) #log of human capital 

#import data about physical capital and create names for rows in data frame
pc <- read_excel("ipeadata_2000_capital_fisico.xlsx")
pc <- as.data.frame(pc) 
names(pc) <- c("UF","cod_ibge","nome","pc2000" )
summary(pc$pc2000) #without log
pc$pc2000 <- log(pc$pc2000)

#set back the working directory to project file
setwd("C:/Users/willi/Desktop/working/Projects/181120 trabalho espacial")

#merge of pib, hc, pc
mybase <- merge(pib, hc, by='cod_ibge', all=TRUE)
mybase <- merge(mybase, pc, by='cod_ibge', all=TRUE)

#build a variable for region
region <- substr(mybase[,1],1,1)
for (i in 1:dim(mybase)[1]) {
  if (region[i] == '1') {region[i] <- "Norte"}  
  if (region[i] == '2') {region[i] <- "Nordeste"}  
  if (region[i] == '3') {region[i] <- "Sudeste"}
  if (region[i] == '4') {region[i] <- "Sul"}
  if (region[i] == '5') {region[i] <- "Centro-Oeste"}  
}
mybase$region <- region

#load the spatial polygon data frame
#munic <- readShapePoly(
#  "C:/Users/willi/Desktop/working/Projects/RAW_DATA/map_brasil/RG2017_regioesgeograficas2017",
#  proj4string = CRS("+proj=longlat +ellps=WGS84"))
munic <- readShapePoly(
  "C:/Users/willi/Desktop/working/Projects/RAW_DATA/map_brasil/RG2017_regioesgeograficas2017",
  proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs"))

munic <- munic[,3] #use only the cod_ibge
names(munic) <- c('cod_ibge')

mybase <- merge(munic, mybase, by='cod_ibge') #merge the data with map

#delete some objects that will not be used
rm(hc,pc,pib,munic)

#chunk to drop the missings 
dim(mybase) #how many were
mybase <- mybase[!is.na(mybase$pib2000),]
mybase <- mybase[!is.na(mybase$pc2000),]
mybase <- mybase[!is.na(mybase$hc2000),]
dim(mybase) #how many rows after

#chunk for analysis of neighborhood matrices 
hold.nb.br <- poly2nb(mybase, queen=T)  #nb object for the whole country
summary(hold.nb.br) #let us look at  "Link number distribution"
#from the summary we know that there are 5 municipalities that are islands
#5 of them have 0 links

#we know that hold.nb.br is a list of vectors, the vectors tell the code
#of the neighbors, if a region have no neighbors then the only code
#that appears is 0

#chunk to find out which cities are islands, i.e. which do not have any neighbor
find.islands <-  #create this function to find which municipalities are islands
  function(nb.data){   #nb.data is of class "nb"
    n <- length(nb.data) #object to hold the length
    matrix.h1 <-  matrix(rep(0,2*n),ncol=2,nrow=n) #matrix to store information
    for (i in 1:n) {
      matrix.h1[i,1] <- i #build index column, to find municipality
      matrix.h1[i,2]<-sum(nb.data[[i]]) #sum the codes of neighbor regions
    }
    matrix.h2 <- as.data.frame(matrix.h1)
    matrix.h3 <- matrix.h2[order(matrix.h2[,2]),] #order according the sum of codes
    matrix.h3[1:10,] #give the 10 first municipalities, 5 of which are islands
}

find.islands(hold.nb.br) #use the function with "nb" of Brazil
#the output is
#       V1  V2
#57     57   0
#400   400   0
#2018 2018   0
#3293 3293   0
#3986 3986   0
#those are the municipalities that are islands
#drop those municipalities

#dropping municipalities that are islands
#reason: so that queen is usable

mybase <- mybase[-c(57,400,2018,3293,3986),] #drop the islands
#check if islands are gone
#summary(poly2nb(mybase, queen=T) ) #look in Link number distribution:
#there are no more regions with 0 neighbors

#chunk for visualization of data
#density graphs  
density.viz <-  #build this function with the objective to visualize 
  function(mybase){ #all three density plots in one graph
    df1 <- slot(mybase,'data')
    df_pib <- cbind(df1$pib2000,"log GDP")
    df_hc <- cbind(df1$hc2000,"log Human Capital")
    df_pc <- cbind(df1$pc2000,"log Physical Capital")
    df2 <- as.data.frame(rbind(df_pib,df_pc,df_hc))  
    df2$V1 <- as.numeric(df2$V1) 
    names(df2) <- c('value','type')
    ggplot(df2, aes(x=value, color=type, fill=type)) +
      geom_density(alpha=0.3) + theme(legend.position="top")+
      labs(x="log R$ of year 2000 ", y = "Density")
  }
density.viz(mybase)

#make maps of variables
#plot of maps of physical, human capital and gdp 
Brasil_uf <- readOGR(dsn="C:/Users/willi/Desktop/working/Projects/RAW_DATA/Brasil_nereus_uf")

h1 <- Brasil_uf
h1@data$id <- rownames(h1@data)
mapa.p <- fortify(h1,region = "id")

h1.df <- cbind(coordinates(mybase),mybase@data)
names(h1.df)[c(1,2)] <- c('long','lat')

gg1pib2000 <- #plot of log GDP of municipality
  ggplot() + 
  geom_point(data=h1.df, aes(y=lat, x=long, color=pib2000),size=1.3,alpha=0.5)+
  geom_polygon(data=mapa.p,aes(long,lat,group=group), fill = NA, color = "black") +
  coord_equal()+
  scale_colour_distiller(palette = "Spectral")
gg1pib2000

gg1pc2000 <- #plot log physical capital of municipalities
  ggplot() + 
  geom_point(data=h1.df, aes(y=lat, x=long, color=pc2000),size=1.3,alpha=0.5)+
  geom_polygon(data=mapa.p,aes(long,lat,group=group), fill = NA, color = "black") +
  coord_equal()+
  scale_colour_distiller(palette = "Spectral")
gg1pc2000


gg1hc2000 <- #plot log human capital of municipalities
  ggplot() + 
  geom_point(data=h1.df, aes(y=lat, x=long, color=hc2000),size=1.3,alpha=0.5)+
  geom_polygon(data=mapa.p,aes(long,lat,group=group), fill = NA, color = "black") +
  coord_equal()+
  scale_colour_distiller(palette = "Spectral")
gg1hc2000



#create ratio variables:
#the idea here is to visualize how output/capital is distributed through the country
attach(h1.df)
h1.df$ratio_pib.pc <- pib2000/pc2000
h1.df$ratio_pib.hc <- pib2000/hc2000
h1.df$ratio_pc.hc <- pc2000/hc2000
detach(h1.df)


gg1ratio_pib.pc <- #plot map ratio of GDP and physical capital
  ggplot() + 
  geom_point(data=h1.df, aes(y=lat, x=long, color=ratio_pib.pc),size=1.3,alpha=0.5)+
  geom_polygon(data=mapa.p,aes(long,lat,group=group), fill = NA, color = "black") +
  coord_equal()+
  scale_colour_distiller(palette = "Spectral")
gg1ratio_pib.pc


gg1ratio_pib.hc <- #plot map ratio of GDP and human capital
  ggplot() + 
  geom_point(data=h1.df, aes(y=lat, x=long, color=ratio_pib.hc),size=1.3,alpha=0.5)+
  geom_polygon(data=mapa.p,aes(long,lat,group=group), fill = NA, color = "black") +
  coord_equal()+
  scale_colour_distiller(palette = "Spectral")
gg1ratio_pib.hc


gg1ratio_pc.hc <- #plot map ratio of physical and human capital
  ggplot() + 
  geom_point(data=h1.df, aes(y=lat, x=long, color=ratio_pc.hc),size=1.3,alpha=0.5)+
  geom_polygon(data=mapa.p,aes(long,lat,group=group), fill = NA, color = "black") +
  coord_equal()+
  scale_colour_distiller(palette = "Spectral")
gg1ratio_pc.hc


#scatterplots

gg1.pib.hc <- #scatterplot x=human capita, y=gdp
  ggplot(data=slot(mybase,'data'), aes(hc2000, pib2000, colour = pc2000))+ geom_point()
gg1.pib.hc

gg1.pib.pc <- #scatterplot x=physical capita, y=gdp
  ggplot(data=slot(mybase,'data'), aes(pc2000, pib2000, colour = hc2000))+ geom_point()
gg1.pib.pc

gg1.hc.pc <- #scatterplot x=physical capita, y=human capital
  ggplot(data=slot(mybase,'data'), aes(pc2000, hc2000, colour = pib2000))+ geom_point()
gg1.hc.pc


#regions chosen to be analyzed: Brasil, São Paulo, Norte, Sul, Nordeste
#analsis for Brazil
basetemp <- mybase
m1 <- lm(pib2000 ~ -1 + pc2000 + hc2000, data=basetemp)
summary(m1)
coef(m1)[1]+coef(m1)[2]


#test for spatial dependence on redisuals
h1.nb <- poly2nb(basetemp, queen=T)  #using queen criterion
summary(h1.nb)
n.links.queen.br <-as.data.frame(  #number of links data frame
cbind(c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	23),
c(6,	87,	338,	926,	1219,	1181,	764,	468,	259,	122,	68,	34,	17,	8,	1,	1,	1,	1,	1)))
names(n.links.queen.br) <-c("n.links","n.munic")
gg1.n.links.queen.br <- #plot of relation number of links and quantity of municipalities
  ggplot()+ 
  geom_line(data=n.links.queen.br, aes(n.links, n.munic))+ 
  ggtitle("how many municipalities have n links")
gg1.n.links.queen.br
#we can see that the majority of municipalities gather around 4~7 links each. 
#hold.nb <- dnearneigh(coordinates(basetemp), 0, 100, longlat=T)
plot.nb(h1.nb,coordinates(basetemp))
#those are the links that are used in the weight matrices in the queen criterion, 
#it is hard to see any pattern besides the regions Norte and Centro-Oeste
W1<-nb2listw(h1.nb, glist=NULL, style ="W") #normlize weights to 1
W1$neighbours 
lm.morantest(m1, W1) #Moran's I test
#there is spatial dependence.


#Moran's I test for GDP 
moran.test(mybase@data$pib2000,W1)
#Moran's test for spatial autocorrelation using a spatial 
#weights matrix in weights list form. The assumptions underlying
#the test are sensitive to the form of the graph of neighbour 
#relationships and other factors, and results may be checked against 
#those of moran.mc permutations.


#plot of GDP and spatially lagged GDP 2
vec1 <- mybase@data$pib2000
W <- W1
#plot for GDP and spatially lagged GDP
scatter.plot.spatial.lag <-
  function(vec1,W){
    vec1lag <- lag.listw(W,vec1) 
    h2.df <- as.data.frame(cbind(vec1,vec1lag))  
    gg1.gdp.lagged.gdp <- #scatterplot x=vec, y=lagged vec1
      ggplot(data=h2.df, aes(y=vec1lag, x=vec1),size=1.3,alpha=0.5) + 
      geom_point()+
      coord_equal()+ 
      ggtitle("scatterplot x=GDP,y=lagged GDP ")+
      xlab("GDP") + ylab("lagged GDP")
    gg1.gdp.lagged.gdp
  }
scatter.plot.spatial.lag(mybase@data$pib2000,W1)


#plot of GDP and spatially lagged GDP
moran.plot(mybase@data$pib2000,W1,xlab = 'GDP',ylab = 'spatial lag GDP',type='p')

#a test for robustness of spatial dependence
moran.mc(mybase@data$pib2000,W1,nsim=1000)
#A permutation test for Moran's I statistic calculated by 
#using nsim random permutations of x for the given spatial weighting scheme, 
#to establish the rank of the observed statistic in relation to the nsim 
#simulated values.
moran.mc(mybase@data$pc2000,W1,nsim=1000)
moran.mc(mybase@data$hc2000,W1,nsim=1000)
moran.plot(mybase@data$pc2000,W1)
moran.plot(mybase@data$hc2000,W1)


#plot map of residuals of Brasil non spatial regression
e <- residuals(m1)
basetemp$residuals <- e
basetemp.df<- cbind(coordinates(basetemp), basetemp@data) 
names(basetemp.df)[c(1,2)] <- c('long','lat')
gg1.res.lm.br <- #plot map residual linear model of Brasil
  ggplot() + 
  geom_point(data=basetemp.df, aes(y=lat, x=long, color=residuals),size=1.3,alpha=0.5)+
  geom_polygon(data=mapa.p,aes(long,lat,group=group), fill = NA, color = "black") +
  coord_equal()+
  scale_colour_distiller(palette = "Spectral")
gg1.res.lm.br

#we can test if the residuals have different means depending on the region
mean(basetemp.df[basetemp.df$region=='Centro-Oeste',]$residuals)
mean(basetemp.df[basetemp.df$region=='Centro-Oeste',]$residuals)/
sd(basetemp.df[basetemp.df$region=='Centro-Oeste',]$residuals) 

mean(basetemp.df[basetemp.df$region=='Sul',]$residuals)
mean(basetemp.df[basetemp.df$region=='Sul',]$residuals)/
sd(basetemp.df[basetemp.df$region=='Sul',]$residuals)

mean(basetemp.df[basetemp.df$region=='Nordeste',]$residuals)
mean(basetemp.df[basetemp.df$region=='Nordeste',]$residuals)/
sd(basetemp.df[basetemp.df$region=='Nordeste',]$residuals)

mean(basetemp.df[basetemp.df$region=='Norte',]$residuals)
mean(basetemp.df[basetemp.df$region=='Norte',]$residuals)/
sd(basetemp.df[basetemp.df$region=='Norte',]$residuals)

mean(basetemp.df[basetemp.df$region=='Sudeste',]$residuals)
mean(basetemp.df[basetemp.df$region=='Sudeste',]$residuals)/
sd(basetemp.df[basetemp.df$region=='Sudeste',]$residuals)



table(basetemp.df$region)
sum(table(basetemp.df$region))
sum(is.na(e))
sum(is.na(basetemp.df[region=='Centro-Oeste',]$residuals))


#a analysis for São Paulo
basetemp <- mybase[mybase$UF=='SP',]

#regression without the intercept
m1 <- lm(pib2000 ~ -1 + pc2000 + hc2000, data=basetemp)
summary(m1)
coef(m1)[1]+coef(m1)[2]


hold.nb <- poly2nb(basetemp, queen=T) 
summary(hold.nb)

#hold.nb <- dnearneigh(coordinates(basetemp), 0, 100, longlat=T)
W1<-nb2listw(hold.nb, glist=NULL, style ="W")
plot.nb(hold.nb,coordinates(basetemp))
W1$neighbours
#it seems that distance weights is not a good idea for Brazil
#because there is a high level of spatial heterogeneity in municipalities.

lm.morantest(m1, W1)
#for some states there is a clear spatial pattern in the error term

#building the inverse distance matrix
#this is another way to find the Moran's I
c1 <- coordinates(basetemp)
dist_matrix <- as.matrix(dist(c1))
dist_inv <- 1/dist_matrix
diag(dist_inv) <- 0
dist_inv[1:5, 1:5]

Moran.I(e1, dist_inv)

#build function to estimate the spatial relation
#spatial regression production function 
#it is in the script "181122 function file nonlinear estimator.R"

source("181122 function file nonlinear estimator.R")
prod.function.sr1(basetemp,W1)








