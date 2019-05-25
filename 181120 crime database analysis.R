#181120 crime database analysis

library(sp)
library(spdep)
library(maptools)
library(readxl)  
library(lmtest)

#There is a database from the ministry of justice 
#that contain the ocurrances of crimes in municipalities in Brazil.
#the objective of this script is to explore this data set

##cleaning data of 2017#################
data_2017 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2017.xlsx")

#Registros de Boletins de OcorrÃªncias por munÃ­cipio e mÃªs - 1 semestre de 2017.					
#Fonte: SinespJC - MÃ³dulo PolÃ­cias Civis do Brasil. ExtraÃ§Ã£o em 25/01/2018.					
#Nota 1: As informaÃ§Ãµes apresentadas estÃ£o de acordo com o nÃ­vel de alimentaÃ§Ã£o e consolidaÃ§Ã£o por parte dos Estados					
#Nota 2: Por motivo de compactaÃ§Ã£o do arquivo, os campos "sem informaÃ§Ã£o" ou preenchidos com "0 (zero)" foram desconsiderados.					

class(data_2017)
head(data_2017)
#there are many variables that we are not going to use

names(data_2017)
names(data_2017) <- c("Região","Sigla UF","UF","Código IBGE Município","Município","Tipo Crime","Mês","date","qtd"  )
#the variable for ibge code of municipality have 9 digits, we should change it
#for 7 digits
cod_ibge <- as.character(as.data.frame(data_2017)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7))
head(cod_ibge)

#change "date" to date type variable
class(data_2017$date)
head(data_2017$date)
date <- as.Date(substr(as.character(data_2017$date),1,10),"%Y-%m-%d") 
class(date)
head(as.character(data_2017$date))

#build new data set only with variables of interest
data_2017 <- cbind(cod_ibge,date,data_2017[,c(6,9)]) 
names(data_2017)
#put better names
names(data_2017)
head(data_2017)


##cleaning data of 2016#################
data_2016 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2016.xlsx")
names(data_2016)
names(data_2016)[8] <- "date"
names(data_2016)

cod_ibge <- as.character(as.data.frame(data_2016)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
class(cod_ibge)
head(cod_ibge)
class(cod_ibge)

#change "date" to date type variable
names(data_2016)
class(data_2016$date)
head(data_2016$date)
date <- as.Date(substr(as.character(data_2016$date),1,10),"%Y-%m-%d") 
class(date)
head(date)

#build new data set only with variables of interest
data_2016 <- cbind(cod_ibge,date,data_2016[,c(6,9)]) 
names(data_2016)
#put better names
names(data_2016) <- c("cod_ibge","date","Tipo Crime","qtd")
names(data_2016)
head(data_2016)


##cleaning data of 2015#################
data_2015 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2015.xlsx")
names(data_2015)
names(data_2015)[8] <- "date"
names(data_2015)

cod_ibge <- as.character(as.data.frame(data_2015)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
class(cod_ibge)
head(cod_ibge)


#change "date" to date type variable
names(data_2015)
class(data_2015$date)
head(data_2015$date)
date <- as.Date(substr(as.character(data_2015$date),1,10),"%Y-%m-%d") 
class(date)
head(date)

#build new data set only with variables of interest
data_2015 <- cbind(cod_ibge,date,data_2015[,c(6,9)]) 
names(data_2015)
#put better names
names(data_2015) <- c("cod_ibge","date","Tipo Crime","qtd")
names(data_2015)
head(data_2015)


##cleaning data of 2014#################
data_2014 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2014.xlsx")
names(data_2014)
names(data_2014)[8] <- "date"
names(data_2014)

cod_ibge <- as.character(as.data.frame(data_2014)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
class(cod_ibge)
head(cod_ibge)


#change "date" to date type variable
names(data_2014)
class(data_2014$date)
head(data_2014$date)
date <- as.Date(substr(as.character(data_2014$date),1,10),"%Y-%m-%d") 
class(date)
head(date)

#build new data set only with variables of interest
data_2014 <- cbind(cod_ibge,date,data_2014[,c(6,9)]) 
names(data_2014)
#put better names
names(data_2014) <- c("cod_ibge","date","Tipo Crime","qtd")
names(data_2014)
head(data_2014)

##cleaning data of 2013#################
data_2013 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2013.xlsx")
names(data_2013)
names(data_2013)[8] <- "date"
names(data_2013)

cod_ibge <- as.character(as.data.frame(data_2013)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)


#change "date" to date type variable
names(data_2013)
class(data_2013$date)
head(data_2013$date)
date <- as.Date(substr(as.character(data_2013$date),1,10),"%Y-%m-%d") 
class(date)
head(date)

#build new data set only with variables of interest
data_2013 <- cbind(cod_ibge,date,data_2013[,c(6,9)]) 
names(data_2013)
#put better names
names(data_2013) <- c("cod_ibge","date","Tipo Crime","qtd")
names(data_2013)
head(data_2013)

###mergind datasets with full information ############
violence <- rbind(data_2017,data_2016,data_2015,data_2014,data_2013)
head(violence)
table(violence$date)
length(table(violence$date))
12*4+6 #2017 only the 1st semester
dim(violence)



##cleaning data of 2012#################
#Registros de Boletins de Ocorrências por munícipio e mês - 2012.
#Fonte: SinespJC - Módulo Polícias Civis do Brasil. Extração em 25/01/2018.
#Nota 1: As informações apresentadas estão de acordo com o nível de alimentação e consolidação por parte dos Estados, na base de dados do SinespJC, módulo Polícia Civil, na data da extração.
#Nota 2: Por motivo de compactação do arquivo, os campos "sem informação" ou preenchidos com "0 (zero)" foram desconsiderados.
data_2012 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2012.xlsx")
#in 2012 dataset all municipalities with less then 100 000 residents are sum together
#per month and per state
names(data_2012)
names(data_2012)[8] <- "date"
names(data_2012)[9] <- "qtd"
names(data_2012)

cod_ibge <- as.character(as.data.frame(data_2012)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2012[is.na(cod_ibge),]$qtd)/sum(data_2012$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)

##cleaning data of 2011#################
data_2011 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2011.xlsx")
names(data_2011)
names(data_2011)[8] <- "date"
names(data_2011)[9] <- "qtd"
names(data_2011)

cod_ibge <- as.character(as.data.frame(data_2011)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2011[is.na(cod_ibge),]$qtd)/sum(data_2011$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)

#we cannot use data on crime in 2011 because 20% of the cases are summed in
#less then 100 000 residents



##cleaning data of 2010#################
data_2010 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2010.xlsx")
names(data_2010)
names(data_2010)[8] <- "date"
names(data_2010)[9] <- "qtd"
names(data_2010)

cod_ibge <- as.character(as.data.frame(data_2010)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2010[is.na(cod_ibge),]$qtd)/sum(data_2010$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)

#in 2010 also there is around 20% of cases are in less then 100 000  residents
















##cleaning data of 2009#################
data_2009 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2009.xlsx")
names(data_2009)
names(data_2009)[8] <- "date"
names(data_2009)[9] <- "qtd"
names(data_2009)

cod_ibge <- as.character(as.data.frame(data_2009)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2009[is.na(cod_ibge),]$qtd)/sum(data_2009$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)


##cleaning data of 2008#################
data_2008 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2008.xlsx")
names(data_2008)
names(data_2008)[8] <- "date"
names(data_2008)[9] <- "qtd"
names(data_2008)

cod_ibge <- as.character(as.data.frame(data_2008)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2008[is.na(cod_ibge),]$qtd)/sum(data_2008$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)

##cleaning data of 2007#################
data_2007 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2007.xlsx")
names(data_2007)
names(data_2007)[8] <- "date"
names(data_2007)[9] <- "qtd"
names(data_2007)

cod_ibge <- as.character(as.data.frame(data_2007)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2007[is.na(cod_ibge),]$qtd)/sum(data_2007$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)

##cleaning data of 2006#################
data_2006 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2006.xlsx")
names(data_2006)
names(data_2006)[8] <- "date"
names(data_2006)[9] <- "qtd"
names(data_2006)

cod_ibge <- as.character(as.data.frame(data_2006)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2006[is.na(cod_ibge),]$qtd)/sum(data_2006$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)

##cleaning data of 2005#################
data_2005 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2005.xlsx")
names(data_2005)
names(data_2005)[8] <- "date"
names(data_2005)[9] <- "qtd"
names(data_2005)

cod_ibge <- as.character(as.data.frame(data_2005)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2005[is.na(cod_ibge),]$qtd)/sum(data_2005$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)


##cleaning data of 2004#################
data_2004 <- read_excel("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC/ocorrenciasmun-brasil2004.xlsx")
names(data_2004)
names(data_2004)[8] <- "date"
names(data_2004)[9] <- "qtd"
names(data_2004)

cod_ibge <- as.character(as.data.frame(data_2004)[,4]) 
head(cod_ibge)
cod_ibge <- as.integer(substr(cod_ibge,1,7)) 
sum(data_2004[is.na(cod_ibge),]$qtd)/sum(data_2004$qtd)
sum(is.na(cod_ibge))
class(cod_ibge)
head(cod_ibge)
