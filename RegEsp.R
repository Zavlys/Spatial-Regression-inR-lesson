install.packages(c("spdep", "maptools", "rgdal","sp"), dependencies=TRUE)

#Dados do IPEA (Malhas e dados)
#http://www.ipea.gov.br/ipeageo/index.html

setwd("/Users/odiralmeida/Documents/Minas")
library(rgdal)
library(maptools)
library(sp)
library(spdep)

#Lendo o shapefile no rgdal
Minas=readOGR(dsn="/Users/odiralmeida/Documents/Minas",layer="MG_Mun97_region")
plot(Minas)

#Lendo os dados (já ordenados)
dados=read.csv2("MG_Mun97_region.csv", sep=";", quote="", fileEncoding="latin1" , header=T)

#Adicionando os dados ao objeto que contém o shapefile (opcional)
Minas@data = cbind(Minas@data,dados)

#Testando a Regressão simples
x<- Minas@data$Öndice.de.Gini
y<- Minas@data$Taxa.de.fecundidade.total
plot(x,y)

Reg<- lm(y ~ x)
summary(Reg)
plot(x,y)
abline(lm(y ~ x), col= "red")

residuos<- Reg$residuals
plot(residuos)

#Encontrando vizinhos
nb <- poly2nb(Minas,queen=TRUE)
#Ponderação (soma das linhas= 1)
nbw <- nb2listw(nb,style="W")

imoran.lm = moran.mc(residuos, nbw, nsim = 999, alternative = "greater")
imoran.lm

#Modelos de Regressão Espacial
SAR <- lagsarlm(y ~ x , data = data.frame(cbind(x, y)) , nbw , method = "eigen")
summary(SAR)

SEM <- errorsarlm(y ~ x , data = data.frame(cbind(x, y)) , nbw , method = "eigen")
summary(SEM)

#Moran para os resíduos dos modelos
Minas@data$residuos_SAR = SAR$residuals
imoran.SAR = moran.mc(SAR$residuals, nbw, nsim = 999, alternative = "less")
imoran.SAR

