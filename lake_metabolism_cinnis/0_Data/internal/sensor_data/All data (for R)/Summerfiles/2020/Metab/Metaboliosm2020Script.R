##### 2020 Metabolism #####
##
## J Vanderwall
##
#
rm(list=ls())
#
library(ggplot2)
library(dplyr)
library(wesanderson)

##
#
pat <- "/Users/josep/Desktop/Summerfiles/2020/Metab/"
setwd(pat)

uph <- read.csv(file = "Upper Holland_metab_raw.txt", header = T)
uph$X <- NULL
uph$datetime <- strptime(uph$datetime, format = '%Y-%m-%d')


hol <- read.csv(file ='Holland_metab_raw.txt')
sap <- read.csv(file = 'Sapphire_metab_raw.txt')
mcd <- read.csv(file = 'McDonald_metab_raw.txt')
lng <- read.csv(file = 'Long_metab_raw.txt')
sum <- read.csv(file = 'Summit_metab_raw.txt')

uph$lake <- 'UPH'
hol$lake <- "HOL"
sap$lake <- "SAP"
mcd$lake <- "MCD"
lng$lake <- "LNG"
sum$lake <- "SUM"

metab <- rbind(uph,hol,sap,mcd,lng,sum)
metab$X <- NULL

metab$datetime <- strptime(metab$datetime, format = '%Y-%m-%d')

mean(uph$NEP)
mean(hol$NEP)
mean(sap$NEP)

mean(mcd$NEP)
mean(lng$NEP)
mean(sum$NEP)

ggplot(data = metab, aes(x = as.POSIXct(datetime), y = NEP)) +
  geom_point() +
  geom_point(aes(x=as.POSIXct(datetime), y=-abs(R)), col='red')+
  geom_point(aes(x=as.POSIXct(datetime), y=abs(GPP)), col='green')+
  ylim(-2.5,2.5)+
  facet_wrap(~lake) +
  theme_classic()
