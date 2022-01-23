# Estimating the infrastructure stock index
# Calderon(2015)
# Database: WDI, World Bank
# 
# Change working directory
setwd("/Users/ylov3/Desktop/Luke Li/Data/Imputed data")

# Loading
library("tidyverse")
library("haven")
library("FactoMineR")
library("factoextra")
library("readxl") # The readxl package comes with the function read_excel() to read xls and xlsx files
library("dplyr")
library("imputeTS")
library("ggplot2")
# =====================================================
#install.packages("imputeTS")
library("imputeTS")
# Reference: https://journal.r-project.org/archive/2017/RJ-2017-009/RJ-2017-009.pdf

WDI <- read_excel("Metadata_WDI.xlsx")
names(WDI)[2] <- 'Country'
names(WDI)[4] <- 'Indicator'
# Data cleaning
WDI_data0 <- reshape2::recast(WDI ,Country + variable ~ Indicator)
sapply(WDI_data0$variable,class)
WDI_data0$variable <- as.numeric(as.character(WDI_data0$variable))
WDI_data0 <- subset(WDI_data0, variable>=2001 & variable<2022)

asean10_isi <- subset(WDI_data0, 
                  Country == 'BRN'| Country == 'KHM' | Country == 'IDN' | Country == 'LAO' | Country == 'MYS' | Country == 'MMR' | Country == 'PHL' | Country == 'SGP' | Country == 'THA' | Country == 'VNM'
)
asean10_isi <- asean10_isi[,c('Country','variable','EG.USE.ELEC.KH.PC','IT.NET.BBND.P2','IS.RRS.PASG.KM')]
## Electric power sector
# electric power consumption - Electric power consumption (kWh per capita)


## Telecom sector
# the number of landline telephone per 100 persons (from 1990 to 2000)
# the number of broadband internet subscriptions per 100 persons (after 2001)

## Transportation sector
# railway length per thousand persons - Railways, passengers carried (million passenger-km)










# Impute Time-series data
for(i in unique(asean10_isi$Country)) # select unique value
  for(j in c('EG.USE.ELEC.KH.PC','IT.NET.BBND.P2','IS.RRS.PASG.KM'))
    if (0 %in% is.na(asean10_isi[asean10_isi$Country==i,][,j])) # a %in% b - determine if a is inside b or not
      asean10_isi[asean10_isi$Country==i,][,j]<- na_mean(asean10_isi[asean10_isi$Country==i,][,j]) # mean imputation by default

#### PCA ####
dim(data01) # 9x8
dim.c <- 8 # the number of component
dim.r <- 9 # the number of countries

## The 1st STAGE PCA ##
ave.data <- colMeans(asean10, na.rm = TRUE)
std.data <- apply(asean10, 2, sd) # 2 =  by column operator, 1 = by row operator
sd.asean10 <- asean10
for(i in 1:dim.c){
  sd.data01[,i] <- (data01[,i]-ave.data[i])/std.data[i]
}
dim(sd.data01) # 10 x 4

## The 2nd step
RR <- cor(sd.data01, method = "pearson")
dim(RR) # 8 x 8

sd.pca <- prcomp(RR, scale = TRUE) # sdev = eigenvalue, rotation = eigenvector

## The 3rd step
eigva <- sd.pca$sdev
eigvt <- sd.pca$rotation
dim(eigvt) # 8x8

#verify the condition: e'e = 1
for(i in 1:dim.c){
  print(sum(eigvt[,i]^2))
} 


## The 4th step
#PP <- rep(0, dim.c)
#for(i in 1:dim.c){
#  for(j in 1:dim.c){
#    PP[i] <- PP[i] + eigvt[j,i] * sd.data01[[i,j]]
#  }
#}

PP01 <- matrix(0, nrow = dim.r, ncol = dim.c)
for(i in 1:dim.r){
  for(j in 1:dim.c){
    PP01[i,j] <- PP01[i,j] + 
      eigvt[1,j] * sd.data01[[i,1]] + eigvt[2,j] * sd.data01[[i,2]] + eigvt[3,j] * sd.data01[[i,3]] +
      eigvt[4,j] * sd.data01[[i,4]] + eigvt[5,j] * sd.data01[[i,5]] + eigvt[6,j] * sd.data01[[i,6]] +
      eigvt[7,j] * sd.data01[[i,7]] + eigvt[8,j] * sd.data01[[i,8]]
  }
}

## The 5th step
HH <- rep(0, dim.r)

for(i in 1:dim.r){
  for(j in 1:dim.c){
    HH[i] <- HH[i] + eigva[j] * PP01[[i,j]]
  }
}

HDI <- HH / sum(eigva)
print("Singapore, Philippines, Thailand, Myanmar,  China,   Pakistan,   Bangladesh,  Brazil,   Poland")
print(HDI)