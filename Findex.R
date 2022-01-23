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

FAS <- read_excel("Metadata_FAS.xlsx")
# the number of deposit bank account per 1000 adult population
# the number of ATM per 100,000 people
# the number of bank outlets per 100,000 populations
# credit from a commercial bank (% of GDP)
# deposit from a commercial bank (% of GDP)
Dimension <- FAS[,c('ISO-3 code','Year','Number of commercial bank branches per 100,000 adults','Number of ATMs per 100,000 adults','Number of commercial bank branches per 100,000 adults','Outstanding deposits with commercial banks (% of GDP)','Outstanding loans from commercial banks (% of GDP)')]
Dimension <- Dimension[
  Dimension$Year==2011|Dimension$Year==2014|Dimension$Year==2017,
  ]
# Attention! Kosovo has no ISO-3 code in Meatadata_FAS.xlsx

# rename
colnames(Dimension) <- c('Country','Year','D1','D2i','D2ii','D3i','D3ii')

# add 4th dimension
WB <- read_excel('Global Findex Database.xlsx') # sources: https://globalfindex.worldbank.org/
D4 <- WB[,c('...2','...1','Used the internet to pay bills or to buy something online in the past year (% age 15+)','Paid utility bills: using a mobile phone (% age 15+)','Made or received digital payments in the past year (% age 15+)')]
colnames(D4) <- c('Country','Year', 'D4i', 'D4ii','D4iii')

# used the internet to pay bills or to buy something online in the past year (% age 15+)
# paid utility bills using a mobile phone (% age 15+)
# made or received digital payments in the past year (% age 15+)

# Merging data, add D4 to Dimension
merged_data <- merge(Dimension,D4,all.x=TRUE)
# Countries of concern

asean10 <- subset(merged_data, 
                  Country == 'BRN'| Country == 'KHM' | Country == 'IDN' | Country == 'LAO' | Country == 'MYS' | Country == 'MMR' | Country == 'PHL' | Country == 'SGP' | Country == 'THA' | Country == 'VNM'
                  )
asean5 <- asean10[!(asean10$Country == 'BRN'| asean10$Country == 'KHM'| asean10$Country == 'LAO'| asean10$Country == 'MMR'| asean10$Country == 'VNM'),]

# Impute Time-series data
for(i in unique(asean10$Country)) # select unique value
  for(j in c('D4i','D4ii','D4iii'))
    if (0 %in% is.na(asean10[asean10$Country==i,][,j])) # a %in% b - determine if a is inside b or not
      asean10[asean10$Country==i,][,j]<- na_mean(asean10[asean10$Country==i,][,j]) # mean imputation by default

########################### Finished ##########################







# ----------------------------- #
# Construct Findex - PCA method #
# ============================= #

### ------------------------------------------------------------- ###

# Estimated the latent variable IFI by using PCA, page 609 in       #
# (2005) Handbook of Applied Econometrics and Statistical Inference #
# July 21/22/23 2021

### ============================================================= ###

list2env(split(asean10,asean10$Year), envir = .GlobalEnv) #split the dataset into a list of datasets based on the value of iris$Species

############# PCA #############
dim(data01) # 9x8
dim.c <- 8 # the number of component
dim.r <- 9 # the number of countries - County BRN is neglected

## The 1st STAGE PCA ##
ave.data <- colMeans(data01, na.rm = TRUE)
std.data <- apply(data01, 2, sd) # 2 =  by column operator, 1 = by row operator
sd.data01 <- data01
for(i in 1:dim.c){
  sd.data01[,i] <- (data01[,i]-ave.data[i])/std.data[i]
}
dim(sd.data01) # 9 x 8

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




# Graphic summary
#ggplot(panel, aes(x = year, y = y, group = country,color = country)) +
  #geom_line() +
  #geom_point() +
  #scale_color_viridis_d() +
  #geom_vline(xintercept = 2013, linetype = "dashed")
##
# https://www.princeton.edu/~otorres/DID101R.pdf
