setwd("C:/Users/Anais/OneDrive/Documents/UAF/Research")
source("Blank Data Subtraction.R")
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(readxl)
library(growthrates)

#--------------------- Testing with the Melt Data Test ( BRX8.1_Melt)---------------------
### Plot the data for each temperature ( example)
xyplot(value ~ `Time Difference`|as.factor(Temperature), data = Melt_Data_List[[1]], scales = "free")

### split the data set based on temperature
Data_Names <- list("BRX8.1","BRX8.2","BRX8.3","BRX8.4","BRX8.5","BRX8.6","BRX8.7","BRX8.8","BRX8.9","BRX9.1","BRX10.1","BRX10.2","BRX10.3","BRX10.4","BRX10.5","BRX10.6","BRX10.7","BRX10.8","BRX10.9")
for (i in 1:length(`Data _ Mod_All`)){
  for (j in 1:length(Data_Names)) {
    Dname <- paste(Data_Names[i])
  }
listA <- list(subset.data.frame(`Data _ Mod_All`[[i]],`Data _ Mod_All`[[i]]$Temperature == 4),subset.data.frame(`Data _ Mod_All`[[i]],`Data _ Mod_All`[[i]]$Temperature == 11),subset.data.frame(`Data _ Mod_All`[[i]],`Data _ Mod_All`[[i]]$Temperature == 17))
assign(Dname,listA,envir = .GlobalEnv)
}
BRX8.1[[1]] <- NULL
BRX8.2[[1]] <- NULL
BRX8.3[[1]] <- NULL
BRX8.4[[1]] <- NULL
BRX8.5[[1]] <- NULL
BRX8.6[[1]] <- NULL

Temp_Split <- list(BRX8.1,BRX8.2,BRX8.3,BRX8.4,BRX8.5,BRX8.6,BRX8.7,BRX8.8,BRX8.9,BRX9.1,BRX10.1,BRX10.2,BRX10.3,BRX10.4,BRX10.5,BRX10.6,BRX10.7,BRX10.8,BRX10.9)
rm(BRX8.1,BRX8.2,BRX8.3,BRX8.4,BRX8.5,BRX8.6,BRX8.7,BRX8.8,BRX8.9,BRX9.1,BRX10.1,BRX10.2,BRX10.3,BRX10.4,BRX10.5,BRX10.6,BRX10.7,BRX10.8,BRX10.9)
rm(B_Data,B_Data_Names,`BRX8.1 _Mod_ALL`,BRX8.1_Melt,listA,Melt_Data_List,Melt_Temp_Split,Mod_Raw_Sheet_Data,Raw_Sheet_Data,UpDated_B_Data,x)
rm(Abs.Blank_11C_P1,Abs.Blank_11C_P2,Abs.Blank_11C_P3,Abs.Blank_11C_P4,Abs.Blank_17C_P1,Abs.Blank_17C_P2,Abs.Blank_17C_P3,Abs.Blank_17C_P4,Abs.Blank_4C_P4,B4C.P2.Mod.TotalAverage,B4C.P3.Mod.TotalAverage,Dname,i,j,k,name,Raw_Sheet_List,y)
Temp_Split[[7]][[3]][1,9] <- rowMeans(Temp_Split[[7]][[3]][1,2:7])
Temp_Split[[18]][[3]][1,9] <- rowMeans(Temp_Split[[18]][[3]][1,2:7])

### Fit the line to each temperature based on hours lapsed and solve for the coefficients
### ---Test: Clean up the data to graph it and get the coefficients -------------------------
CalcGrowth <- function(data,ColNum){
  line1 <- fit_easylinear(data$`Time Difference`,data[,ColNum])
  fit1 <- as.numeric(rsquared(line1))
  GR1 <- coef(line1)[3]
  GR1 <- as.numeric(GR1)
  
  line2 <- fit_spline(data$`Time Difference`,data[,ColNum])
  fit2 <- rsquared(line2)
  GR2 <- coef(line2)[2]
  GR2 <- as.numeric(GR2)
  
  if(fit1 > fit2){
    x[[ColNum]] <- GR1
  } else{
    x[[ColNum]] <- GR2
  }
}

### --- All Data: Apply the graphs to all the data for all the temperatures and the replicates
### Creates a list of growth rates( one for each replicate, 8) for each strain and temperature
for (k in 1:length(Temp_Split)) {
  for (i in 1:length(Temp_Split[[k]])) {
    Temperature <- Temp_Split[[k]][[i]][[1,1]]
    Name <- paste(Data_Names[[k]],".",Temperature)
    Temp_Split[[k]][[i]]$Temperature <- NULL
    Data1 <- as.data.frame(Temp_Split[[k]][[i]])
    Temp_Split[[k]][[i]] <- Data1
    Temp_Split[[k]][[i]]$`Time Difference` <- as.numeric(Temp_Split[[k]][[i]]$`Time Difference`)
    Temp_Split[[k]][[i]][1,9] <- 0.1
    Temp_Split[[k]][[i]][,1:8] <- apply(Temp_Split[[k]][[i]][,1:8],c(1,2),abs)
    x <- list()
    for (j in 1:8) {
      y <- CalcGrowth(Temp_Split[[k]][[i]],j)
      x[[j]] <- y
    }
    assign(Name,x)
  }
}

BRX8.1GR <- do.call(rbind,Map(data.frame,'11' = `BRX8.1 . 11`,'17' = `BRX8.1 . 17`))
BRX8.1GR <- t(BRX8.1GR)
BRX8.1GR <- as.data.frame(BRX8.1GR)
row.names(BRX8.1GR) <- c("11","17")
BRX8.1GR <- rownames_to_column(BRX8.1GR,"Temperature")
BRX8.1GR$Temperature <- as.numeric(BRX8.1GR$Temperature) 
BRX8.1GR$Strain <- 'BRX8.1'
rm(`BRX8.1 . 11`,`BRX8.1 . 17`)

BRX8.2GR <- do.call(rbind,Map(data.frame,"11C" = `BRX8.2 . 11`,"17C" = `BRX8.2 . 17`))
BRX8.2GR <- t(BRX8.2GR)
BRX8.2GR <- as.data.frame(BRX8.2GR)
row.names(BRX8.2GR) <- c("11","17")
BRX8.2GR <- rownames_to_column(BRX8.2GR,"Temperature")
BRX8.2GR$Temperature <- as.numeric(BRX8.2GR$Temperature) 
BRX8.2GR$Strain <- 'BRX8.2'
rm(`BRX8.2 . 11`,`BRX8.2 . 17`)

BRX8.3GR <- do.call(rbind,Map(data.frame,"11C" = `BRX8.3 . 11`,"17C" = `BRX8.3 . 17`))
BRX8.3GR <- t(BRX8.3GR)
BRX8.3GR <- as.data.frame(BRX8.3GR)
row.names(BRX8.3GR) <- c("11","17")
BRX8.3GR <- rownames_to_column(BRX8.3GR,"Temperature")
BRX8.3GR$Temperature <- as.numeric(BRX8.3GR$Temperature) 
BRX8.3GR$Strain <- 'BRX8.3'
rm(`BRX8.3 . 11`,`BRX8.3 . 17`)

BRX8.4GR <- do.call(rbind,Map(data.frame,"11C" = `BRX8.4 . 11`,"17C" = `BRX8.4 . 17`))
BRX8.4GR <- t(BRX8.4GR)
BRX8.4GR <- as.data.frame(BRX8.4GR)
row.names(BRX8.4GR) <- c("11","17")
BRX8.4GR <- rownames_to_column(BRX8.4GR,"Temperature")
BRX8.4GR$Temperature <- as.numeric(BRX8.4GR$Temperature) 
BRX8.4GR$Strain <- 'BRX8.4'
rm(`BRX8.4 . 11`,`BRX8.4 . 17`)

BRX8.5GR <- do.call(rbind,Map(data.frame,"11C" = `BRX8.5 . 11`,"17C" = `BRX8.5 . 17`))
BRX8.5GR <- t(BRX8.5GR)
BRX8.5GR <- as.data.frame(BRX8.5GR)
row.names(BRX8.5GR) <- c("11","17")
BRX8.5GR <- rownames_to_column(BRX8.5GR,"Temperature")
BRX8.5GR$Temperature <- as.numeric(BRX8.5GR$Temperature) 
BRX8.5GR$Strain <- 'BRX8.5'
rm(`BRX8.5 . 11`,`BRX8.5 . 17`)

BRX8.6GR <- do.call(rbind,Map(data.frame,"11C" = `BRX8.6 . 11`,"17C" = `BRX8.6 . 17`))
BRX8.6GR <- t(BRX8.6GR)
BRX8.6GR <- as.data.frame(BRX8.6GR)
row.names(BRX8.6GR) <- c("11","17")
BRX8.6GR <- rownames_to_column(BRX8.6GR,"Temperature")
BRX8.6GR$Temperature <- as.numeric(BRX8.6GR$Temperature) 
BRX8.6GR$Strain <- 'BRX8.6'
rm(`BRX8.6 . 11`,`BRX8.6 . 17`)

BRX8.7GR <- do.call(rbind,Map(data.frame,"4C" = `BRX8.7 . 4`,"11C" = `BRX8.7 . 11`,"17C" = `BRX8.7 . 17`))
BRX8.7GR <- t(BRX8.7GR)
BRX8.7GR <- as.data.frame(BRX8.7GR)
row.names(BRX8.7GR) <- c("4","11","17")
BRX8.7GR <- rownames_to_column(BRX8.7GR,"Temperature")
BRX8.7GR$Temperature <- as.numeric(BRX8.7GR$Temperature) 
BRX8.7GR$Strain <- 'BRX8.7'
rm(`BRX8.7 . 4`,`BRX8.7 . 11`,`BRX8.7 . 17`)

BRX8.8GR <- do.call(rbind,Map(data.frame,"4C" = `BRX8.8 . 4`,"11C" = `BRX8.8 . 11`,"17C" = `BRX8.8 . 17`))
BRX8.8GR <- t(BRX8.8GR)
BRX8.8GR <- as.data.frame(BRX8.8GR)
row.names(BRX8.8GR) <- c("4","11","17")
BRX8.8GR <- rownames_to_column(BRX8.8GR,"Temperature")
BRX8.8GR$Temperature <- as.numeric(BRX8.8GR$Temperature) 
BRX8.8GR$Strain <- 'BRX8.8'
rm(`BRX8.8 . 4`,`BRX8.8 . 11`,`BRX8.8 . 17`)

BRX8.9GR <- do.call(rbind,Map(data.frame,"4C" = `BRX8.9 . 4`,"11C" = `BRX8.9 . 11`,"17C" = `BRX8.9 . 17`))
BRX8.9GR <- t(BRX8.9GR)
BRX8.9GR <- as.data.frame(BRX8.9GR)
row.names(BRX8.9GR) <- c("4","11","17")
BRX8.9GR <- rownames_to_column(BRX8.9GR,"Temperature")
BRX8.9GR$Temperature <- as.numeric(BRX8.9GR$Temperature) 
BRX8.9GR$Strain <- 'BRX8.9'
rm(`BRX8.9 . 4`,`BRX8.9 . 11`,`BRX8.9 . 17`)

BRX9.1GR <- do.call(rbind,Map(data.frame,"4C" = `BRX9.1 . 4`,"11C" = `BRX9.1 . 11`,"17C" = `BRX9.1 . 17`))
BRX9.1GR <- t(BRX9.1GR)
BRX9.1GR <- as.data.frame(BRX9.1GR)
row.names(BRX9.1GR) <- c("4","11","17")
BRX9.1GR <- rownames_to_column(BRX9.1GR,"Temperature")
BRX9.1GR$Temperature <- as.numeric(BRX9.1GR$Temperature) 
BRX9.1GR$Strain <- 'BRX9.1'
rm(`BRX9.1 . 4`,`BRX9.1 . 11`,`BRX9.1 . 17`)

BRX10.1GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.1 . 4`,"11C" = `BRX10.1 . 11`,"17C" = `BRX10.1 . 17`))
BRX10.1GR <- t(BRX10.1GR)
BRX10.1GR <- as.data.frame(BRX10.1GR)
row.names(BRX10.1GR) <- c("4","11","17")
BRX10.1GR <- rownames_to_column(BRX10.1GR,"Temperature")
BRX10.1GR$Temperature <- as.numeric(BRX10.1GR$Temperature) 
BRX10.1GR$Strain <- 'BRX10.1'
rm(`BRX10.1 . 4`,`BRX10.1 . 11`,`BRX10.1 . 17`)

BRX10.2GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.2 . 4`,"11C" = `BRX10.2 . 11`,"17C" = `BRX10.2 . 17`))
BRX10.2GR <- t(BRX10.2GR)
BRX10.2GR <- as.data.frame(BRX10.2GR)
row.names(BRX10.2GR) <- c("4","11","17")
BRX10.2GR <- rownames_to_column(BRX10.2GR,"Temperature")
BRX10.2GR$Temperature <- as.numeric(BRX10.2GR$Temperature) 
BRX10.2GR$Strain <- 'BRX10.2'
rm(`BRX10.2 . 4`,`BRX10.2 . 11`,`BRX10.2 . 17`)

BRX10.3GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.3 . 4`,"11C" = `BRX10.3 . 11`,"17C" = `BRX10.3 . 17`))
BRX10.3GR <- t(BRX10.3GR)
BRX10.3GR <- as.data.frame(BRX10.3GR)
row.names(BRX10.3GR) <- c("4","11","17")
BRX10.3GR <- rownames_to_column(BRX10.3GR,"Temperature")
BRX10.3GR$Temperature <- as.numeric(BRX10.3GR$Temperature) 
BRX10.3GR$Strain <- 'BRX10.3'
rm(`BRX10.3 . 4`,`BRX10.3 . 11`,`BRX10.3 . 17`)

BRX10.4GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.4 . 4`,"11C" = `BRX10.4 . 11`,"17C" = `BRX10.4 . 17`))
BRX10.4GR <- t(BRX10.4GR)
BRX10.4GR <- as.data.frame(BRX10.4GR)
row.names(BRX10.4GR) <- c("4","11","17")
BRX10.4GR <- rownames_to_column(BRX10.4GR,"Temperature")
BRX10.4GR$Temperature <- as.numeric(BRX10.4GR$Temperature) 
BRX10.4GR$Strain <- 'BRX10.4'
rm(`BRX10.4 . 4`,`BRX10.4 . 11`,`BRX10.4 . 17`)

BRX10.5GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.5 . 4`,"11C" = `BRX10.5 . 11`,"17C" = `BRX10.5 . 17`))
BRX10.5GR <- t(BRX10.5GR)
BRX10.5GR <- as.data.frame(BRX10.5GR)
row.names(BRX10.5GR) <- c("4","11","17")
BRX10.5GR <- rownames_to_column(BRX10.5GR,"Temperature")
BRX10.5GR$Temperature <- as.numeric(BRX10.5GR$Temperature) 
BRX10.5GR$Strain <- 'BRX10.5'
rm(`BRX10.5 . 4`,`BRX10.5 . 11`,`BRX10.5 . 17`)

BRX10.6GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.6 . 4`,"11C" = `BRX10.6 . 11`,"17C" = `BRX10.6 . 17`))
BRX10.6GR <- t(BRX10.6GR)
BRX10.6GR <- as.data.frame(BRX10.6GR)
row.names(BRX10.6GR) <- c("4","11","17")
BRX10.6GR <- rownames_to_column(BRX10.6GR,"Temperature")
BRX10.6GR$Temperature <- as.numeric(BRX10.6GR$Temperature) 
BRX10.6GR$Strain <- 'BRX10.6'
rm(`BRX10.6 . 4`,`BRX10.6 . 11`,`BRX10.6 . 17`)

BRX10.7GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.7 . 4`,"11C" = `BRX10.7 . 11`,"17C" = `BRX10.7 . 17`))
BRX10.7GR <- t(BRX10.7GR)
BRX10.7GR <- as.data.frame(BRX10.7GR)
row.names(BRX10.7GR) <- c("4","11","17")
BRX10.7GR <- rownames_to_column(BRX10.7GR,"Temperature")
BRX10.7GR$Temperature <- as.numeric(BRX10.7GR$Temperature) 
BRX10.7GR$Strain <- 'BRX10.7'
rm(`BRX10.7 . 4`,`BRX10.7 . 11`,`BRX10.7 . 17`)

BRX10.8GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.8 . 4`,"11C" = `BRX10.8 . 11`,"17C" = `BRX10.8 . 17`))
BRX10.8GR <- t(BRX10.8GR)
BRX10.8GR <- as.data.frame(BRX10.8GR)
row.names(BRX10.8GR) <- c("4","11","17")
BRX10.8GR <- rownames_to_column(BRX10.8GR,"Temperature")
BRX10.8GR$Temperature <- as.numeric(BRX10.8GR$Temperature) 
BRX10.8GR$Strain <- 'BRX10.8'
rm(`BRX10.8 . 4`,`BRX10.8 . 11`,`BRX10.8 . 17`)

BRX10.9GR <- do.call(rbind,Map(data.frame,"4C" = `BRX10.9 . 4`,"11C" = `BRX10.9 . 11`,"17C" = `BRX10.9 . 17`))
BRX10.9GR <- t(BRX10.9GR)
BRX10.9GR <- as.data.frame(BRX10.9GR)
row.names(BRX10.9GR) <- c("4","11","17")
BRX10.9GR <- rownames_to_column(BRX10.9GR,"Temperature")
BRX10.9GR$Temperature <- as.numeric(BRX10.9GR$Temperature) 
BRX10.9GR$Strain <- 'BRX10.9'
rm(`BRX10.9 . 4`,`BRX10.9 . 11`,`BRX10.9 . 17`)


GrowthRates_Data <- rbind.data.frame(BRX8.1GR,BRX8.2GR,BRX8.3GR,BRX8.4GR,BRX8.5GR,BRX8.6GR,BRX8.7GR,BRX8.8GR,BRX8.9GR,BRX9.1GR,BRX10.1GR,BRX10.2GR,BRX10.3GR,BRX10.4GR,BRX10.5GR,BRX10.6GR,BRX10.7GR,BRX10.8GR,BRX10.9GR)
rm(BRX8.1GR,BRX8.2GR,BRX8.3GR,BRX8.4GR,BRX8.5GR,BRX8.6GR,BRX8.7GR,BRX8.8GR,BRX8.9GR,BRX9.1GR,BRX10.1GR,BRX10.2GR,BRX10.3GR,BRX10.4GR,BRX10.5GR,BRX10.6GR,BRX10.7GR,BRX10.8GR,BRX10.9GR)
rm(x, Test_Melt,Data1,i,j,k,y)
colnames(GrowthRates_Data) <- c("Temperature","GR1","GR2","GR3","GR4","GR5","GR6","GR7","GR8","Strain")
GrowthRates_Data <- GrowthRates_Data[c("Strain","Temperature","GR1","GR2","GR3","GR4","GR5","GR6","GR7","GR8")]
write.csv(GrowthRates_Data,file = "Chesapeake Bay - Colwellia Growth Rates.csv")




