setwd("C:/Users/Anais/Onedrive/Documents/UAF/Research")
library(readxl)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(outliers)
#run a specfic program for each tab of excel, for loop 
Blank_Data<- read_excel("Total Raw Data.xlsx", sheet = "Blanks")
Temps <- c(-1,4,11,17)
PlateNum <- c(1:4)
P.Temp.Combo <- expand.grid(Temps = Temps,PlateNum = PlateNum)
B_Data<- list()
B_Data_Names <- list()

#--------------------------Isolating Blank Data to subtract from Curve Data ------------------------------------
#Average out a specific part of the Blank Data to use as a blank to subtract from the data
SubB_Avg_RangeR <- function(Temp,PlateNum,rowS,rowE){
  name <- paste("Blank.",Temp,"C.P",PlateNum,".Range",rowS,"-",rowE,sep = "")
  subsetB <-subset.data.frame(Blank_Data,Blank_Data$Temperature == Temp & Blank_Data$Plate == PlateNum)
  subsetB1 <- subsetB[rowS:rowE,]
  subsetB1$Date <- NULL
  subsetB1$Time <- NULL
  subsetB1$Plate <- NULL
  subsetB1$Temperature <- NULL
  subsetB1$Average <- NULL
  assign(name,subsetB1,envir = .GlobalEnv)
}

SubB_AllP.Temp <- function(Temp,PlateNum){
  name <- paste("Blank.",Temp,"C.P",PlateNum,"ALL",sep = "")
  subsetB <-subset.data.frame(Blank_Data,Blank_Data$Temperature == Temp & Blank_Data$Plate == PlateNum)
  subsetB$Date <- NULL
  subsetB$Time <- NULL
  subsetB$Average <- NULL
  subsetB$Plate <- NULL
  subsetB$Temperature <- NULL
  subsetB$TimeTaken <- NULL
  AllValues <- stack(subsetB)
  AllValues$ind <- NULL
  names(AllValues) <- name
  assign(name,AllValues,envir = .GlobalEnv)
}

SubB_Avg_RangeR(4,2,9,17)
DataCount1 <- nrow(`Blank.4C.P2.Range9-17`)*(ncol(`Blank.4C.P2.Range9-17`)-1)
TotalSum1 <- sum(colSums(`Blank.4C.P2.Range9-17`[,-1])) # sum up each columns except the first
B4C.P2.Mod.TotalAverage <- TotalSum1/DataCount1

SubB_Avg_RangeR(4,3,8,16)
`Blank.4C.P3.Range8-16` <- `Blank.4C.P3.Range8-16`[-c(5),] # remove the 5th row
DataCount2 <- nrow(`Blank.4C.P3.Range8-16`)*(ncol(`Blank.4C.P3.Range8-16`)-1)
TotalSum2 <- sum(colSums(`Blank.4C.P3.Range8-16`[,-1]))
B4C.P3.Mod.TotalAverage <- TotalSum2/DataCount2

rm(DataCount1,DataCount2,TotalSum1,TotalSum2,`Blank.4C.P2.Range9-17`,`Blank.4C.P3.Range8-16`)
#----------Remove specific outliers from the Data and average out all of the times stamps to get one average--------

#For loop that creates a list of all of the data frames for all P and Temps with all Data in 
# one column for boxplots
for (i in 1:nrow(P.Temp.Combo)) {
  dataList <- SubB_AllP.Temp(P.Temp.Combo$Temps[i],P.Temp.Combo$PlateNum[i])
  dataName <- paste("All OD600 at ",P.Temp.Combo$Temps[i],"C for Plate",P.Temp.Combo$PlateNum[i],sep = "")
  B_Data[[i]] <- dataList
  B_Data_Names[[i]] <- dataName
}
rm(`Blank.-1C.P1ALL`,`Blank.-1C.P2ALL`,`Blank.-1C.P3ALL`,`Blank.-1C.P4ALL`,Blank.11C.P1ALL,Blank.11C.P2ALL,Blank.11C.P3ALL,Blank.11C.P4ALL,Blank.17C.P1ALL,Blank.17C.P2ALL,Blank.17C.P3ALL,Blank.17C.P4ALL,Blank.4C.P1ALL,Blank.4C.P2ALL,Blank.4C.P3ALL,Blank.4C.P4ALL)

# instead of creating a graph with all the box plots on it to remove the outliers, create a for loop that run through each dataframe, 
# identifies the outliers and removes them and creates a new list with those that removed the outliers
UpDated_B_Data <- list()
for (i in 1:length(B_Data)) {
  Outliers <- boxplot(B_Data[[i]][[1]], plot = FALSE)$out
  if (length(Outliers) > 0) {
    Data <- B_Data[[i]][-which(B_Data[[i]][[1]] %in% Outliers),]
    UpDated_B_Data[[i]] <- as.data.frame(Data)
    colnames(UpDated_B_Data[[i]]) <- colnames(B_Data[[i]])
  } else {
    UpDated_B_Data[[i]] <-B_Data[[i]]
    colnames(UpDated_B_Data[[i]]) <- colnames(B_Data[[i]])
  }
}
rm(Data,dataName,Outliers)

# creates a for loop that runs through the new list of data without outliers and calculate the average and save it as a new variable
All_Means <- list()
for (i in 1:length(UpDated_B_Data)) {
  Calc <- mean(UpDated_B_Data[[i]][[1]],na.rm = TRUE)
  new_Name <- paste(names(UpDated_B_Data[[i]]),"Average")
  All_Means[[i]] <- assign(new_Name,Calc)
}
Abs.Blank_4C_P4 <- `Blank.4C.P4ALL Average`
Abs.Blank_11C_P1 <- `Blank.11C.P1ALL Average`
Abs.Blank_11C_P2 <- `Blank.11C.P2ALL Average`
Abs.Blank_11C_P3 <- `Blank.11C.P3ALL Average`
Abs.Blank_11C_P4 <- `Blank.11C.P4ALL Average`
Abs.Blank_17C_P1 <- `Blank.17C.P1ALL Average`
Abs.Blank_17C_P2 <- `Blank.17C.P2ALL Average`
Abs.Blank_17C_P3 <- `Blank.17C.P3ALL Average`
Abs.Blank_17C_P4 <- `Blank.17C.P4ALL Average`
rm(`Blank.-1C.P1ALL Average`,`Blank.-1C.P4ALL Average`,`Blank.4C.P1ALL Average`,`Blank.-1C.P2ALL Average`,`Blank.-1C.P3ALL Average`,new_Name,Calc)
rm(`Blank.11C.P1ALL Average`,`Blank.11C.P2ALL Average`,`Blank.11C.P3ALL Average`,`Blank.11C.P4ALL Average`,`Blank.17C.P1ALL Average`,`Blank.17C.P2ALL Average`,`Blank.17C.P3ALL Average`,`Blank.17C.P4ALL Average`)
rm(`Blank.4C.P2ALL Average`,`Blank.4C.P3ALL Average`,`Blank.4C.P4ALL Average`,dataList)






