setwd("C:/Users/Anais/Onedrive/Documents/UAF/Research")
library(readxl)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
#run a specfic program for each tab of excel, for loop 
Raw_Sheet_data <- list()
Raw_Sheet_List <- excel_sheets("Total Raw Data.xlsx")
Blank_Data<- read_excel("Total Raw Data.xlsx", sheet = "Blanks")
# subtracting the blank average from every raw measurement and create a new spreadsheet with the corrected measurements
# with still all the data in the first 5 columns 

# if the temperature, the date, the time, and the plate number matches, subtract each measurement 
# with the average from the first tab

# --------------------------Average Blank--------------------------------------------
Subset_Blank1 <- function(Temp){
  name <- paste("Blank.",Temp,"C",sep = "")
  subsetB <-subset.data.frame(Blank_Data,Blank_Data$Temperature == Temp)
  subsetB$Date <- NULL
  subsetB$Time <- NULL
  assign(name,subsetB,envir = .GlobalEnv)
}

Subset_Blank1(-1)
ggplot(`Blank.-1C`,aes(`Time Taken`,Average, color = factor(Plate))) + 
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("red","green","blue", "orange")) +
  ggtitle("Blank Data for -1C") + 
  xlab("Time Taken") + 
  ylab("OD600")

Subset_Blank1(4)
ggplot(`Blank.4C`,aes(`Time Taken`,Average, color = factor(Plate))) + 
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("red","green","blue", "orange")) +
  ggtitle("Blank Data at 4C") + 
  xlab("Time Taken") + 
  ylab("OD600")

Subset_Blank1(11)
ggplot(`Blank.11C`,aes(`Time Taken`,Average, color = factor(Plate))) + 
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("red","green","blue", "orange")) +
  ggtitle("Blank Data at 11C") + 
  xlab("Time Taken") + 
  ylab("OD600")

Subset_Blank1(17)
ggplot(`Blank.17C`,aes(`Time Taken`,Average, color = factor(Plate))) + 
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("red","green","blue", "orange")) +
  ggtitle("Blank Data at 17C") + 
  xlab("Time Taken") + 
  ylab("OD600")
#--------------------------------------------------------------------------------------------------------

#---------------------------All Blank Data with curve ---------------------------------------------------
Subset_Blank2 <- function(Temp,PlateNum){
  name <- paste("Blank.",Temp,"C.P",PlateNum,sep = "")
  subsetB <-subset.data.frame(Blank_Data,Blank_Data$Temperature == Temp & Blank_Data$Plate == PlateNum)
  subsetB$Date <- NULL
  subsetB$Time <- NULL
  subsetB$Average <- NULL
  subsetB$Plate <- NULL
  subsetB$Temperature <- NULL
  assign(name,subsetB,envir = .GlobalEnv)
}
Subset_Blank2(-1,1)
Subset_Blank2(-1,2)
Subset_Blank2(-1,3)
Subset_Blank2(-1,4)
Subset_Blank2(4,1)
Subset_Blank2(4,2)
Subset_Blank2(4,3)
Subset_Blank2(4,4)
Subset_Blank2(11,1)
Subset_Blank2(11,2)
Subset_Blank2(11,3)
Subset_Blank2(11,4)
Subset_Blank2(17,1)
Subset_Blank2(17,2)
Subset_Blank2(17,3)
Subset_Blank2(17,4)
Blank_Data_List <- list(`Blank.-1C.P1`,`Blank.-1C.P2`,`Blank.-1C.P3`,`Blank.-1C.P4`,Blank.4C.P1,Blank.4C.P2,Blank.4C.P3,Blank.4C.P4,Blank.11C.P1,Blank.11C.P2,Blank.11C.P3,Blank.11C.P4,Blank.17C.P1,Blank.17C.P2,Blank.17C.P3,Blank.17C.P4)
Blank_Data_Names <- list("Blank.-1C.P1","Blank.-1C.P2","Blank.-1C.P3","Blank.-1C.P4","Blank.4C.P1","Blank.4C.P2","Blank.4C.P3","Blank.4C.P4","Blank.11C.P1","Blank.11C.P2","Blank.11C.P3","Blank.11C.P4","Blank.17C.P1","Blank.17C.P2","Blank.17C.P3","Blank.17C.P4")
plot_lst <- list()
for (i in 1:length(Blank_Data_List)) {
  for (j in 1:length(Blank_Data_Names)) {
    Dname <- Blank_Data_Names[i]
  }
    mData <- melt(Blank_Data_List[[i]], id.vars = "Time Taken")
    p <- ggplot(mData,aes(`Time Taken`,value)) + 
      geom_point() +
      geom_smooth() +
      ggtitle(print(Dname)) + 
      xlab("Time Taken") + 
      ylab("OD600") +
      ylim(0,0.65)
    plot_lst[[i]] <- p 
}
marrangeGrob(plot_lst,nrow = 4, ncol = 4)
#---------------------------All Blank Data with curve ---------------------------------------------------






















