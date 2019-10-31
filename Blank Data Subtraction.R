#-------------------------------Setup------------------------------------------------------------------------------
setwd("C:/Users/Anais/OneDrive/Documents/UAF/Research")
source("Blank Data Average Calc.R")
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(readxl)

#-----Create a list of the sheets of the excel file and take a subset of each strain depending on temperature--------
#run a specfic program for each tab of excel, for loop 
Raw_Sheet_List <- excel_sheets("Total Raw Data.xlsx")
for (i in 1:length(Raw_Sheet_List)) {
  tempdf <- read_excel("Total Raw Data.xlsx", sheet = i, col_names = TRUE)
  tempdf$sheetname <- Raw_Sheet_List[i]
  assign(Raw_Sheet_List[[i]],tempdf)
}
Raw_Sheet_Data <- list(BRX8.1,BRX8.2,BRX8.3,BRX8.4,BRX8.5,BRX8.6,BRX8.7,BRX8.8,BRX8.9,BRX9.1,BRX10.1,BRX10.2,BRX10.3,BRX10.4,BRX10.5,BRX10.6,BRX10.7,BRX10.8,BRX10.9)
rm(All_Means,Blanks,tempdf)
rm(BRX10.1,BRX10.2,BRX10.3,BRX10.4,BRX10.5,BRX10.6,BRX10.7,BRX10.8,BRX10.9,BRX8.1,BRX8.2,BRX8.3,BRX8.4,BRX8.5,BRX8.6,BRX8.7,BRX8.8,BRX8.9,BRX9.1)

# for each of the datasets I need to split it into four sub-dataframes with each of the temperature  and make that into a list in itself
Mod_Raw_Sheet_Data <- list()
for (i in 1:length(Raw_Sheet_Data)) {
  Raw_Sheet_Data[[i]]$Average <- NULL
  Raw_Sheet_Data[[i]]$Date <- NULL
  Raw_Sheet_Data[[i]]$Time <- NULL
  Raw_Sheet_Data[[i]]$`T Number` <- NULL
  name <- paste(Raw_Sheet_Data[[i]][1,12])
  Mod_Raw_Sheet_Data[[i]] <- Raw_Sheet_Data[[i]]
  assign(name,Mod_Raw_Sheet_Data[[i]])
}
rm(BRX10.1,BRX10.2,BRX10.3,BRX10.4,BRX10.5,BRX10.6,BRX10.7,BRX10.8,BRX10.9,BRX8.1,BRX8.2,BRX8.3,BRX8.4,BRX8.5,BRX8.6,BRX8.7,BRX8.8,BRX8.9,BRX9.1)

for (i in 1:length(Mod_Raw_Sheet_Data)) {
  name <- paste(Mod_Raw_Sheet_Data[[i]][1,12]) 
  listA <- list(subset.data.frame(Raw_Sheet_Data[[i]], Raw_Sheet_Data[[i]]$Temperature == -1),subset.data.frame(Raw_Sheet_Data[[i]],Raw_Sheet_Data[[i]]$Temperature == 4),subset.data.frame(Raw_Sheet_Data[[i]],Raw_Sheet_Data[[i]]$Temperature == 11),subset.data.frame(Raw_Sheet_Data[[i]],Raw_Sheet_Data[[i]]$Temperature == 17) )
  assign(name,listA)
}

Plate_1 <- list(BRX8.1,BRX8.2,BRX8.3,BRX8.4,BRX8.5,BRX8.6)
Plate_2 <- list(BRX8.7,BRX8.8,BRX8.9,BRX9.1,BRX10.1,BRX10.2)
Plate_3 <- list(BRX10.3,BRX10.4,BRX10.5,BRX10.6,BRX10.7,BRX10.8)
Plate_4 <- list(BRX10.9)
#--------------------------------------------------------------------------------------------------------------------

#-----Subtract the designated blank value from each experimental value-----------------------------------------------
Subtract_Blanks1 <- function(DataList,PlateNum){
  P_Name <- paste(DataList[[1]][1,12],"_Mod")
  x <- list()
  for (i in 1:length(DataList)) {
    # Get the first data frame of the new list
    if (i == 1){
      x[[i]] <- DataList[[i]]
    }
    
    # Get the second dataframe of the new list
    if ( i == 2){
      if ( PlateNum == 1){
        x[[i]] <- DataList[[i]]  
      } else if(PlateNum == 2){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - B4C.P2.Mod.TotalAverage
        DataList[[i]]$M2 <- DataList[[i]]$M2 - B4C.P2.Mod.TotalAverage
        DataList[[i]]$M3 <- DataList[[i]]$M3 - B4C.P2.Mod.TotalAverage
        DataList[[i]]$M4 <- DataList[[i]]$M4 - B4C.P2.Mod.TotalAverage
        DataList[[i]]$M5 <- DataList[[i]]$M5 - B4C.P2.Mod.TotalAverage
        DataList[[i]]$M6 <- DataList[[i]]$M6 - B4C.P2.Mod.TotalAverage
        DataList[[i]]$M7 <- DataList[[i]]$M7 - B4C.P2.Mod.TotalAverage
        DataList[[i]]$M8 <- DataList[[i]]$M8 - B4C.P2.Mod.TotalAverage
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 3){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - B4C.P3.Mod.TotalAverage
        DataList[[i]]$M2 <- DataList[[i]]$M2 - B4C.P3.Mod.TotalAverage
        DataList[[i]]$M3 <- DataList[[i]]$M3 - B4C.P3.Mod.TotalAverage
        DataList[[i]]$M4 <- DataList[[i]]$M4 - B4C.P3.Mod.TotalAverage
        DataList[[i]]$M5 <- DataList[[i]]$M5 - B4C.P3.Mod.TotalAverage
        DataList[[i]]$M6 <- DataList[[i]]$M6 - B4C.P3.Mod.TotalAverage
        DataList[[i]]$M7 <- DataList[[i]]$M7 - B4C.P3.Mod.TotalAverage
        DataList[[i]]$M8 <- DataList[[i]]$M8 - B4C.P3.Mod.TotalAverage
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 4){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_4C_P4
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_4C_P4
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_4C_P4
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_4C_P4
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_4C_P4
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_4C_P4
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_4C_P4
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_4C_P4
        x[[i]] <- DataList[[i]]
      }
    }
    # Get the 3rd dataframe for the new list
    if ( i == 3){
      if (PlateNum == 1) {
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_11C_P1
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_11C_P1
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_11C_P1
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_11C_P1
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_11C_P1
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_11C_P1
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_11C_P1
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_11C_P1
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 2){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_11C_P2
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_11C_P2
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_11C_P2
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_11C_P2
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_11C_P2
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_11C_P2
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_11C_P2
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_11C_P2
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 3){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_11C_P3
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_11C_P3
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_11C_P3
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_11C_P3
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_11C_P3
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_11C_P3
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_11C_P3
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_11C_P3
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 4){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_11C_P4
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_11C_P4
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_11C_P4
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_11C_P4
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_11C_P4
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_11C_P4
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_11C_P4
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_11C_P4
        x[[i]] <- DataList[[i]]
      }
    }
      
    if ( i == 4){
      if(PlateNum == 1){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_17C_P1
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_17C_P1
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_17C_P1
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_17C_P1
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_17C_P1
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_17C_P1
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_17C_P1
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_17C_P1
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 2){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_17C_P2
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_17C_P2
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_17C_P2
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_17C_P2
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_17C_P2
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_17C_P2
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_17C_P2
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_17C_P2
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 3){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_17C_P3
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_17C_P3
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_17C_P3
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_17C_P3
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_17C_P3
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_17C_P3
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_17C_P3
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_17C_P3
        x[[i]] <- DataList[[i]]
      } else if (PlateNum == 4){
        DataList[[i]]$M1 <- DataList[[i]]$M1 - Abs.Blank_17C_P4
        DataList[[i]]$M2 <- DataList[[i]]$M2 - Abs.Blank_17C_P4
        DataList[[i]]$M3 <- DataList[[i]]$M3 - Abs.Blank_17C_P4
        DataList[[i]]$M4 <- DataList[[i]]$M4 - Abs.Blank_17C_P4
        DataList[[i]]$M5 <- DataList[[i]]$M5 - Abs.Blank_17C_P4
        DataList[[i]]$M6 <- DataList[[i]]$M6 - Abs.Blank_17C_P4
        DataList[[i]]$M7 <- DataList[[i]]$M7 - Abs.Blank_17C_P4
        DataList[[i]]$M8 <- DataList[[i]]$M8 - Abs.Blank_17C_P4
        x[[i]] <- DataList[[i]]
      }
    }
    assign(P_Name,x, envir = .GlobalEnv)
  }
}
Subtract_Blanks1(BRX8.1,1)
Subtract_Blanks1(BRX8.2,1)
Subtract_Blanks1(BRX8.3,1)
Subtract_Blanks1(BRX8.4,1)
Subtract_Blanks1(BRX8.5,1)
Subtract_Blanks1(BRX8.6,1)
Subtract_Blanks1(BRX8.7,2)
Subtract_Blanks1(BRX8.8,2)
Subtract_Blanks1(BRX8.9,2)
Subtract_Blanks1(BRX9.1,2)
Subtract_Blanks1(BRX10.1,2)
Subtract_Blanks1(BRX10.2,2)
Subtract_Blanks1(BRX10.3,3)
Subtract_Blanks1(BRX10.4,3)
Subtract_Blanks1(BRX10.5,3)
Subtract_Blanks1(BRX10.6,3)
Subtract_Blanks1(BRX10.7,3)
Subtract_Blanks1(BRX10.8,3)
Subtract_Blanks1(BRX10.9,4)
#----------------------------Graph each strain depending on temperature & Modification for Graphing------------------
Mod_Data <- list(`BRX8.1 _Mod`,`BRX8.2 _Mod`,`BRX8.3 _Mod`,`BRX8.4 _Mod`,`BRX8.5 _Mod`,`BRX8.6 _Mod`,`BRX8.7 _Mod`,`BRX8.8 _Mod`,`BRX8.9 _Mod`,`BRX9.1 _Mod`,`BRX10.1 _Mod`,`BRX10.2 _Mod`,`BRX10.3 _Mod`,`BRX10.4 _Mod`,`BRX10.5 _Mod`,`BRX10.6 _Mod`,`BRX10.7 _Mod`,`BRX10.8 _Mod`,`BRX10.9 _Mod`)
rm(BRX10.1,`BRX10.1 _Mod`,BRX10.2,`BRX10.2 _Mod`,BRX10.3,`BRX10.3 _Mod`,BRX10.4,`BRX10.4 _Mod`,BRX10.5,`BRX10.5 _Mod`,BRX10.6,`BRX10.6 _Mod`,BRX10.7,`BRX10.7 _Mod`,BRX10.8,`BRX10.8 _Mod`,BRX10.9,`BRX10.9 _Mod`,BRX9.1,`BRX9.1 _Mod`)
rm(BRX8.1,`BRX8.1 _Mod`,BRX8.2,`BRX8.2 _Mod`,BRX8.3,`BRX8.3 _Mod`,BRX8.4,`BRX8.4 _Mod`,BRX8.5,`BRX8.5 _Mod`,BRX8.6,`BRX8.6 _Mod`,BRX8.7,`BRX8.7 _Mod`,BRX8.8,`BRX8.8 _Mod`,BRX8.9,`BRX8.9 _Mod`)
rm(listA,Plate_1,Plate_2,Plate_3,Plate_4)
Data_Names <- list("BRX8.1","BRX8.2","BRX8.3","BRX8.4","BRX8.5","BRX8.6","BRX8.7","BRX8.8","BRX8.9","BRX9.1","BRX10.1","BRX10.2","BRX10.3","BRX10.4","BRX10.5","BRX10.6","BRX10.7","BRX10.8","BRX10.9")

for (i in 1:length(Mod_Data)) {
  for (j in 1:length(PlateNum)) {
    Mod_Data[[i]][[j]]$`Start Time` <- rep(Mod_Data[[i]][[j]][[1,2]],nrow(Mod_Data[[i]][[j]]))
    for (k in 1:nrow(Mod_Data[[i]][[j]])) {
      Mod_Data[[i]][[j]]$`Time Difference` <- difftime(Mod_Data[[i]][[j]]$`Time Taken`,Mod_Data[[i]][[j]]$`Start Time`,units = "hours")
      Mod_Data[[i]][[j]]$`Time Difference` <- round(Mod_Data[[i]][[j]]$`Time Difference`,digits = 3)
    }
  }
}

# Recombine the dataframes by strain 
for (i in 1:length(Mod_Data)) {
y <- paste(Mod_Data[[i]][[1]][1,12],"_Mod_ALL")
x <- rbind.data.frame(Mod_Data[[i]][[1]],Mod_Data[[i]][[2]],Mod_Data[[i]][[3]],Mod_Data[[i]][[4]])
x$`Plate Number` <- NULL
x$sheetname <- NULL
x$`Time Taken` <- NULL
x$`Start Time` <- NULL
assign(y,x,envir = .GlobalEnv)
}

#Delete -1 data for all strains and 4 degree data from Plate 1
`BRX8.1 _Mod_ALL` <- `BRX8.1 _Mod_ALL`[!(`BRX8.1 _Mod_ALL`$Temperature == "-1"),]
`BRX8.1 _Mod_ALL` <- `BRX8.1 _Mod_ALL`[!(`BRX8.1 _Mod_ALL`$Temperature == "4"),]
`BRX8.2 _Mod_ALL` <- `BRX8.2 _Mod_ALL`[!(`BRX8.2 _Mod_ALL`$Temperature == "-1"),]
`BRX8.2 _Mod_ALL` <- `BRX8.2 _Mod_ALL`[!(`BRX8.2 _Mod_ALL`$Temperature == "4"),]
`BRX8.3 _Mod_ALL` <- `BRX8.3 _Mod_ALL`[!(`BRX8.3 _Mod_ALL`$Temperature == "-1"),]
`BRX8.3 _Mod_ALL` <- `BRX8.3 _Mod_ALL`[!(`BRX8.3 _Mod_ALL`$Temperature == "4"),]
`BRX8.4 _Mod_ALL` <- `BRX8.4 _Mod_ALL`[!(`BRX8.4 _Mod_ALL`$Temperature == "-1"),]
`BRX8.4 _Mod_ALL` <- `BRX8.4 _Mod_ALL`[!(`BRX8.4 _Mod_ALL`$Temperature == "4"),]
`BRX8.5 _Mod_ALL` <- `BRX8.5 _Mod_ALL`[!(`BRX8.5 _Mod_ALL`$Temperature == "-1"),]
`BRX8.5 _Mod_ALL` <- `BRX8.5 _Mod_ALL`[!(`BRX8.5 _Mod_ALL`$Temperature == "4"),]
`BRX8.6 _Mod_ALL` <- `BRX8.6 _Mod_ALL`[!(`BRX8.6 _Mod_ALL`$Temperature == "-1"),]
`BRX8.6 _Mod_ALL` <- `BRX8.6 _Mod_ALL`[!(`BRX8.6 _Mod_ALL`$Temperature == "4"),]
`BRX8.7 _Mod_ALL` <- `BRX8.7 _Mod_ALL`[!(`BRX8.7 _Mod_ALL`$Temperature == "-1"),]
`BRX8.8 _Mod_ALL` <- `BRX8.8 _Mod_ALL`[!(`BRX8.8 _Mod_ALL`$Temperature == "-1"),]
`BRX8.9 _Mod_ALL` <- `BRX8.9 _Mod_ALL`[!(`BRX8.9 _Mod_ALL`$Temperature == "-1"),]
`BRX9.1 _Mod_ALL` <- `BRX9.1 _Mod_ALL`[!(`BRX9.1 _Mod_ALL`$Temperature == "-1"),]
`BRX10.1 _Mod_ALL` <- `BRX10.1 _Mod_ALL`[!(`BRX10.1 _Mod_ALL`$Temperature == "-1"),]
`BRX10.2 _Mod_ALL` <- `BRX10.2 _Mod_ALL`[!(`BRX10.2 _Mod_ALL`$Temperature == "-1"),]
`BRX10.3 _Mod_ALL` <- `BRX10.3 _Mod_ALL`[!(`BRX10.3 _Mod_ALL`$Temperature == "-1"),]
`BRX10.4 _Mod_ALL` <- `BRX10.4 _Mod_ALL`[!(`BRX10.4 _Mod_ALL`$Temperature == "-1"),]
`BRX10.5 _Mod_ALL` <- `BRX10.5 _Mod_ALL`[!(`BRX10.5 _Mod_ALL`$Temperature == "-1"),]
`BRX10.6 _Mod_ALL` <- `BRX10.6 _Mod_ALL`[!(`BRX10.6 _Mod_ALL`$Temperature == "-1"),]
`BRX10.7 _Mod_ALL` <- `BRX10.7 _Mod_ALL`[!(`BRX10.7 _Mod_ALL`$Temperature == "-1"),]
`BRX10.8 _Mod_ALL` <- `BRX10.8 _Mod_ALL`[!(`BRX10.8 _Mod_ALL`$Temperature == "-1"),]
`BRX10.9 _Mod_ALL` <- `BRX10.9 _Mod_ALL`[!(`BRX10.9 _Mod_ALL`$Temperature == "-1"),]



# Melt the Data to plot it
BRX8.1_Melt <- melt(`BRX8.1 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.2_Melt <- melt(`BRX8.2 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.3_Melt <- melt(`BRX8.3 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.4_Melt <- melt(`BRX8.4 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.5_Melt <- melt(`BRX8.5 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.6_Melt <- melt(`BRX8.6 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.7_Melt <- melt(`BRX8.7 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.8_Melt <- melt(`BRX8.8 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX8.9_Melt <- melt(`BRX8.9 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX9.1_Melt <- melt(`BRX9.1 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.1_Melt <- melt(`BRX10.1 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.2_Melt <- melt(`BRX10.2 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.3_Melt <- melt(`BRX10.3 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.4_Melt <- melt(`BRX10.4 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.5_Melt <- melt(`BRX10.5 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.6_Melt <- melt(`BRX10.6 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.7_Melt <- melt(`BRX10.7 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.8_Melt <- melt(`BRX10.8 _Mod_ALL`,id = c("Temperature","Time Difference"))
BRX10.9_Melt <- melt(`BRX10.9 _Mod_ALL`,id = c("Temperature","Time Difference"))

Melt_Data_List <- list(BRX8.1_Melt,BRX8.2_Melt,BRX8.3_Melt,BRX8.4_Melt,BRX8.5_Melt,BRX8.6_Melt,BRX8.7_Melt,BRX8.8_Melt,BRX8.9_Melt,BRX9.1_Melt,BRX10.1_Melt,BRX10.2_Melt,BRX10.3_Melt,BRX10.4_Melt,BRX10.5_Melt,BRX10.6_Melt,BRX10.7_Melt,BRX10.8_Melt,BRX10.9_Melt)
Data_Names <- list("BRX8.1","BRX8.2","BRX8.3","BRX8.4","BRX8.5","BRX8.6","BRX8.7","BRX8.8","BRX8.9","BRX9.1","BRX10.1","BRX10.2","BRX10.3","BRX10.4","BRX10.5","BRX10.6","BRX10.7","BRX10.8","BRX10.9")
`Data _ Mod_All` <- list(`BRX8.1 _Mod_ALL`,`BRX8.2 _Mod_ALL`,`BRX8.3 _Mod_ALL`,`BRX8.4 _Mod_ALL`,`BRX8.5 _Mod_ALL`,`BRX8.6 _Mod_ALL`,`BRX8.7 _Mod_ALL`,`BRX8.8 _Mod_ALL`,`BRX8.9 _Mod_ALL`,`BRX9.1 _Mod_ALL`,`BRX10.1 _Mod_ALL`,`BRX10.2 _Mod_ALL`,`BRX10.3 _Mod_ALL`,`BRX10.4 _Mod_ALL`,`BRX10.5 _Mod_ALL`,`BRX10.6 _Mod_ALL`,`BRX10.7 _Mod_ALL`,`BRX10.8 _Mod_ALL`,`BRX10.9 _Mod_ALL`)


rm(`BRX10.1 _Mod_ALL`,BRX10.1_Melt,`BRX10.2 _Mod_ALL`,BRX10.2_Melt,`BRX10.3 _Mod_ALL`,BRX10.3_Melt,`BRX10.4 _Mod_ALL`,BRX10.4_Melt,`BRX10.5 _Mod_ALL`,BRX10.5_Melt,`BRX10.6 _Mod_ALL`,BRX10.6_Melt,`BRX10.7 _Mod_ALL`,BRX10.7_Melt,BRX10.8_Melt,`BRX10.8 _Mod_ALL`,`BRX10.9 _Mod_ALL`,BRX10.9_Melt)
rm(`BRX8.2 _Mod_ALL`,BRX8.2_Melt,`BRX8.3 _Mod_ALL`,BRX8.3_Melt,`BRX8.4 _Mod_ALL`,BRX8.4_Melt,`BRX8.5 _Mod_ALL`,BRX8.5_Melt,`BRX8.6 _Mod_ALL`,BRX8.6_Melt,`BRX8.7 _Mod_ALL`,BRX8.7_Melt,`BRX8.8 _Mod_ALL`,BRX8.8_Melt,`BRX8.9 _Mod_ALL`,BRX8.9_Melt,`BRX9.1 _Mod_ALL`,BRX9.1_Melt)
Test_Mod_ALL <- `BRX8.1 _Mod_ALL`
Test_Melt <-BRX8.1_Melt
Strain_Plot_List <- list()
for (i in 1:length(Melt_Data_List)) {
  for (j in 1:length(Data_Names)) {
    Dname <- Data_Names[i]
  }
  p <- ggplot(Melt_Data_List[[i]],aes(x = `Time Difference`,y = value, group = Temperature, colour = factor(Temperature))) + 
    geom_point(size = 1) +
    scale_color_manual(values = c("red","blue","purple","orange")) +
    geom_smooth() +
    ggtitle(print(Dname)) + 
    xlab("Time (Hours)") + 
    ylab("OD600")
  Strain_Plot_List[[i]] <- p 
}
marrangeGrob(Strain_Plot_List,nrow = 3, ncol = 2)

Strain_Plot_List <- list()
for (i in 1:length(Melt_Data_List)) {
  for (j in 1:length(Data_Names)) {
    Dname <- Data_Names[i]
  }
  p <- ggplot(Melt_Data_List[[i]],aes(x = `Time Difference`,y = value, group = Temperature, colour = factor(Temperature))) + 
    geom_point(size = 1) +
    scale_color_manual(values = c("red","blue","purple","orange")) +
    geom_smooth() +
    ggtitle(print(Dname)) + 
    xlab("Time (Hours") + 
    ylab("OD600") + 
    scale_y_log10()
  Strain_Plot_List[[i]] <- p 
}
marrangeGrob(Strain_Plot_List,nrow = 3, ncol = 2)


for (i in 1:length(Mod_Data)) {
  y <- paste(Mod_Data[[i]][[1]][1,12],"ALL")
  x <- rbind.data.frame(Mod_Data[[i]][[1]],Mod_Data[[i]][[2]],Mod_Data[[i]][[3]],Mod_Data[[i]][[4]])
  assign(y,x,envir = .GlobalEnv)
}

All.Mod.Data <- list(`BRX8.1 ALL`,`BRX8.2 ALL`,`BRX8.3 ALL`,`BRX8.4 ALL`,`BRX8.5 ALL`,`BRX8.6 ALL`,`BRX8.7 ALL`,`BRX8.8 ALL`,`BRX8.9 ALL`,`BRX9.1 ALL`,`BRX10.1 ALL`,`BRX10.2 ALL`,`BRX10.3 ALL`,`BRX10.4 ALL`,`BRX10.5 ALL`,`BRX10.6 ALL`,`BRX10.7 ALL`,`BRX10.8 ALL`,`BRX10.9 ALL`)
rm(`BRX10.1 ALL`,`BRX10.2 ALL`,`BRX10.3 ALL`,`BRX10.4 ALL`,`BRX10.5 ALL`,`BRX10.6 ALL`,`BRX10.7 ALL`,`BRX10.8 ALL`,`BRX10.9 ALL`,`BRX9.1 ALL`,`BRX8.1 ALL`,`BRX8.2 ALL`,`BRX8.3 ALL`,`BRX8.4 ALL`,`BRX8.5 ALL`,`BRX8.6 ALL`,`BRX8.7 ALL`,`BRX8.8 ALL`,`BRX8.9 ALL`)
rm(Dname,x,i,j,name,y)
rm(Abs.Blank_11C_P1,Abs.Blank_11C_P2,Abs.Blank_11C_P3,Abs.Blank_11C_P4,Abs.Blank_17C_P1,Abs.Blank_17C_P2,Abs.Blank_17C_P3,Abs.Blank_17C_P4,Abs.Blank_4C_P4,B4C.P2.Mod.TotalAverage,B4C.P3.Mod.TotalAverage)
rm(B_Data_Names,B_Data,UpDated_B_Data,p,Test_Mod_ALL)
#-------------------------------------------------------------------------------------------------------------------- 