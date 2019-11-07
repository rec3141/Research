#------------------------------------------SETUP------------------------------------------------------------------------------
setwd("C:/Users/Anais/OneDrive/Documents/UAF/Research/")
library(protr)
library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(data.table)

# do an anova for each strain comparing each temperature to isolate the strain you can use using the strains you can use, make 
# a graph of each strain of growth rate vs. temperature fit a line through it (non-linear fit). This will give you an optimal 
# temperature that is not necessarily one of temps chosen this will also give you an optimal growth temperature range 
Ind.Med.Data <- read.csv("Indices Median Data.csv",header = TRUE)
CB.GR.Data <- read.csv("Chesapeake Bay - Colwellia Growth Rates.csv",header = TRUE)

# do a subset of just the ones of Chesapeake Bay( not public)

CB.Ind.Med.Data <- filter(Ind.Med.Data,!Type == "P") # BRX8.1 and 10.8 are missing? 

#---------------have a dataframe of the growth rates of each strain at each temperature------------------------------------------

CB.GR.Data$X.1 <- NULL
CB.GR.Data$X <- NULL
grd <- melt(CB.GR.Data ,id.vars = c("Strain","Temperature"))
grd$Temperature <- as.factor(grd$Temperature)
Data <- split(grd,grd$Strain) # split dataset into multiple datasets depending on strain

#-------------------------------Anova Analysis of Temperatures for each Strain---------------------------------------------------
# with the summary of the anova you are going to get P values and F values. A P-value that is less tha 0.05 means that we can accept
# the alternative hypothesis that the mean of all three temperatures are NOT equal. A large F-value (greater than 0)means that the variation
# among sample means is greater than the variation within the group ( and vice versa in terms of a low F-value). Isolated those strains
# that are significantly different between the temperatures (Sig.Diff.Strains)
result.list <- list()
Temp.Statistic.Data <- NULL
for (i in 1:length(Data)){
  Dataframe <- Data[[i]]
  result.list[[1]] <- paste(Dataframe[1,1])
  result.list[[2]] <- summary(aov(value~Temperature,data = Dataframe))[[1]][["F value"]][[1]]
  p.value <- summary(aov(value~Temperature,data = Dataframe))[[1]][["Pr(>F)"]][[1]]
  result.list[[3]] <- p.value
  if(p.value < 0.05){
    result.list[[4]] <- paste("Sig.Temp.Diff")
  }else {
    result.list[[4]] <- paste("No.Sig.Temp.Diff")
  }
  Temp.Statistic.Data <- rbind.data.frame(Temp.Statistic.Data,result.list,stringsAsFactors = FALSE)
}
colnames(Temp.Statistic.Data) <- c("Strain","F-value","p-value","Significant Difference")
Sig.Diff.Data <- split(Temp.Statistic.Data,Temp.Statistic.Data$`Significant Difference`)
Sig.Diff.Strains <- Sig.Diff.Data[[2]]

#----------------------create a dataframe with optimal temperature and optimal growth range (max)------------------------
# subset only the strains where the growth rates between temperatures are statistically significant 
Rel.Strain <- as.list(unique(Sig.Diff.Strains$Strain))
Rel.GR.Data <- CB.GR.Data[CB.GR.Data$Strain %in% Rel.Strain,]
Rel.grd <- melt(Rel.GR.Data,id.vars = c("Strain","Temperature"))
Rel.grd$Temperature <- as.factor(Rel.grd$Temperature)
Rel.Temp.Plot <- ggplot(data = Rel.grd,aes(x = Temperature, y = value))+
  geom_point()+
  stat_smooth(method = "loess",aes(colour = "loess",group = Strain)) +
  facet_wrap(~Strain)
print(Rel.Temp.Plot)

Rel.GR.Data$Average <- rowMeans(Rel.GR.Data[,3:10]) # going to average out each temperature at each strain and use temperature where the growth rate is max. 
Rel.GR.Data <- data.table(Rel.GR.Data, key = "Strain")
Rel.GR.Average <- Rel.GR.Data[, .SD[Average %in% max(Average)], by= Strain] # use the temperature associated with the maximum average
Rel.GR.OPT <- Rel.GR.Average[,-c(3:10)] # remove replicate columns
colnames(Rel.GR.OPT)[1] <- "Type"
Rel.Ind.Data <- CB.Ind.Med.Data[CB.Ind.Med.Data$Type %in% Rel.Strain,]
Rel.Data <- merge.data.frame(Rel.Ind.Data,Rel.GR.OPT,by="Type")
colnames(Rel.Data)[10] <- "OPT"
colnames(Rel.Data)[11] <- "Max.GR.Avg." # Created one dataframe that has the reliable strains with their optimal temperatures and the median of each indice

# ----------------------create a dataframe that compares optimal temperature vs. median indice and gets a pearson coef.----------------------------------
Arg.Lys_OPT.PR <- cor.test(Rel.Data$Arg.Lys.Ratio,Rel.Data$OPT,method = "pearson")$estimate
Acid.Res_OPT.PR <- cor.test(Rel.Data$Acidic.Residue,Rel.Data$OPT,method = "pearson")$estimate
GRAVY_OPT.PR <- cor.test(Rel.Data$GRAVY,Rel.Data$OPT,method = "pearson")$estimate
Pro.Res_OPT.PR <- cor.test(Rel.Data$Proline.Residue,Rel.Data$OPT,method = "pearson")$estimate
Aroma_OPT.PR <- cor.test(Rel.Data$Aromaticity,Rel.Data$OPT,method = "pearson")$estimate
Alipha.Ind_OPT.PR <- cor.test(Rel.Data$Aliphatic.Index,Rel.Data$OPT,method = "pearson")$estimate

# create a heatmap of strain vs. optimal temperature vs. indice value (fill)
# https://jcoliver.github.io/learn-r/006-heatmaps.html