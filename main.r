# General imports
library(ggplot2)
library(plyr)
library(scales)
library(reshape2)

# CSV Dataset import
HRFData <- read.csv2(file="D:/5 ISS/Bigdata/HumanRightsAnalysis/dataset.csv", header=TRUE, sep=",", fileEncoding = 'UTF-8')

# =============== Data manipulation =============== # 
# -------- 1st chart: global correlation of economic and personnal freedom
# Only take into account data from 2016
LastYearData <- subset(HRFData, HRFData$year == "2016")
# Convert factor data to numerical in order to plot it correctly
LastYearData$pf_score <- as.numeric(as.character(LastYearData$pf_score))
LastYearData$ef_score <- as.numeric(as.character(LastYearData$ef_score))



# -------- 2nd chart: region-based box plot on hf

# Only take into account data from 2016
LastYearData <- subset(HRFData, HRFData$year == "2016")
# Convert factor data to numerical in order to exploit it in box plot (means, medians, ...)
LastYearData$hf_score <- as.numeric(as.character(LastYearData$hf_score))

# -------- 3rd chart
# Choose to study region with the biggest number of countries
data.frame(table(LastYearData$region))
LastYearSubSaharan <- subset(LastYearData, LastYearData$region == "Sub-Saharan Africa")
LastYearSubSaharan$pf_ss_women <- as.numeric(as.character(LastYearSubSaharan$pf_ss_women))
LastYearSubSaharan$pf_religion_restrictions <- as.numeric(as.character(LastYearSubSaharan$pf_religion_restrictions))

# -------- 4th chart : Study of France motto compare to Europe and the World
# Convert factor data to numerical in order to plot it correctly
LastYearData$pf_expression <- as.numeric(as.character(LastYearData$pf_expression))
LastYearData$pf_rol <- as.numeric(as.character(LastYearData$pf_rol))
LastYearData$pf_identity <- as.numeric(as.character(LastYearData$pf_identity))

#Create a data frame containing informations about western european countries
LastYearWesternEurope <- subset(LastYearData, LastYearData$region == "Western Europe")
WesternEuropeMean <- data.frame(name = "Western Europe", pf_expression = mean(na.omit(LastYearWesternEurope[["pf_expression"]])), pf_rol = mean(na.omit(LastYearWesternEurope[["pf_rol"]])), pf_identity = mean(na.omit(LastYearWesternEurope[["pf_identity"]])))

#Create a data frame containing informations about France
LastYearFrance <- subset(LastYearWesternEurope, LastYearWesternEurope$countries == "France")
LibEquFraFrance <- data.frame(name = "France", pf_expression = LastYearFrance$pf_expression, pf_rol = LastYearFrance$pf_rol, pf_identity = LastYearFrance$pf_identity)

#Create a data frame containing informations about the world
WorldMean <- data.frame(name = "World", pf_expression = mean(na.omit(LastYearData[["pf_expression"]])), pf_rol = mean(na.omit(LastYearData[["pf_rol"]])), pf_identity = mean(na.omit(LastYearData[["pf_identity"]])))

#Create the data.frame composed of all previous data frames in order to plot it 
LibEquFraWorld <- rbind(LibEquFraFrance, WesternEuropeMean, WorldMean)


# -------- 5th chart
# Get the data frame composed only of the human freedom score mean by year worlwide
HRFData$hf_score <- as.numeric(as.character(HRFData$hf_score))
HFSmean <- ddply(HRFData, "year", function(X) data.frame(m = mean(na.omit(X$hf_score))))
HFSmeanWW <- data.frame(region = "World", Year = HFSmean$year, Human_Freedom_Score =  HFSmean$m)

# Get the data frame composed only of the human freedom score mean by year in Western Europe
WesternEuropeAllYear <- subset(HRFData, HRFData$region == "Western Europe")
WesternEuropeAllYear$hf_score <- as.numeric(as.character(WesternEuropeAllYear$hf_score))
HFSmean <- ddply(WesternEuropeAllYear, "year", function(X) data.frame(m = mean(na.omit(X$hf_score))))
HFSmeanWE <- data.frame(region = "Western Europe", Year = HFSmean$year, Human_Freedom_Score =  HFSmean$m)

# Get the data frame composed only of the human freedom score mean by year in South Asia
SouthAsiaAllYear <- subset(HRFData, HRFData$region == "South Asia")
SouthAsiaAllYear$hf_score <- as.numeric(as.character(SouthAsiaAllYear$hf_score))
HFSmean <- ddply(SouthAsiaAllYear, "year", function(X) data.frame(m = mean(na.omit(X$hf_score))))
HFSmeanSA <- data.frame(region = "South Asia", Year = HFSmean$year, Human_Freedom_Score =  HFSmean$m)

# Get the data frame composed only of the human freedom score mean by year in Middle East & North Africa
MeNaAllYear <- subset(HRFData, HRFData$region == "Middle East & North Africa")
MeNaAllYear$hf_score <- as.numeric(as.character(MeNaAllYear$hf_score))
HFSmean <- ddply(MeNaAllYear, "year", function(X) data.frame(m = mean(na.omit(X$hf_score))))
HFSmeanMeNa <- data.frame(region = "Middle East & North Africa", Year = HFSmean$year, Human_Freedom_Score =  HFSmean$m)

# Get the data frame composed only of the human freedom score mean by year in Sub-Saharan Africa
SubSaharanAfricaAllYear <- subset(HRFData, HRFData$region == "Sub-Saharan Africa")
SubSaharanAfricaAllYear$hf_score <- as.numeric(as.character(SubSaharanAfricaAllYear$hf_score))
HFSmean <- ddply(SubSaharanAfricaAllYear, "year", function(X) data.frame(m = mean(na.omit(X$hf_score))))
HFSmeanSSA <- data.frame(region = "Sub-Saharan Africa", Year = HFSmean$year, Human_Freedom_Score =  HFSmean$m)

# Get the data frame composed only of the human freedom score mean by year in Latin America & the Caribbean
LacAllYear <- subset(HRFData, HRFData$region == "Latin America & the Caribbean")
LacAllYear$hf_score <- as.numeric(as.character(LacAllYear$hf_score))
HFSmean <- ddply(LacAllYear, "year", function(X) data.frame(m = mean(na.omit(X$hf_score))))
HFSmeanLAC <- data.frame(region = "Latin America & the Caribbean", Year = HFSmean$year, Human_Freedom_Score =  HFSmean$m)

# Get the data frame composed only of the human freedom score mean by year in Oceania
OcAllYear <- subset(HRFData, HRFData$region == "Oceania")
OcAllYear$hf_score <- as.numeric(as.character(OcAllYear$hf_score))
HFSmean <- ddply(OcAllYear, "year", function(X) data.frame(m = mean(na.omit(X$hf_score))))
HFSmeanOC <- data.frame(region = "Oceania", Year = HFSmean$year, Human_Freedom_Score =  HFSmean$m)

# Bind all the different data set together
HFSfinal <- rbind(HFSmeanWW, HFSmeanWE, HFSmeanSSA, HFSmeanMeNa, HFSmeanSA, HFSmeanLAC, HFSmeanOC)

# ==================== Plotting =================== # 
# -------- 1st chart
g <- ggplot(LastYearData, aes(pf_score, ef_score))
g + geom_point(aes(colour = region), size = 2) + geom_abline(intercept=0, slope=1) + labs(title="Personnal and economical freedom for each country by region", 
                                                      subtitle="Box plot",
                                                      caption="Source: mpg",
                                                      x="Personnal Freedom",
                                                      y="Economical Freedom")

# -------- 2nd chart
par(mar = c(6.5, 6.5, 0.5, 0.5), mgp = c(5, 1, 0))

g <- ggplot(LastYearData, aes(region, hf_score))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Human Freedom indicator by world region", 
       subtitle="Box plot",
       caption="Source: mpg",
       x="World region",
       y="Human Freedom indicator")

# -------- 3rd chart
g <- ggplot(LastYearSubSaharan, aes(pf_religion_restrictions, pf_ss_women))
g + geom_point(size = 3, colour="#51bcda") + geom_smooth(method = lm, se = FALSE) +
  labs(subtitle="ScatterPlot", 
       y="Woman freedom", 
       x="Religion restrictions", 
       title="Correlation of Woman Freedom and Religion in Sub-Saharan Africa") + theme_gray()



#  -------- 4th chart
# We "melt" the data frame in order to reorganize the data.frame to plot it correctly (transform a complex line in multiples simples lines)
LibEquFraWorld.m <- melt(LibEquFraWorld, id.vars='name')
g <- ggplot(LibEquFraWorld.m, aes(variable, value))   
g +  geom_bar(aes(fill = name), position = "dodge", stat="identity") + labs(title="Liberty, Equality, Fraternity : a true motto ? ", 
       subtitle="Box plot",
       caption="Source: mpg",
       x="Variables",
       y="Value") 

#  -------- 5th chart
g <- ggplot(HFSfinal, aes(Year, Human_Freedom_Score))
g + geom_line(aes(colour = region)) + labs(title="Human freedom evolution through the years by regions", 
                                           subtitle="Box plot",
                                           caption="Source: mpg",
                                           x="Years",
                                           y="Human Freedom Score") 

