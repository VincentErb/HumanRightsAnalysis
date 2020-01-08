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
LastYearSubSaharan$ef_score <- as.numeric(as.character(LastYearSubSaharan$ef_score))
LastYearSubSaharan$ef_government <- as.numeric(as.character(LastYearSubSaharan$ef_government))

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

# ==================== Plotting =================== # 
# -------- 1st chart
g <- ggplot(LastYearData, aes(pf_score, ef_score))
g + geom_point(aes(colour = region), size = 2) + labs(title="Personnal and economical freedom for each country by region", 
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
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(LastYearSubSaharan, aes(ef_government, ef_score))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="Jittered Points", 
       y="hwy", 
       x="cty", 
       title="Correlation of ... in Sub-Saharan Africa")




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