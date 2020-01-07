# General imports
library(ggplot2)
library(plyr)
library(scales)
library(reshape2)

# CSV Dataset import
HRFData <- read.csv2(file="C:/Users/vince/Documents/INSA/5ISS/BigData/HumanRightsAnalysis/dataset.csv", header=TRUE, sep=",", fileEncoding = 'UTF-8')

# =============== Data manipulation =============== # 
# 1st chart: global correlation

# 2nd chart: region-based box plot on hf

# Only take into account data from 2016
LastYearData <- subset(HRFData, HRFData$year == "2016")
# Convert factor data to numerical in order to exploit it in box plot (means, medians, ...)
LastYearData$hf_score <- as.numeric(as.character(LastYearData$hf_score))

# 3rd chart
# Choose to study region with the biggest number of countries
data.frame(table(LastYearData$region))
LastYearSubSaharan <- subset(LastYearData, LastYearData$region == "Sub-Saharan Africa")
LastYearSubSaharan$ef_score <- as.numeric(as.character(LastYearSubSaharan$ef_score))
LastYearSubSaharan$ef_government <- as.numeric(as.character(LastYearSubSaharan$ef_government))

# 4th chart

# 5th chart

# ==================== Plotting =================== # 
# 1st chart

# 2nd chart
par(mar = c(6.5, 6.5, 0.5, 0.5), mgp = c(5, 1, 0))

g <- ggplot(LastYearData, aes(region, hf_score))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Human Freedom indicator by world region", 
       subtitle="Box plot",
       caption="Source: mpg",
       x="World region",
       y="Human Freedom indicator")

# 3rd chart
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(LastYearSubSaharan, aes(ef_government, ef_score))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="Jittered Points", 
       y="hwy", 
       x="cty", 
       title="Correlation of ... in Sub-Saharan Africa")




# 4th chart

# 5th chart