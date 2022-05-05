#READ IN DATA AND CLEAN NAMES
#################################################################
heartData <- read.csv("heart_updated.csv")
strokeData <- read.csv("stroke_updated.csv")
spending <- read.csv("regionspending.csv")
strokeData[strokeData == "American Indian and Alaskan Native"] <- "AI/AN"
strokeData[strokeData == "Asian and Pacific Islander"] <- "A/PI"
heartData[heartData == "American Indian and Alaskan Native"] <- "AI/AN"
heartData[heartData == "Asian and Pacific Islander"] <- "A/PI"

Race <- c("White", "Black", "Asian", "Hispanic")
AnyHealthInsurance <- c(87.2,84.1,85.5,75.7)
PrivateHealthInsurance <- c(66.8,49.6,68.1,45.6)
GovtHealthInsurance <- c(33.2,43.8,25.8,36.4)
Uninsured <- c(12.8, 15.9, 14.5, 24.3)
Insurance <- data.frame(Race, AnyHealthInsurance, PrivateHealthInsurance, GovtHealthInsurance, Uninsured)

Race <- c("White", "Asian", "Hispanic", "Black", "Alaskan Natives", "Native Americans")
DiabetesPrevalence <- c(7.1,8.4,11.8,12.6,5.5,33)
Diabetes <- data.frame(Race,DiabetesPrevalence)


#ADD LIBRARIES
#################################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(agricolae)
library(FSA)
library(ggpubr)
library(scales)
require(gridExtra)

#SUBSETTING DATA FOR FURTHER ANALYSIS
#################################################################
#subset data by ethnicity for stroke/heart
whiteheartData = subset(x = heartData, subset = Race.Ethnicity == "White")
whitestrokeData = subset(x = strokeData, subset = Race.Ethnicity == "White")
BlackheartData = subset(x = heartData, subset = Race.Ethnicity == "Black")
BlackstrokeData = subset(x = strokeData, subset = Race.Ethnicity == "Black")
APIheartData = subset(x = heartData, subset = Race.Ethnicity == "A/PI")
APIstrokeData = subset(x = strokeData, subset = Race.Ethnicity == "A/PI")
HispanicheartData = subset(x = heartData, subset = Race.Ethnicity == "Hispanic")
HispanicstrokeData = subset(x = strokeData, subset = Race.Ethnicity == "Hispanic")
AIANheartData = subset(x = heartData, subset = Race.Ethnicity == "AI/AN")
AIANstrokeData = subset(x = strokeData, subset = Race.Ethnicity == "AI/AN")


#ADD REGIONS FOR MERGED DATA
#################################################################
mergedheartData <- rbind(whiteheartData,BlackheartData, APIheartData, HispanicheartData, AIANheartData)
mergedstrokeData <- rbind(whitestrokeData,BlackstrokeData, APIstrokeData, HispanicstrokeData, AIANstrokeData)
mergedheartData$region <- with(mergedheartData, 
                         ifelse(LocationAbbr %in% c("AR","TN","LA","MS","AL","FL","GA","SC","NC","KY","WV","DE","MD", "DC","TX","OK", "VA"),'South', 
                                ifelse(LocationAbbr %in% c("HI","AK","OR","WA","ID","UT","NV","CA","AZ","MT","CO","NM"), 'West',
                                       ifelse(LocationAbbr %in% c("IN","OH","MI","IL","MO","IA","MN","ND","SD","NE","KS", "WI"), 'MidWest',
                                                     ifelse(LocationAbbr %in% c("PA","RI","CT", "ME", "NH", "VT", "MA", "NY", "NJ"), 'Northeast',
                                                            ifelse(LocationAbbr %in% c("US"), 'Overall', 'Overall')
                                              )
                                       )
                                )
                         )
)

mergedstrokeData$region <- with(mergedstrokeData, 
                               ifelse(LocationAbbr %in% c("AR","TN","LA","MS","AL","FL","GA","SC","NC","KY","WV","DE","MD", "DC","TX","OK", "VA"),'South', 
                                      ifelse(LocationAbbr %in% c("HI","AK","OR","WA","ID","UT","NV","CA","AZ","MT","CO","NM"), 'West',
                                             ifelse(LocationAbbr %in% c("IN","OH","MI","IL","MO","IA","MN","ND","SD","NE","KS", "WI"), 'MidWest',
                                                    ifelse(LocationAbbr %in% c("PA","RI","CT", "ME", "NH", "VT", "MA", "NY", "NJ"), 'Northeast',
                                                           ifelse(LocationAbbr %in% c("US"), 'Overall', 'Overall')
                                                    )
                                             )
                                      )
                               )
)
mergedheartData$region <- gsub("Overall", "U.S. Overall", mergedheartData$region)
mergedstrokeData$region <- gsub("Overall", "U.S. Overall", mergedstrokeData$region)


#INSURANCE RATES PIE GRAPH
#################################################################


Insurance$fraction = Insurance$AnyHealthInsurance / sum(Insurance$AnyHealthInsurance)
Insurance$ymax = cumsum(Insurance$fraction)
Insurance$ymin = c(0, head(Insurance$ymax, n = -1))
Insurance$labelPosition <- (Insurance$ymax + Insurance$ymin) / 2
Insurance$label <- paste0(Insurance$Race, "\n Percentage: ", round(Insurance$fraction * 100))

ggplot(Insurance, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = Race)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=4) +
  coord_polar(theta="y") +
  scale_fill_brewer("Blues") +
  xlim(c(2,4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Uninsured by Race/Ethnicity (2013)", subtitle = "By Patrick Asztabski")

#DIABETES PREVALENCE PIE GRAPH
#################################################################

Diabetes$fraction = Diabetes$DiabetesPrevalence / sum(Diabetes$DiabetesPrevalence)
Diabetes$ymax = cumsum(Diabetes$fraction)
Diabetes$ymin = c(0, head(Diabetes$ymax, n = -1))
Diabetes$labelPosition <- (Diabetes$ymax + Diabetes$ymin) / 2
Diabetes$label <- paste0(Diabetes$Race, "\n Percentage: ", round(Diabetes$fraction * 100))
labels = paste0(DiabetesPrevalence, "%")

x <- ggplot(data = Diabetes, aes(x="", y = DiabetesPrevalence, fill = Race)) +
  geom_col(stat = "identity") +
  geom_text(aes(label=labels), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Diabetes Prevalence by Race/Ethnicity in Adults >= 20 years of age (2013)", subtitle = "By Patrick Asztabski")
x


ggplot(Diabetes, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = Race)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=4) +
  coord_polar(theta="y") +
  scale_fill_brewer("Blues") +
  xlim(c(2,4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Diabetes Prevalence by Race/Ethnicity in Adults >= 20 years of age (2013)", subtitle = "By Patrick Asztabski")

#Exploratory Data Analysis
#################################################################
#Number of deaths
case.vector = tapply(mergedheartData$Deaths.per.100.000, mergedheartData$Race.Ethnicity, sum)
case.vector2 = tapply(mergedstrokeData$Deaths.per.100.000, mergedstrokeData$Race.Ethnicity, sum)

case.vector #Total number of deaths for this year from heart disease
case.vector2 #stroke disease

#Average number of deaths
aggregate(mergedheartData$Deaths.per.100.000, list(mergedheartData$Race.Ethnicity), FUN=median) #Average number of deaths
aggregate(mergedstrokeData$Deaths.per.100.000, list(mergedstrokeData$Race.Ethnicity), FUN=median)


#STATISTICAL TESTING MODEL
#################################################################
#TEST FOR NORMALITY

p1 <- ggqqplot(whitestrokeData$Deaths.per.100.000)
p2 <- ggqqplot(BlackstrokeData$Deaths.per.100.000)
p3 <- ggqqplot(APIstrokeData$Deaths.per.100.000)
p4 <- ggqqplot(AIANstrokeData$Deaths.per.100.000)
p5 <- ggqqplot(HispanicstrokeData$Deaths.per.100.000)


ggqqplot(whiteheartData$Deaths.per.100.000)
ggqqplot(BlackheartData$Deaths.per.100.000)
ggqqplot(APIheartData$Deaths.per.100.000)
ggqqplot(AIANheartData$Deaths.per.100.000)
ggqqplot(HispanicheartData$Deaths.per.100.000)

#Kruskal Test

# Hypotheses from Median Model
# H_0: me_A/PI = me_AI/AN = me_Black = me_Hispanic = me_White
# H_1: at least one of the medians is different

# Kruskal test is used to find significant differences in the medians
#of the independent groups (race/ethnicity groups) (deaths per 100,000)

kruskal.test(mergedheartData$Deaths.per.100.000 ~ mergedheartData$Race.Ethnicity)
kruskal.test(mergedstrokeData$Deaths.per.100.000 ~ mergedstrokeData$Race.Ethnicity)
# In both datasets, p-value is small for both tests, reject H_0.  
# Conclude at least one median is different statistically. There is a significant
# difference in the average mortalities for stroke and heart between races.

#Use poc-host to find exact differences

dunnTest(mergedheartData$Deaths.per.100.000 ~ mergedheartData$Race.Ethnicity)
dunnTest(mergedstrokeData$Deaths.per.100.000 ~ mergedstrokeData$Race.Ethnicity)
# In both datasets, p-value is small for every comparison, reject H_0.  
# Conclude median is different statistically for each group. There is a significant
# difference in the median mortalities for stroke and heart between races.


#################################################################
#Additional Exploratory Data Analysis
par(mfrow = c(2,2)) #can adjust this for 1,1 for individual view
rainbowcols <- rainbow(6,s = 0.5)
boxplot(mergedstrokeData$Deaths.per.100.000 ~ mergedstrokeData$Race.Ethnicity, xlab = "Race"
        , ylab = "Deaths per 100,000", col=c(rainbowcols), main = "Heart fatalities per 100,000 by Race (2013)")
boxplot(mergedheartData$Deaths.per.100.000 ~ mergedheartData$Race.Ethnicity, xlab = "Race"
        , ylab = "Deaths per 100,000", col=c(rainbowcols), main = "Stroke fatalities per 100,000 by Race (2013)")
#Boxplots to distinguish outliers and present visualization on significant
#differences between multiple race/ethnicities

cbp1 <- c("#003f5c","#58508d","#bc5090","#ff6361","#ffa600")
x <- ggplot(data = mergedheartData) +
  geom_bar(mapping = aes(x=region, y = Deaths.per.100.000, fill=Race.Ethnicity), position = "dodge", stat = "summary", fun = "median")
x + scale_fill_manual(values = cbp1) + labs(title = "Average Heart Fatalities for Race/Ethnicity by region in the U.S. (2013)",
                                            x = "Region", y = "Average Fatalities per 100,000",
                                            subtitle = "By Patrick Asztabski", fill = "Race/Ethnicity")

x <- ggplot(data = mergedstrokeData) +
  geom_bar(mapping = aes(x=region, y = Deaths.per.100.000, fill=Race.Ethnicity), position = "dodge", stat = "summary", fun = "median")
x + scale_fill_manual(values = cbp1) + labs(title = "Average Stroke Fatalities for Race/Ethnicity by region in the U.S. (2013)",
                                            x = "Region", y = "Average Fatalities per 100,000",
                                            subtitle = "By Patrick Asztabski", fill = "Race/Ethnicity")

#################################################################
#RACE/ETHNICITY DENSITY PLOTS FOR HEART
par(mfrow = c(3,2))
hearthist <- hist(whiteheartData$Deaths.per.100.000, breaks = 100, xlab="Deaths per 100,000",
                  ylab="Frequency",main="White",xlim=(c(0,900)), ylim=(c(0,350)), col = "#0bda51", cex.lab = 1.25, cex.axis = 1.25)
xfit <- seq(min(whiteheartData$Deaths.per.100.000), max(whiteheartData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(whiteheartData$Deaths.per.100.000), sd = sd(whiteheartData$Deaths.per.100.000))
yfit <- yfit*diff(hearthist$mids[1:2])*length(whiteheartData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

hearthist <- hist(BlackheartData$Deaths.per.100.000, breaks = 100, xlab="Deaths per 100,000",
                  ylab="Frequency",main="Black",xlim=(c(0,900)), ylim=(c(0,350)), col = "#ff5349", cex.lab = 1.25, cex.axis = 1.25)
xfit <- seq(min(BlackheartData$Deaths.per.100.000), max(BlackheartData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(BlackheartData$Deaths.per.100.000), sd = sd(BlackheartData$Deaths.per.100.000))
yfit <- yfit*diff(hearthist$mids[1:2])*length(BlackheartData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

hearthist <- hist(APIheartData$Deaths.per.100.000, breaks = 100, xlab="Deaths per 100,000",
                  ylab="Frequency",main="A/PI",xlim=(c(0,900)), ylim=(c(0,350)), col = "#008ecc", cex.lab = 1.25, cex.axis = 1.25)
xfit <- seq(min(APIheartData$Deaths.per.100.000), max(APIheartData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(APIheartData$Deaths.per.100.000), sd = sd(APIheartData$Deaths.per.100.000))
yfit <- yfit*diff(hearthist$mids[1:2])*length(APIheartData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

hearthist <- hist(AIANheartData$Deaths.per.100.000, breaks = 100, xlab="Deaths per 100,000",
                  ylab="Frequency",main="AI/AN",xlim=(c(0,900)), ylim=(c(0,350)), col = "#9370DB", cex.lab = 1.25, cex.axis = 1.25)
xfit <- seq(min(AIANheartData$Deaths.per.100.000), max(AIANheartData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(AIANheartData$Deaths.per.100.000), sd = sd(AIANheartData$Deaths.per.100.000))
yfit <- yfit*diff(hearthist$mids[1:2])*length(AIANheartData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

hearthist <- hist(HispanicheartData$Deaths.per.100.000, breaks = 100, xlab="Deaths per 100,000",
                  ylab="Frequency",main="Hispanic",xlim=(c(0,900)), ylim=(c(0,350)), col = "#EE82EE", cex.lab = 1.25, cex.axis = 1.25)
xfit <- seq(min(HispanicheartData$Deaths.per.100.000), max(HispanicheartData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(HispanicheartData$Deaths.per.100.000), sd = sd(HispanicheartData$Deaths.per.100.000))
yfit <- yfit*diff(hearthist$mids[1:2])*length(HispanicheartData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

#RACE/ETHNICITY DENSITY PLOTS FOR STROKE
par(mfrow = c(3,2))
strokehist <- hist(whitestrokeData$Deaths.per.100.000, breaks = 15, xlab=" White Deaths per 100,000",
                  ylab="Frequency",main="Histogram of Deaths per 100,000",xlim=(c(0,250)), ylim=(c(0,1000)), col = "#0bda51")
xfit <- seq(min(whitestrokeData$Deaths.per.100.000), max(whitestrokeData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(whitestrokeData$Deaths.per.100.000), sd = sd(whitestrokeData$Deaths.per.100.000))
yfit <- yfit*diff(strokehist$mids[1:2])*length(whitestrokeData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

strokehist <- hist(BlackstrokeData$Deaths.per.100.000, breaks = 15, xlab="Black Deaths per 100,000",
                  ylab="Frequency",main="Histogram of Deaths per 100,000",xlim=(c(0,250)), ylim=(c(0,1000)), col = "#ff5349")
xfit <- seq(min(BlackstrokeData$Deaths.per.100.000), max(BlackstrokeData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(BlackstrokeData$Deaths.per.100.000), sd = sd(BlackstrokeData$Deaths.per.100.000))
yfit <- yfit*diff(strokehist$mids[1:2])*length(BlackstrokeData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

strokehist <- hist(APIstrokeData$Deaths.per.100.000, breaks = 15, xlab="A/PI Deaths per 100,000",
                  ylab="Frequency",main="Histogram of Deaths per 100,000",xlim=(c(0,250)), ylim=(c(0,1000)), col = "#008ecc")
xfit <- seq(min(APIstrokeData$Deaths.per.100.000), max(APIstrokeData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(APIstrokeData$Deaths.per.100.000), sd = sd(APIstrokeData$Deaths.per.100.000))
yfit <- yfit*diff(strokehist$mids[1:2])*length(APIstrokeData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

strokehist <- hist(AIANstrokeData$Deaths.per.100.000, breaks = 15, xlab="AI/AN Deaths per 100,000",
                  ylab="Frequency",main="Histogram of Deaths per 100,000",xlim=(c(0,250)), ylim=(c(0,1000)), col = "#9370DB")
xfit <- seq(min(AIANstrokeData$Deaths.per.100.000), max(AIANstrokeData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(AIANstrokeData$Deaths.per.100.000), sd = sd(AIANstrokeData$Deaths.per.100.000))
yfit <- yfit*diff(strokehist$mids[1:2])*length(AIANstrokeData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

strokehist <- hist(HispanicstrokeData$Deaths.per.100.000, breaks = 15, xlab="Hispanic Deaths per 100,000",
                  ylab="Frequency",main="Histogram of Deaths per 100,000",xlim=(c(0,250)), ylim=(c(0,1000)), col = "#EE82EE")
xfit <- seq(min(HispanicstrokeData$Deaths.per.100.000), max(HispanicstrokeData$Deaths.per.100.000), length = 40)
yfit <- dnorm(xfit, mean=mean(HispanicstrokeData$Deaths.per.100.000), sd = sd(HispanicstrokeData$Deaths.per.100.000))
yfit <- yfit*diff(strokehist$mids[1:2])*length(HispanicstrokeData$Deaths.per.100.000)
lines(xfit,yfit,lwd=2, col = "black")

