remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
#READ IN DATA AND CLEAN NAMES
#################################################################
heartData <- read.csv("heart_updated.csv")
strokeData <- read.csv("stroke_updated.csv")

strokeData[strokeData == "American Indian and Alaskan Native"] <- "AI/AN"
strokeData[strokeData == "Asian and Pacific Islander"] <- "A/PI"
heartData[heartData == "American Indian and Alaskan Native"] <- "AI/AN"
heartData[heartData == "Asian and Pacific Islander"] <- "A/PI"

#SUBSETTING DATA FOR OUTLIER REMOVAL
#################################################################
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

#REMOVING OUTLIERS
#################################################################
whiteheartData$Deaths.per.100.000 <- remove_outliers(whiteheartData$Deaths.per.100.000)
whitestrokeData$Deaths.per.100.000 <- remove_outliers(whitestrokeData$Deaths.per.100.000)
whitestrokeData <- na.omit(whitestrokeData)
whiteheartData <- na.omit(whiteheartData) 

BlackheartData$Deaths.per.100.000 <- remove_outliers(BlackheartData$Deaths.per.100.000)
BlackstrokeData$Deaths.per.100.000 <- remove_outliers(BlackstrokeData$Deaths.per.100.000)
BlackheartData <- na.omit(BlackheartData) 
BlackstrokeData <- na.omit(BlackstrokeData) 

APIheartData$Deaths.per.100.000 <- remove_outliers(APIheartData$Deaths.per.100.000)
APIstrokeData$Deaths.per.100.000 <- remove_outliers(APIstrokeData$Deaths.per.100.000)
APIheartData <- na.omit(APIheartData)
APIstrokeData <- na.omit(APIstrokeData)

HispanicheartData$Deaths.per.100.000 <- remove_outliers(HispanicheartData$Deaths.per.100.000)
HispanicstrokeData$Deaths.per.100.000 <- remove_outliers(HispanicstrokeData$Deaths.per.100.000)
HispanicstrokeData <- na.omit(HispanicstrokeData) 
HispanicheartData <- na.omit(HispanicheartData)

AIANheartData$Deaths.per.100.000 <- remove_outliers(AIANheartData$Deaths.per.100.000)
AIANstrokeData$Deaths.per.100.000 <- remove_outliers(AIANstrokeData$Deaths.per.100.000)
AIANheartData <- na.omit(AIANheartData) 
AIANstrokeData <- na.omit(AIANstrokeData) 

#MERGING DATA
#################################################################
mergedheartData <- rbind(whiteheartData,BlackheartData, APIheartData, HispanicheartData, AIANheartData)
mergedstrokeData <- rbind(whitestrokeData,BlackstrokeData, APIstrokeData, HispanicstrokeData, AIANstrokeData)
#STATISTICAL TESTING MODEL
#################################################################
#Data_aovmodel <- aov(Stroke_DataClean$avg_glucose_level~Stroke_DataClean$smoking_status)
#Data_aovmodel
#summary(Data_aovmodel)

#################################################################
#ANOVA TEST

# Hypotheses from Mean Model
# H_0: mu_A/PI = mu_AI/AN = mu_Black = mu_Hispanic = mu_White
# H_1: at least one of the means is different

# ANOVA method
dataAnova <- aov(mergedheartData$Deaths.per.100.000 ~ mergedheartData$Race.Ethnicity)
dataAnova2 <- aov(mergedstrokeData$Deaths.per.100.000 ~ mergedstrokeData$Race.Ethnicity)
summary(dataAnova)
summary(dataAnova2)
# p-value is small for both tests, reject H_0.  
# Conclude at least one mean is different statistically. There is a significant
# difference in the mortality trend between races.

#################################################################
#Additional Exploratory Data Analysis
par(mfrow = c(1,1)) #can adjust this for 1,1 for individual view
boxplot(mergedstrokeData$Deaths.per.100.000 ~ mergedstrokeData$Race.Ethnicity, xlab = "Race"
        , ylab = "Deaths per 100,000")
boxplot(mergedheartData$Deaths.per.100.000 ~ mergedheartData$Race.Ethnicity, xlab = "Race"
        , ylab = "Deaths per 100,000")
#Boxplots to distinguish outliers and present visualization on significant
#differences between multiple race/ethnicities

case.vector = tapply(mergedheartData$Deaths.per.100.000, mergedheartData$Race.Ethnicity, sum)
case.vector2 = tapply(mergedstrokeData$Deaths.per.100.000, mergedstrokeData$Race.Ethnicity, sum)

case.vector #Total number of deaths for this year from heart disease
case.vector2

aggregate(mergedheartData$Deaths.per.100.000, list(mergedheartData$Race.Ethnicity), FUN=mean) #Average number of deaths
aggregate(mergedstrokeData$Deaths.per.100.000, list(mergedstrokeData$Race.Ethnicity), FUN=mean)
