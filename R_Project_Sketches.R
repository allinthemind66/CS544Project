accidents <- read.csv("/Users/ricknilon/Downloads/CRSS2018CSV/ACCIDENT.csv")
person <- read.csv("/Users/ricknilon/Downloads/CRSS2018CSV/PERSON.csv")
head(person)
#install.packages("dplyr")
#install.packages("plyr")
#library(dplyr)
#library(plyr)
library(ggplot2)
library(plotly)
require(gridExtra)
#head(accidents)

# Alcohol Use In Accidents
alcoholInvolvementDF <- data.frame(table(accidents$ALCHL_IM))
alcoholInvolvementDF
alcoholUseInAccidentsPlot <- ggplot(alcoholInvolvementDF, aes(Var1, Freq)) + 
  geom_bar(stat="identity") + 
  labs( x="Day Of Week", title="Alcohol Use In Accidents") 
alcoholUseInAccidentsPlot


# Hour of Day Of Accidents
hourOfDayDF <- data.frame(table(accidents$HOUR_IM))
hourOfDayDF
accidentsByHourPlot <- ggplot(hourOfDayDF, aes(Var1, Freq)) + 
  geom_bar(stat="identity") + 
  labs( x="Day Of Week", title="Accidents By Hour Of Day") 
accidentsByHourPlot
rownames(hourOfDayDF)
summary(hourOfDayDF)
hourOfDayDF
accidents$HOUR_IM
table(accidents$HOUR_IM)
df <- data.frame(table(accidents$HOUR_IM))
df
plot_ly(y = table(accidents$HOUR_IM), type = "box")
table(accidents$HOUR_IM)

# Day Of Week Of Accidents
dayOfWeekDF <- data.frame(table(accidents$DAY_WEEK ))
dayOfWeekDF
accidentsByDayOfWeek <- ggplot(dayOfWeekDF, aes(Var1, Freq)) + 
  geom_bar(stat="identity") + 
  labs( x="Day Of Week", title="Accidents By Day Of Week") 

accidentsByDayOfWeek


# Alcohol Use vs Accidents By Hour Table
alcoholUseByHourOfDay <- table(accidents$ALCHL_IM,accidents$HOUR_IM)
alcoholUseByHourOfDay
alcoholUseByHourOfDayDF <- data.frame(table(accidents$ALCHL_IM,accidents$HOUR_IM))
alcoholUseByHourOfDayDF
xlab("Hour Of Day") # for the x axis label
alcoholUseByHourOfDayPlot <- ggplot(alcoholUseByHourOfDayDF, aes(Var2, Freq, color = Var1)) + 
  geom_bar(stat="identity") + 
  labs(colour = "Alcohol Use",fill = "Dose (mg)", x="Hour Of Day", title="Alcohol Use By Hour Of Day") +
  scale_color_manual(labels = c("Yes", "No"), values = c("blue", "red")) 

alcoholUseByHourOfDayPlot


# Note: add line to bar plot for proportionality?
# Note: show only alcohol use

# Alcohol Use vs Accidents By Week Table

alcoholUseByWeekDF <- data.frame(table(accidents$ALCHL_IM,accidents$DAY_WEEK))
alcoholUseByWeekDF

alcoholUseByWeekPlot <- ggplot(alcoholUseByWeekDF, aes(Var2, Freq, color = Var1)) + 
  geom_bar(stat="identity") + 
  labs(colour = "Alcohol Use",fill = "Dose (mg)", x="Day Of Week", title="Alcohol Use By Day Of Week") +
  scale_color_manual(labels = c("Yes", "No"), values = c("blue", "red")) +
scale_x_discrete(labels= c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
alcoholUseByWeekPlot
alcByAllAccidentsDF <- data.frame(table(accidents$ALCHL_IM,accidents$HOUR_IM))
alcByAllAccidentsDF

alcByAllAccidentsDF <- data.frame(table(accidents$ALCHL_IM,accidents$HOUR_IM))
alcByAllAccidentsDF$Var1
#####

alcoholUseByWeekDF <- data.frame(table(accidents$ALCHL_IM,accidents$DAY_WEEK))
alcoholUseByWeekDF

alcUse<- alcoholUseByWeekDF[alcoholUseByWeekDF$Var1 == 1,]
noAlcUse <- alcoholUseByWeekDF[alcoholUseByWeekDF$Var1 == 2,]
newDataFrame <- data.frame(noAlcUse$Var2, noAlcUse$Freq, alcUse$Freq)

daysOfWeek <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
noAlcUseVector <- noAlcUse$Freq
alcUseVector <- alcUse$Freq
newDataFrame <- data.frame(daysOfWeek, noAlcUseVector, alcUseVector)
alcPlot <- plot_ly(
  newDataFrame,
  x = ~daysOfWeek,
  y = ~noAlcUseVector,
  name = "No Alcohol Use",
  type = "bar",
  width = 600
) 
alcPlot <- alcPlot %>% add_trace(y = ~alcUseVector, name = 'Alcohol Use')
alcPlot <- alcPlot %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
alcPlot
#######

alcUse<- alcByAllAccidentsDF[alcByAllAccidentsDF$Var1 == 1,]
noAlcUse <- alcByAllAccidentsDF[alcByAllAccidentsDF$Var1 == 2,]
newDataFrame <- data.frame(noAlcUse$Var2, noAlcUse$Freq, alcUse$Freq)

hours <- c(0:23)
noAlcUseVector <- noAlcUse$Freq
alcUseVector <- alcUse$Freq
newDataFrame <- data.frame(hours, noAlcUseVector, alcUseVector)
alcPlot <- plot_ly(
  newDataFrame,
  x = ~hours,
  y = ~noAlcUseVector,
  name = "No Alcohol Use",
  type = "bar",
  width = 600
) 
alcPlot <- alcPlot %>% add_trace(y = ~alcUseVector, name = 'Alcohol Use')
alcPlot <- alcPlot %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
alcPlot
########

# Alcohol Use By Week Table
accidents$DAYS_OF_WEEK_TEXT <- mapvalues(accidents$DAY_WEEK,
                                         from = c(1,2,3,4,5,6,7),
                                         to = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

alcoholUseByDayOfWeekDF <- data.frame(table(accidents$DAY_WEEK[which(accidents$ALCHL_IM == 1)]))
alcoholUseByDayOfWeekDF
alcoholUseByDayOfWeekPlot <- ggplot(alcoholUseByDayOfWeekDF, aes(Var1, Freq)) + 
  geom_bar(stat="identity") + 
  labs(fill = "Dose (mg)", x="Day Of Week", title="Alcohol Use By Day Of Week") +
  scale_x_discrete(labels= c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
alcoholUseByDayOfWeekPlot

# Alcohol Use By Hour
alcoholUseByHourDF <- data.frame(table(accidents$HOUR_IM[which(accidents$ALCHL_IM == 1)]))
alcoholUseByHourDF


xlab("Hour Of Day") # for the x axis label
ggp <- ggplot(alcoholUseByHourDF, aes(Var1, Freq)) + 
  geom_bar(stat="identity") + 
  labs(colour = "Alcohol Use",fill = "Dose (mg)", x="Hour Of Day", title="Alcohol Use By Hour Of Day") +
  scale_color_manual(labels = c("Yes", "No"), values = c("blue", "red")) 

ggp

weather <- table(accidents$WEATHR_IM)
weather
rownames(weather) <- c("Clear", 
                       "Rain", 
                       "Sleet Or Hail", 
                       "Snow", 
                       "Fog, Smog, Smoke", 
                       "Severe Crosswinds",
                       "Blowing Sand, Soil, Dirt",
                       "Other",
                       "Cloudy",
                       "Blowing Snow",
                       "Freezing Rain or Drizzle"
                       )

weatherDF <- data.frame(weather)
colnames(weatherDF) <- c("Condition", "Freq")
alcoholAccidentsByWeatherConditionDF <- data.frame(table(accidents$WEATHR_IM[which(accidents$ALCHL_IM == 1)]))

alcoholAccidentsByWeatherConditionPlot <- ggplot(alcoholAccidentsByWeatherConditionDF, aes(Var1, Freq)) + 
  geom_bar(stat="identity") + 
  labs(fill = "Dose (mg)", x="Day Of Week", title="Alcohol Use By Day Of Week") +
  scale_x_discrete(labels= c("Clear", 
                             "Rain", 
                             "Sleet Or Hail", 
                             "Snow", 
                             "Fog, Smog, Smoke", 
                             "Severe Crosswinds",
                             "Blowing Sand, Soil, Dirt",
                             "Other",
                             "Cloudy",
                             "Blowing Snow",
                             "Freezing Rain or Drizzle"
  ))
alcoholAccidentsByWeatherConditionPlot

# Severity of accidents involving alcohol use
# MAX SEVERITY IMPUTED

# Central Limit Theorem On Age

barplot(table(person$AGE_IM))
popMean <- mean(person$AGE_IM)
# => 37.33053
popMean
# => 108680.9
popSd <- sd(person$AGE_IM)
popSd


samples <- 10000
sample.size <- 50


xbar <- numeric(samples)

for(i in 1:samples) {
  xbar[i] <- mean(sample(person$AGE_IM, size=sample.size, replace = F))
}
xbar
plot_ly(x =xbar, type = "histogram")
hist(xbar)
ageSampleMean <- mean(xbar)
ageSampleMean
ageSampleSD <- sd(xbar)
ageSampleSD


barplot(table(person$SEAT_IM))

# 11 is driver side
boxplot(table(person$AGE_IM[which(person$SEAT_IM == 11)]))
which(person$SEAT_IM == 11)
which(person$PER_TYP == 1)
boxplot(table(person$AGE_IM[which(person$PER_TYP == 1)]))
head(person)


# Alcohol Use In Driver Side Person
df<-data.frame(table(person$PERALCH_IM[which(person$SEAT_IM == 11)], person$AGE_IM[which(person$SEAT_IM == 11)]))
df
colnames(df) <- c("AlcoholUsed", "Age", "Freq")

accidentWithAlcoholUseByDriverAgePlot <- ggplot(df, aes(Age, Freq, color = AlcoholUsed)) + 
  geom_bar(stat="identity") + 
  labs(colour = "Alcohol Use",fill = "Dose (mg)", x="Day Of Week", title="Alcohol Use By Age Of Driver") +
  scale_color_manual(labels = c("No", "Yes"), values = c("blue", "red"))
accidentWithAlcoholUseByDriverAgePlot


driverSidePassgenger <-person[which(person$SEAT_IM == 11), ]

df<-data.frame(table(driverSidePassgenger$AGE_IM[which(person$PERALCH_IM == 1)]))
colnames(df) <- c("Age", "Freq")
alcoholUseByDriverAgePlot <- ggplot(df, aes(Age, Freq)) + 
  geom_bar(stat="identity") + 
  labs(fill = "Dose (mg)", x="Day Of Week", title="Alcohol Use By Age Of Driver")
alcoholUseByDriverAgePlot

driver <-person[which(person$SEAT_IM == 11), ]

df<-data.frame(table(driver$AGE_IM[which(person$PERALCH_IM == 1)]))
colnames(df) <- c("Age", "Freq")
alcoholUseByDriverAgePlot <- ggplot(df, aes(Age, Freq)) + 
  geom_bar(stat="identity") + 
  labs(fill = "Dose (mg)", x="Day Of Week", title="Alcohol Use By Age Of Driver")
alcoholUseByDriverAgePlot

plot1 <- qplot(1)
plot2 <- qplot(1)
grid.arrange(plot1, plot2, ncol=2)



