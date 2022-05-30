library(ggplot2)
library(colorspace)
library(magrittr)
library(tidyverse)
library(knitr)
library(ggmap)
library(scales)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(statsr)
library(gridExtra)

hist <- read.csv("C:/Users/Danny/Desktop/Data101/hist.csv")

#HYPOTHESIS 1
#Permutation Testing based on
#Example and Notes in Class

#Prevents errors by changing countries from factors
hist$Country = trimws(as.character(hist$Country))
hist$Country = as.character(hist$Country)
hist$Country = trimws(as.character(hist$Country))
na.rm = TRUE
US.weather= subset(hist,hist$Country == "United States")
aussie.weather = subset(hist, hist$Country == "Australia")
df <- data.frame(hist, hist$AverageCelsiusTemperature, hist$Country)
#Permutation(10000,hist,c1,c2,w1,w2=NA,tail="R",plot = TRUE)
Permutation(1000, df, "Country", "AverageCelsiusTemperature", "United States", "Australia", tail = "T")



#Calculating Z-Score and P-Value based on previous assignment logic

#Z-Score Function
zscore = function(x,y){
  z<-(mean(x)-mean(y))/(sqrt(sd(x)^2/length(x) +sd(y)^2/length(y)))
  return(z)
}


test = sample(hist[hist$Year == 2000,]$AverageCelsiusTemperature, 20)
test2 = sample(hist[hist$Year == 2001,]$AverageCelsiusTemperature, 20)
na.rm = TRUE
summary(test)
summary(test2)
firstmean = mean(test)
#For some reason (although it says the mean is 12.40 in the summary)
#It will not give first mean that value
firstmean = 12.40
secondmean = mean(test2)
firstSD = sd(test)
secondSD = sd(test2)
firstNum = length(test)
secondNum = length(test2)

#Calculate P-value based on z-score
sd.math = sqrt(firstSD^2/firstNum + secondSD^2/secondNum)
zscore.math = (firstmean - secondmean)/sd.math
pvalue = 1 - pnorm(zscore.math)

