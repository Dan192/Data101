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


#import data file from canvas
citibike1 <- read.csv("C:/Users/Danny/Desktop/Data101/Data/citibike.May20.50K.csv")
citibike2 <- read.csv("C:/Users/Danny/Desktop/Data101/Data/citibike.May21.50K.csv")

zscore = function(x,y){
  z<-(mean(x)-mean(y))/(sqrt(sd(x)^2/length(x) +sd(y)^2/length(y)))
  return(z)
}

#Plot 1
data = data.frame(citibike1)
ggplot(data, aes(x = citibike1$birth.year, y = citibike1$tripduration)) + geom_point(size = 3, shape = 20) + ggtitle("Citibike trip duration based on age") + labs(x = "Age", y = "Trip Duration") + theme(plot.title = element_text(hjust = 0.5))

#First Hypothesis
#change the genders to 1,2, and 3
citibike1$gender <- ifelse(citibike1$gender==1,"Male",ifelse(citibike1$gender==2,"Female","Unknown"))
#Find the mean, median of age
summary(citibike1$age)
citibike1$age = 2021 - citibike1$birth.year

#calculate z score between trip duration and age
z_score = zscore(citibike1$tripduration, ages)
#calculate p-value based on z-value
pvalue = 1 - pnorm(z_score, mean = 0, sd = 1)


#Plot 2
citibike1 %>% ggplot(aes(x = usertype, fill = usertype)) + geom_bar(color = "red") + ggtitle("Total Number of purchases based on UserType") + labs(x= "Citibike User Type", y= "Number of Users")

#Second Hypothesis
#Logic as seen from
#Our recitation Files
customer = subset(citibike1, citibike1$usertype == "Customer")
subscriber = subset(citibike1, citibike1$usertype == "Subscriber")
customermean = mean(customer$age)
subscribermean = mean(subscriber$age)
sd.customer = sd(customer$age)
sd.subscriber = sd(subscriber$age)
num.customer = length(customer$age)
num.subscriber = length(subscriber$age)
#I used a different method for calculating the z-score since I am using a more specific subset
sd.customer.math = sqrt(sd.customer^2/num.customer + sd.subscriber^2/num.subscriber)
zscore.math = (subscribermean - customermean)/sd.customer.math
pvalue2 = 1 - pnorm(zscore.math)
