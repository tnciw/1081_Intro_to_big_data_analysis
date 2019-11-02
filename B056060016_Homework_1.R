#B056060016 政經系109級 陳奕瑋 1081巨量HW1

#1 done
library(readr)
library(reshape)
library(dplyr)
library(tidyr)
gtstock = read_csv("Stock_Data.csv")

stock_long = melt(data = stock, id.vars = c("security_id", "type"))
stock_merge = dcast(stock_long, formula = security_id+date ~ type)
#sid & date as a key

#melt can reduce variables, making dataframe be a longer but thinner data.
#dcast can produce more varivables, conversely.

stock_merge_date = separate(stock_merge, "date",c("year", "month", "day"), sep = "/")
stock_merge_date = stock_merge_date[(stock_merge_date$security_id == 2105 
                                     & stock_merge_date$year == 2015
                                     & stock_merge_date$close - stock_merge_date$open >= 2),]
stock_merge_date = stock_merge_date[order(stock_merge_date$day),]

#2 done
library(PASWR)
library(sqldf)
titanic = data.frame(titanic3)

sqldf("SELECT COUNT(*) FROM titanic WHERE sex == 'female' AND embarked == 'Southampton'")
sqldf("SELECT COUNT(*) FROM titanic WHERE name LIKE 'A%'")

#3
library(MASS)
#library(ggplot2)
survey = data.frame(survey)

#3.1
sum(is.na(survey))

#3.2
survey = na.omit(survey)

#3.3
plot(survey$Exer)

#3.4
survey$catgory = ifelse(survey$Height >=0 & survey$Height <=160,'low',
                              ifelse(survey$Height >= 160 & survey$Height <=170,'med',
                                ifelse(survey$Height >= 170 & survey$Height <=180,'high','ultra_high')))

chi = table(survey$Exer, survey$Height)
summary(chi)

#Number of cases in table: 168 
#Number of factors: 2 
#Test for independence of all factors:
#  Chisq = 131.42, df = 126, p-value = 0.3524
#  Chi-squared approximation may be incorrect

#4
diamonds = read_csv("diamonds.csv")

#4.1
anova = aov(price ~ ., data = diamonds)
summary(anova)

#Pr of store to price is <2e-16
#Pr of channel to price is 0.00961
#As a result, we can conclude that both store and channel are important variable to explain price.

#4.2

