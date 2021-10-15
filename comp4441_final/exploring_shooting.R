library(tidyverse)
library(ggpubr)
library(ggplot2)
dat<-read.csv("/Users/abbyward/Documents/DU coursework/stats 4441/policeshooting.csv")

countState <- table(dat$state)
barplot(countState, main="State Distribution",
        xlab="Number of police shootings")

countDate <- table(dat$date)
barplot(countDate, main="Date Distribution",
        xlab="Number of police shootings")

countAge <- table(dat$age)
barplot(countAge, main="Age Distribution",
        xlab="Number of police shootings")

countArmed <- table(dat$armed)
barplot(countArmed, main="Armed Distribution",
        xlab="Number of police shootings")
gun<-("gun")
unarmed<-("unarmed")
dat$group <- with(dat, ifelse(armed %in% gun, "gun",
                            ifelse(armed %in% unarmed, "unarmed", "other" )))
countArmed.grouped <- table(dat$group)
barplot(countArmed.grouped, main="Armed Distribution",
        xlab="Number of police shootings")




countDate <-transform(table(dat$date))
df <- as.data.frame(countDate)

plot(df$Var1, df$Freq, main="Date Distribution",
        xlab="Number of police shootings")
