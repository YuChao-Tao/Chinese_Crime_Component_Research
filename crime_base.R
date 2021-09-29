rm(list = ls())
library(ggplot2)
library(ggpubr)
setwd("/Users/tyc_219/Desktop/")

allcrime <- read.csv("research/ChineseCrime.csv", header = T)
crime_struc <- allcrime[ ,1:10]
teencrime_struc <- allcrime[ ,c(1,11,12)]

proportion <- function(data,row,volumn){
  for (i in 1:row){
    tol <- sum(data[i, 2:volumn])
    data[i, 2:volumn] <- data[i, 2:volumn]/tol
  }
  return(data)
}

crime_per <- proportion(crime_struc, row = 17, volumn = 10)
teencrime_per <- proportion(teencrime_struc, row = 17, volumn = 3)

a0 <- data.frame(Year = teencrime_per[, 1], value = teencrime_per[, 2])
a1 <- data.frame(Year = teencrime_per[, 1], value = teencrime_per[, 3])
a2 <- data.frame(classify = c(rep("0-18",17), rep("18-25",17)))
teencrime_per0 <- rbind(a0, a1)
teencrime_per1 <- cbind(teencrime_per0, a2)
teenpic1 <- ggplot(teencrime_per1, aes(x = Year, y = value, fill = classify)) + 
  geom_bar(stat = "identity") 
teenpic1

crimepic1 <- ggplot(crime_struc, aes(x=Year, y=Hazardous_public_safety)) +
  geom_line() + geom_point()
crimepic1





