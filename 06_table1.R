library(dplyr)
library(psych)
library(stargazer)

data1 <- read.csv("analysis.csv")

data1$trust <- 5 - data1$Q26s5
data1$central <- 5 - data1$Q32s1
data1$rate_p <- data1$rate_p /100
data1$rate_v <- data1$rate_v /100
data1$age <- data1$F2
data1$female <- data1$F1-1

data2 <- select(data1, MID, trust, central, Wave, 
                rate_p, rate_v, age, female)

data2W1 <- filter(data2, Wave==1 )
data2W2 <- filter(data2, Wave==2 )
data2W3 <- filter(data2, Wave==3 )
data2W4 <- filter(data2, Wave==4 )
data2all <- data2
  
data2W1 <- select(data2W1, trust, central,  
                rate_p, rate_v, age, female) 
data2W2 <- select(data2W2, trust, central,  
                  rate_p, rate_v, age, female)
data2W3 <- select(data2W3, trust, central,  
                  rate_p, rate_v, age, female)
data2W4 <- select(data2W4, trust, central,  
                  rate_p, rate_v, age, female)
 
d1 <- describe(data2W1)[,c("n","mean", "median", "sd", "min", "max")]
d2 <- describe(data2W2)[,c("n","mean", "median", "sd", "min", "max")]
d3 <- describe(data2W3)[,c("n","mean", "median", "sd", "min", "max")]
d4 <- describe(data2W4)[,c("n","mean", "median", "sd", "min", "max")]

print(d1, digits=3)
print(d2, digits=3)
print(d3, digits=3)
print(d4, digits=3)

cor.test(data2all$trust, data2all$central)

