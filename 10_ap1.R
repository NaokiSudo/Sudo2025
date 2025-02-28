library(tidyverse);library(lmtest);
library(modelsummary); library(MASS)

data1 <- read.csv("analysis.csv")
data1$trust <- 5 - data1$Q26s5
data1$policy <- 5 - data1$Q32s1

data1$rate_p <- data1$rate_p /100
data1$rate_v1 <- data1$rate_v /100
data1$rate_v2 <- data1$rate_v /100

data2 <- dplyr::select(data1, MID, Wave, trust, policy, rate_p, rate_v1, rate_v2)
summary(data2)

result1 <- polr(factor(trust)~rate_v1+rate_v1:rate_v2+rate_p, data2)
result2 <- polr(factor(policy)~rate_v1+rate_v1:rate_v2+rate_p, data2)

modelsummary(list(result1, result2), stars = T, fmt=3)