library(tidyverse);library(plm);library(lmtest)
library(modelsummary);library(marginaleffects);library(mice)

data1 <- read.csv("analysis2.csv")
data1$trust <- 5 - data1$Q26s5
data1$policy <- 5 - data1$Q32s1

data1$rate_p <- data1$rate_p /100
data1$rate_v1 <- data1$rate_v /100
data1$rate_v2 <- data1$rate_v /100

data2 <- dplyr::select(data1, MID, Wave, trust, policy, 
                rate_p, rate_v1, rate_v2)

data3 <- pdata.frame(data2, index = c("MID", "Wave"))

cluster_n <- length(unique(data3$MID)) #Number of clusters
correct <- cluster_n/(cluster_n - 1)

p00 <- plm(trust~rate_v1+rate_v1:rate_v2+rate_p, data3, effects="twoways", model = "within")
summary(p00)

p01 <- plm(trust~rate_v1+rate_v1:rate_v2+rate_p+policy, data3, effects="twoways", model = "within")
summary(p01)
# AIC(p01)
# BIC(p01)
coeftest(p01, vcovHC(p01, method="arellano", type="HC1", 
                    cluster="group")*correct) 

predictormatrix<-quickpred(data3,include=c("policy","trust", "rate_p", "rate_v1", "rate_v2"),
                           exclude=c("MID", "Wave"), mincor = 0.1)

nrow(data3)

data4 <- mice(data3, predictorMatrix = predictormatrix,
              m=5,maxit=5,meth='pmm', seed=1234)

p1 <- with(data4,plm(trust ~ rate_v1,
                     data = data.frame(mget(ls())),index = c("MID","Wave"),
                     effects = "individual", model = "within")) 

p2 <- with(data4, plm(trust ~ rate_v1+rate_v1:rate_v2,
                      data=data.frame(mget(ls())), index = c("MID","Wave"),
                      model = "within", effects="individual"))

p3 <- with(data4, plm(trust ~ rate_v1+rate_v1:rate_v2+rate_p,
                      data = data.frame(mget(ls())), index = c("MID","Wave"),model = "within", effects="individual"))

summary(pool(p1))
summary(pool(p2))
summary(pool(p3))

