library(tidyverse);library(plm);library(lmtest)
library(modelsummary);library(marginaleffects)

data1 <- read.csv("analysis.csv")
data1$trust <- 5 - data1$Q26s5

data1$rate_p <- data1$rate_p /100
data1$rate_v1 <- data1$rate_v /100
data1$rate_v2 <- data1$rate_v /100

data2 <- select(data1, MID, Wave, trust, 
                rate_p, rate_v1, rate_v2)

data3 <- pdata.frame(data2, index = c("MID", "Wave"))
data3 <- filter(data3, !is.na(trust))

cluster_n <- length(unique(data3$MID)) #Number of clusters
correct <- cluster_n/(cluster_n - 1)

p1 <- plm(trust ~ rate_v1,
          data3, model = "within", effects="twoways")
coeftest(p1, vcovHC(p1, method="arellano", type="HC1", 
                    cluster="group")*correct) 

p2 <- plm(trust ~ rate_v1+rate_v1:rate_v2,
          data3, model = "within", effects="twoways")
coeftest(p2, vcovHC(p2, method="arellano", type="HC1", 
                    cluster="group")*correct) 

p3 <- plm(trust ~ rate_v1+rate_v1:rate_v2+rate_p,
          data3, model = "within", effects="twoways")
coeftest(p3, vcovHC(p3, method="arellano", type="HC1", 
                    cluster="group")*correct) 

model1 <- modelsummary(list(p1,p2,p3), stars = T, fmt = 3, 
             gof_omit = 'R2|RMSE')
model1

pred <- predict(p3)
rate <- data3$rate_v1
d2 <- data.frame(bind_cols(rate, pred))

g2 <- ggplot(d2, aes(rate, pred))+geom_smooth()+
  labs(x="Vaccinated Rate", y="Predicted Trust in the Government")+
  scale_x_continuous(breaks = seq(0, 0.9, 0.1))+
  scale_y_continuous(breaks = seq(1.7, 2.1, 0.05))
g2

ggsave("figure6.png", dpi=500)

