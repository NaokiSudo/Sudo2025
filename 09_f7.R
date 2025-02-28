library(tidyverse);library(plm);library(lmtest)
library(modelsummary);library(marginaleffects)

data1 <- read.csv("analysis.csv")
data1$policy <- 5 - data1$Q32s1
data1$gov <- 5 - data1$Q26s5
data1$mass <- 5 - data1$Q26s1
data1$socialmedia <- 5 - data1$Q26s6

data1$rate_p <- data1$rate_p /100
data1$rate_v1 <- data1$rate_v /100
data1$rate_v2 <- data1$rate_v /100

data2 <- select(data1, MID, Wave, policy, gov, mass, socialmedia, rate_p, rate_v1, rate_v2)

data3 <- pdata.frame(data2, index = c("MID", "Wave"))

p1 <- plm(policy ~ rate_v1+rate_v1:rate_v2+rate_p,
          data3, model = "within", effects="twoways")
p2 <- plm(gov ~ rate_v1+rate_v1:rate_v2+rate_p,
          data3, model = "within", effects="twoways")
p3 <- plm(mass ~ rate_v1+rate_v1:rate_v2+rate_p,
          data3, model = "within", effects="twoways")
p4 <- plm(socialmedia ~ rate_v1+rate_v1:rate_v2+rate_p,
          data3, model = "within", effects="twoways")

model1 <- modelsummary(list(p1,p2, p3, p4), stars = T, fmt = 3, 
             gof_omit = 'R2|RMSE')
model1

cm <- c('rate_v1'='Rate',
        'rate_v1:rate_v2'='Rate^2')
models <- list(
  "Ifection Control Policies" = p1,
  "Central Government" = p2,
  "Mass Media" = p3,
  "Social Media" = p4)



model2 <- modelplot(models, 
                    background = list(geom_vline(xintercept = 0, color = 'red')),
                    coef_map = cm)+scale_color_manual(values = c('#000000', '#333333', '#777777','#999999'))
model2 
ggsave("figure7.png")

