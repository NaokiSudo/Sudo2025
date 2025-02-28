library(ggplot2)
library(gridExtra)

data1 <- data.frame(matrix(NA,nrow=101, ncol = 4))

for (i in 1:101) {
  data1$X1[i] <- i
  data1$X2[i] <-  (data1$X1[i]-51)^2/2500
  data1$X3[i] <- -(data1$X1[i]-51)^2/2500+1.00
}

head(data1)

f1 <- ggplot(data1, aes(X1, X2))+geom_line()+
      xlab("%Vaccination Rate")+ylab("Trust in Government")+
      scale_x_continuous(breaks=seq(0,100, 10))+
      scale_y_continuous(breaks=seq(0,1.00, 0.1))+
      ggtitle("D_i > P_i")
f1

ggsave("figure4top.png", dpi=400)

f2 <- ggplot(data1, aes(X1, X3))+geom_line()+
      xlab("%Vaccination Rate") +ylab("Trust in Government")+
      scale_x_continuous(breaks=seq(0,100, 10))+
      scale_y_continuous(breaks=seq(0,1.00, 0.1))+ 
      ggtitle("D_i < P_i")
f2

ggsave("figure4bottom.png", dpi=400)

