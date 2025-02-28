data1 <- read.csv("SSPW21_panel_long_final.csv")
data2 <- read.csv("Rate.csv")


data1$rate_p <- NA  
data1$rate_s <- NA
data1$rate_d <- NA
data1$rate_v <- NA

for (i in 1:nrow(data1)) {
  for (j in 1:47) {
    
  if(data1$Wave[i]==1 & data1$F3[i]==j){data1$rate_p[i] <- data2$rate1_p[j]
    }else{ }
  if(data1$Wave[i]==2 & data1$F3[i]==j){data1$rate_p[i] <- data2$rate2_p[j]
    }else{ }
  if(data1$Wave[i]==3 & data1$F3[i]==j){data1$rate_p[i] <- data2$rate3_p[j]
    }else{ }
  if(data1$Wave[i]==4 & data1$F3[i]==j){data1$rate_p[i] <- data2$rate4_p[j]
    }else{ }

  if(data1$Wave[i]==1 & data1$F3[i]==j){data1$rate_s[i] <- data2$rate1_s[j]
    }else{ }
  if(data1$Wave[i]==2 & data1$F3[i]==j){data1$rate_s[i] <- data2$rate2_s[j]
    }else{ }
  if(data1$Wave[i]==3 & data1$F3[i]==j){data1$rate_s[i] <- data2$rate3_s[j]
    }else{ }
  if(data1$Wave[i]==4 & data1$F3[i]==j){data1$rate_s[i] <- data2$rate4_s[j]
    }else{ }   
        
  if(data1$Wave[i]==1 & data1$F3[i]==j){data1$rate_d[i] <- data2$rate1_d[j]
    }else{ }
  if(data1$Wave[i]==2 & data1$F3[i]==j){data1$rate_d[i] <- data2$rate2_d[j]
    }else{ }
  if(data1$Wave[i]==3 & data1$F3[i]==j){data1$rate_d[i] <- data2$rate3_d[j]
    }else{ }
  if(data1$Wave[i]==4 & data1$F3[i]==j){data1$rate_d[i] <- data2$rate4_d[j]
    }else{ }   

  if(data1$Wave[i]==1 & data1$F3[i]==j){data1$rate_v[i] <- data2$rate1_v[j]
    }else{ }
  if(data1$Wave[i]==2 & data1$F3[i]==j){data1$rate_v[i] <- data2$rate2_v[j]
    }else{ }
  if(data1$Wave[i]==3 & data1$F3[i]==j){data1$rate_v[i] <- data2$rate3_v[j]
    }else{ }
  if(data1$Wave[i]==4 & data1$F3[i]==j){data1$rate_v[i] <- data2$rate4_v[j]
    }else{ }       
  }
}

write.csv(data1, "analysis.csv")

