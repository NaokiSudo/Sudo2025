library(ndjson)
library(tidyverse)
library(tidyr)

pdf <- read.csv("population.csv")
df <- stream_in("prefecture.ndjson")
df$prefecture <- as.numeric(df$prefecture)

df1 <- filter(df, status==1 & prefecture==1)
df1 <- group_by(df1, date, prefecture)
df1 <- data.frame(summarize(df1, sum=sum(count)))

for (i in 2:nrow(df1)) {
  df1$sum[i] <- df1$sum[i]+df1$sum[i-1]
}

df1$rate <- df1$sum/pdf$value[1]
dfT <- df1


for (j in 2:47) {

df2 <- filter(df, status==1 & prefecture==j)
df2 <- group_by(df2, date, prefecture)
df2 <- data.frame(summarize(df2, sum=sum(count)))

for (i in 2:nrow(df2)) {
  df2$sum[i] <- df2$sum[i]+df2$sum[i-1] 
}

df2$rate <- df2$sum/pdf$value[j]
dfT <- bind_rows(dfT, df2)

}

pdf$prefecture <- pdf$PID

dfTT <- left_join(dfT, pdf, by="prefecture")
head(dfTT)

dfTT <- select(dfTT, date, Prefecture, rate)

dfTT2 <- group_by(dfTT, date)

dfTT <- spread(dfTT, Prefecture, rate)

dfTT$Tottori[1] <- 0
dfTT$Yamanashi[1] <- 0

for (i in 2:nrow(dfTT)) {
  if(is.na(dfTT$Aichi[i])){dfTT$Aichi[i] <- dfTT$Aichi[i-1]}else{}
  if(is.na(dfTT$Akita[i])){dfTT$Akita[i] <- dfTT$Akita[i-1]}else{}
  if(is.na(dfTT$Aomori[i])){dfTT$Aomori[i] <- dfTT$Aomori[i-1]}else{}
  if(is.na(dfTT$Chiba[i])){dfTT$Chiba[i] <- dfTT$Chiba[i-1]}else{}    
  if(is.na(dfTT$Ehime[i])){dfTT$Ehime[i] <- dfTT$Ehime[i-1]}else{}
  if(is.na(dfTT$Fukui[i])){dfTT$Fukui[i] <- dfTT$Fukui[i-1]}else{}
  if(is.na(dfTT$Fukuoka[i])){dfTT$Fukuoka[i] <- dfTT$Fukuoka[i-1]}else{}
  if(is.na(dfTT$Fukushima[i])){dfTT$Fukushima[i] <- dfTT$Fukushima[i-1]}else{}   
  if(is.na(dfTT$Gifu[i])){dfTT$Gifu[i] <- dfTT$Gifu[i-1]}else{}
  if(is.na(dfTT$Gunma[i])){dfTT$Gunma[i] <- dfTT$Gunma[i-1]}else{}
  if(is.na(dfTT$Hiroshima[i])){dfTT$Hiroshima[i] <- dfTT$Hiroshima[i-1]}else{}
  if(is.na(dfTT$Hokkaido[i])){dfTT$Hokkaido[i] <- dfTT$Hokkaido[i-1]}else{}    
  if(is.na(dfTT$Hyogo[i])){dfTT$Hyogo[i] <- dfTT$Hyogo[i-1]}else{}
  if(is.na(dfTT$Ibaraki[i])){dfTT$Ibaraki[i] <- dfTT$Ibaraki[i-1]}else{}
  if(is.na(dfTT$Ishikawa[i])){dfTT$Ishikawa[i] <- dfTT$Ishikawa[i-1]}else{}
  if(is.na(dfTT$Iwate[i])){dfTT$Iwate[i] <- dfTT$Iwate[i-1]}else{}  
  if(is.na(dfTT$Kagawa[i])){dfTT$Kagawa[i] <- dfTT$Kagawa[i-1]}else{}
  if(is.na(dfTT$Kagoshima[i])){dfTT$Kagoshima[i] <- dfTT$Kagoshima[i-1]}else{}
  if(is.na(dfTT$Kanagawa[i])){dfTT$Kanagawa[i] <- dfTT$Kanagawa[i-1]}else{}
  if(is.na(dfTT$Kochi[i])){dfTT$Kochi[i] <- dfTT$Kochi[i-1]}else{}    
  if(is.na(dfTT$Kumamoto[i])){dfTT$Kumamoto[i] <- dfTT$Kumamoto[i-1]}else{}
  if(is.na(dfTT$Kyoto[i])){dfTT$Kyoto[i] <- dfTT$Kyoto[i-1]}else{}
  if(is.na(dfTT$Mie[i])){dfTT$Mie[i] <- dfTT$Mie[i-1]}else{}
  if(is.na(dfTT$Miyagi[i])){dfTT$Miyagi[i] <- dfTT$Miyagi[i-1]}else{}   
  if(is.na(dfTT$Miyazaki[i])){dfTT$Miyazaki[i] <- dfTT$Miyazaki[i-1]}else{}
  if(is.na(dfTT$Nagano[i])){dfTT$Nagano[i] <- dfTT$Nagano[i-1]}else{}
  if(is.na(dfTT$Nagasaki[i])){dfTT$Nagasaki[i] <- dfTT$Nagasaki[i-1]}else{}
  if(is.na(dfTT$Nara[i])){dfTT$Nara[i] <- dfTT$Nara[i-1]}else{}    
  if(is.na(dfTT$Niigata[i])){dfTT$Niigata[i] <- dfTT$Niigata[i-1]}else{}
  if(is.na(dfTT$Oita[i])){dfTT$Oita[i] <- dfTT$Oita[i-1]}else{}
  if(is.na(dfTT$Okayama[i])){dfTT$Okayama[i] <- dfTT$Okayama[i-1]}else{}
  if(is.na(dfTT$Okinawa[i])){dfTT$Okinawa[i] <- dfTT$Okinawa[i-1]}else{}  
  if(is.na(dfTT$Osaka[i])){dfTT$Osaka[i] <- dfTT$Osaka[i-1]}else{}
  if(is.na(dfTT$Saga[i])){dfTT$Saga[i] <- dfTT$Saga[i-1]}else{}
  if(is.na(dfTT$Saitama[i])){dfTT$Saitama[i] <- dfTT$Saitama[i-1]}else{}
  if(is.na(dfTT$Shiga[i])){dfTT$Shiga[i] <- dfTT$Shiga[i-1]}else{}    
  if(is.na(dfTT$Shimane[i])){dfTT$Shimane[i] <- dfTT$Shimane[i-1]}else{}
  if(is.na(dfTT$Shizuoka[i])){dfTT$Shizuoka[i] <- dfTT$Shizuoka[i-1]}else{}
  if(is.na(dfTT$Tochigi[i])){dfTT$Tochigi[i] <- dfTT$Tochigi[i-1]}else{}
  if(is.na(dfTT$Tokushima[i])){dfTT$Tokushima[i] <- dfTT$Tokushima[i-1]}else{}   
  if(is.na(dfTT$Tokyo[i])){dfTT$Tokyo[i] <- dfTT$Tokyo[i-1]}else{}
  if(is.na(dfTT$Tottori[i])){dfTT$Tottori[i] <- dfTT$Tottori[i-1]}else{}
  if(is.na(dfTT$Toyama[i])){dfTT$Toyama[i] <- dfTT$Toyama[i-1]}else{}
  if(is.na(dfTT$Wakayama[i])){dfTT$Wakayama[i] <- dfTT$Wakayama[i-1]}else{}    
  if(is.na(dfTT$Yamagata[i])){dfTT$Yamagata[i] <- dfTT$Yamagata[i-1]}else{}
  if(is.na(dfTT$Yamaguchi[i])){dfTT$Yamaguchi[i] <- dfTT$Yamaguchi[i-1]}else{}
  if(is.na(dfTT$Yamanashi[i])){dfTT$Yamanashi[i] <- dfTT$Yamanashi[i-1]}else{}
}

ad <- summarize(dfTT2, ALL=mean(rate))

dfTT <- left_join(dfTT, ad, by="date")

write.csv(dfTT, "Vaccine.csv")