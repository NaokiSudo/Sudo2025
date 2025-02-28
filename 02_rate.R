library(tidyverse)

cal_number <- function(X, Y){ 
  
Rate$number <- NA
Covid19S <- filter(Covid19, Date==X)
Covid19E <- filter(Covid19, Date==Y)

Rate$number[Rate$Prefecture=="Hokkaido"] <- (Covid19E$Hokkaido- Covid19S$Hokkaido)*10000
Rate$number[Rate$Prefecture=="Aomori"	] <- (Covid19E$Aomori	- Covid19S$Aomori	)*10000
Rate$number[Rate$Prefecture=="Iwate"	] <- (Covid19E$Iwate	- Covid19S$Iwate	)*10000
Rate$number[Rate$Prefecture=="Miyagi"	] <- (Covid19E$Miyagi	- Covid19S$Miyagi	)*10000
Rate$number[Rate$Prefecture=="Akita"	] <- (Covid19E$Akita	- Covid19S$Akita	)*10000
Rate$number[Rate$Prefecture=="Yamagata"	] <- (Covid19E$Yamagata	- Covid19S$Yamagata	)*10000
Rate$number[Rate$Prefecture=="Fukushima"	] <- (Covid19E$Fukushima	- Covid19S$Fukushima	)*10000
Rate$number[Rate$Prefecture=="Ibaraki"	] <- (Covid19E$Ibaraki	- Covid19S$Ibaraki	)*10000
Rate$number[Rate$Prefecture=="Tochigi"	] <- (Covid19E$Tochigi	- Covid19S$Tochigi	)*10000
Rate$number[Rate$Prefecture=="Gunma"	] <- (Covid19E$Gunma	- Covid19S$Gunma	)*10000
Rate$number[Rate$Prefecture=="Saitama"	] <- (Covid19E$Saitama	- Covid19S$Saitama	)*10000
Rate$number[Rate$Prefecture=="Chiba"	] <- (Covid19E$Chiba	- Covid19S$Chiba	)*10000
Rate$number[Rate$Prefecture=="Tokyo"	] <- (Covid19E$Tokyo	- Covid19S$Tokyo	)*10000
Rate$number[Rate$Prefecture=="Kanagawa"	] <- (Covid19E$Kanagawa	- Covid19S$Kanagawa	)*10000
Rate$number[Rate$Prefecture=="Niigata"	] <- (Covid19E$Niigata	- Covid19S$Niigata	)*10000
Rate$number[Rate$Prefecture=="Toyama"	] <- (Covid19E$Toyama	- Covid19S$Toyama	)*10000
Rate$number[Rate$Prefecture=="Ishikawa"	] <- (Covid19E$Ishikawa	- Covid19S$Ishikawa	)*10000
Rate$number[Rate$Prefecture=="Fukui"	] <- (Covid19E$Fukui	- Covid19S$Fukui	)*10000
Rate$number[Rate$Prefecture=="Yamanashi"	] <- (Covid19E$Yamanashi	- Covid19S$Yamanashi	)*10000
Rate$number[Rate$Prefecture=="Nagano"	] <- (Covid19E$Nagano	- Covid19S$Nagano)*10000
Rate$number[Rate$Prefecture=="Gifu"	] <- (Covid19E$Gifu	- Covid19S$Gifu	)*10000
Rate$number[Rate$Prefecture=="Shizuoka"	] <- (Covid19E$Shizuoka	- Covid19S$Shizuoka	)*10000
Rate$number[Rate$Prefecture=="Aichi"	] <- (Covid19E$Aichi	- Covid19S$Aichi	)*10000
Rate$number[Rate$Prefecture=="Mie"	] <- (Covid19E$Mie	- Covid19S$Mie	)*10000
Rate$number[Rate$Prefecture=="Shiga"	] <- (Covid19E$Shiga	- Covid19S$Shiga	)*10000
Rate$number[Rate$Prefecture=="Kyoto"	] <- (Covid19E$Kyoto	- Covid19S$Kyoto	)*10000
Rate$number[Rate$Prefecture=="Osaka"	] <- (Covid19E$Osaka	- Covid19S$Osaka	)*10000
Rate$number[Rate$Prefecture=="Hyogo"	] <- (Covid19E$Hyogo	- Covid19S$Hyogo	)*10000
Rate$number[Rate$Prefecture=="Nara"	] <- (Covid19E$Nara	- Covid19S$Nara	)*10000
Rate$number[Rate$Prefecture=="Wakayama"	] <- (Covid19E$Wakayama	- Covid19S$Wakayama	)*10000
Rate$number[Rate$Prefecture=="Tottori"	] <- (Covid19E$Tottori	- Covid19S$Tottori	)*10000
Rate$number[Rate$Prefecture=="Shimane"	] <- (Covid19E$Shimane	- Covid19S$Shimane	)*10000
Rate$number[Rate$Prefecture=="Okayama"	] <- (Covid19E$Okayama	- Covid19S$Okayama	)*10000
Rate$number[Rate$Prefecture=="Hiroshima"	] <- (Covid19E$Hiroshima	- Covid19S$Hiroshima	)*10000
Rate$number[Rate$Prefecture=="Yamaguchi"	] <- (Covid19E$Yamaguchi	- Covid19S$Yamaguchi	)*10000
Rate$number[Rate$Prefecture=="Tokushima"	] <- (Covid19E$Tokushima	- Covid19S$Tokushima	)*10000
Rate$number[Rate$Prefecture=="Kagawa"	] <- (Covid19E$Kagawa	- Covid19S$Kagawa	)*10000
Rate$number[Rate$Prefecture=="Ehime"	] <- (Covid19E$Ehime	- Covid19S$Ehime	)*10000
Rate$number[Rate$Prefecture=="Kochi"	] <- (Covid19E$Kochi	- Covid19S$Kochi	)*10000
Rate$number[Rate$Prefecture=="Fukuoka"	] <- (Covid19E$Fukuoka	- Covid19S$Fukuoka	)*10000
Rate$number[Rate$Prefecture=="Saga"	] <- (Covid19E$Saga	- Covid19S$Saga	)*10000
Rate$number[Rate$Prefecture=="Nagasaki"	] <- (Covid19E$Nagasaki	- Covid19S$Nagasaki	)*10000
Rate$number[Rate$Prefecture=="Kumamoto"	] <- (Covid19E$Kumamoto	- Covid19S$Kumamoto	)*10000
Rate$number[Rate$Prefecture=="Oita"	] <- (Covid19E$Oita	- Covid19S$Oita	)*10000
Rate$number[Rate$Prefecture=="Miyazaki"	] <- (Covid19E$Miyazaki	- Covid19S$Miyazaki	)*10000
Rate$number[Rate$Prefecture=="Kagoshima"	] <- (Covid19E$Kagoshima	- Covid19S$Kagoshima	)*10000
Rate$number[Rate$Prefecture=="Okinawa"	] <- (Covid19E$Okinawa	- Covid19S$Okinawa	)*10000

return(Rate$number)

}

cal_number2 <- function(X, Y){ 
  
  Rate$number <- NA
  Covid19$Date <- as.Date(Covid19$Date)
  Covid19_f <- filter(Covid19, Date>=X & Date<=Y)
  
  Rate$number[Rate$Prefecture=="Hokkaido"] <- sum(Covid19_f$Hokkaido	)
  Rate$number[Rate$Prefecture=="Aomori"] <- sum(Covid19_f$Aomori	)
  Rate$number[Rate$Prefecture=="Iwate"] <- sum(Covid19_f$Iwate	)
  Rate$number[Rate$Prefecture=="Miyagi"] <- sum(Covid19_f$Miyagi	)
  Rate$number[Rate$Prefecture=="Akita"] <- sum(Covid19_f$Akita	)
  Rate$number[Rate$Prefecture=="Yamagata"] <- sum(Covid19_f$Yamagata	)
  Rate$number[Rate$Prefecture=="Fukushima"] <- sum(Covid19_f$Fukushima	)
  Rate$number[Rate$Prefecture=="Ibaraki"] <- sum(Covid19_f$Ibaraki	)
  Rate$number[Rate$Prefecture=="Tochigi"] <- sum(Covid19_f$Tochigi	)
  Rate$number[Rate$Prefecture=="Gunma"] <- sum(Covid19_f$Gunma	)
  Rate$number[Rate$Prefecture=="Saitama"] <- sum(Covid19_f$Saitama	)
  Rate$number[Rate$Prefecture=="Chiba"] <- sum(Covid19_f$Chiba	)
  Rate$number[Rate$Prefecture=="Tokyo"] <- sum(Covid19_f$Tokyo	)
  Rate$number[Rate$Prefecture=="Kanagawa"] <- sum(Covid19_f$Kanagawa	)
  Rate$number[Rate$Prefecture=="Niigata"] <- sum(Covid19_f$Niigata	)
  Rate$number[Rate$Prefecture=="Toyama"] <- sum(Covid19_f$Toyama	)
  Rate$number[Rate$Prefecture=="Ishikawa"] <- sum(Covid19_f$Ishikawa	)
  Rate$number[Rate$Prefecture=="Fukui"] <- sum(Covid19_f$Fukui	)
  Rate$number[Rate$Prefecture=="Yamanashi"] <- sum(Covid19_f$Yamanashi	)
  Rate$number[Rate$Prefecture=="Nagano"] <- sum(Covid19_f$Nagano	)
  Rate$number[Rate$Prefecture=="Gifu"] <- sum(Covid19_f$Gifu	)
  Rate$number[Rate$Prefecture=="Shizuoka"] <- sum(Covid19_f$Shizuoka	)
  Rate$number[Rate$Prefecture=="Aichi"] <- sum(Covid19_f$Aichi	)
  Rate$number[Rate$Prefecture=="Mie"] <- sum(Covid19_f$Mie	)
  Rate$number[Rate$Prefecture=="Shiga"] <- sum(Covid19_f$Shiga	)
  Rate$number[Rate$Prefecture=="Kyoto"] <- sum(Covid19_f$Kyoto	)
  Rate$number[Rate$Prefecture=="Osaka"] <- sum(Covid19_f$Osaka	)
  Rate$number[Rate$Prefecture=="Hyogo"] <- sum(Covid19_f$Hyogo	)
  Rate$number[Rate$Prefecture=="Nara"] <- sum(Covid19_f$Nara	)
  Rate$number[Rate$Prefecture=="Wakayama"] <- sum(Covid19_f$Wakayama	)
  Rate$number[Rate$Prefecture=="Tottori"] <- sum(Covid19_f$Tottori	)
  Rate$number[Rate$Prefecture=="Shimane"] <- sum(Covid19_f$Shimane	)
  Rate$number[Rate$Prefecture=="Okayama"] <- sum(Covid19_f$Okayama	)
  Rate$number[Rate$Prefecture=="Hiroshima"] <- sum(Covid19_f$Hiroshima	)
  Rate$number[Rate$Prefecture=="Yamaguchi"] <- sum(Covid19_f$Yamaguchi	)
  Rate$number[Rate$Prefecture=="Tokushima"] <- sum(Covid19_f$Tokushima	)
  Rate$number[Rate$Prefecture=="Kagawa"] <- sum(Covid19_f$Kagawa	)
  Rate$number[Rate$Prefecture=="Ehime"] <- sum(Covid19_f$Ehime	)
  Rate$number[Rate$Prefecture=="Kochi"] <- sum(Covid19_f$Kochi	)
  Rate$number[Rate$Prefecture=="Fukuoka"] <- sum(Covid19_f$Fukuoka	)
  Rate$number[Rate$Prefecture=="Saga"] <- sum(Covid19_f$Saga	)
  Rate$number[Rate$Prefecture=="Nagasaki"] <- sum(Covid19_f$Nagasaki	)
  Rate$number[Rate$Prefecture=="Kumamoto"] <- sum(Covid19_f$Kumamoto	)
  Rate$number[Rate$Prefecture=="Oita"] <- sum(Covid19_f$Oita	)
  Rate$number[Rate$Prefecture=="Miyazaki"] <- sum(Covid19_f$Miyazaki	)
  Rate$number[Rate$Prefecture=="Kagoshima"] <- sum(Covid19_f$Kagoshima	)
  Rate$number[Rate$Prefecture=="Okinawa"] <- sum(Covid19_f$Okinawa	)
  
  return(Rate$number)
  
}

cal_number3 <- function(X, Y){ 
  
  Rate$number <- NA
  Covid19$Date <- as.Date(Covid19$date)
  Covid19_f <- filter(Covid19, Date>=X & Date<=Y)
  D <- as.numeric(difftime(Y, X, units = "days"))
  
  Rate$number[Rate$Prefecture=="Hokkaido"] <- sum(Covid19_f$Hokkaido	)/D
  Rate$number[Rate$Prefecture=="Aomori"] <- sum(Covid19_f$Aomori	)/D
  Rate$number[Rate$Prefecture=="Iwate"] <- sum(Covid19_f$Iwate	)/D
  Rate$number[Rate$Prefecture=="Miyagi"] <- sum(Covid19_f$Miyagi	)/D
  Rate$number[Rate$Prefecture=="Akita"] <- sum(Covid19_f$Akita	)/D
  Rate$number[Rate$Prefecture=="Yamagata"] <- sum(Covid19_f$Yamagata	)/D
  Rate$number[Rate$Prefecture=="Fukushima"] <- sum(Covid19_f$Fukushima	)/D
  Rate$number[Rate$Prefecture=="Ibaraki"] <- sum(Covid19_f$Ibaraki	)/D
  Rate$number[Rate$Prefecture=="Tochigi"] <- sum(Covid19_f$Tochigi	)/D
  Rate$number[Rate$Prefecture=="Gunma"] <- sum(Covid19_f$Gunma	)/D
  Rate$number[Rate$Prefecture=="Saitama"] <- sum(Covid19_f$Saitama	)/D
  Rate$number[Rate$Prefecture=="Chiba"] <- sum(Covid19_f$Chiba	)/D
  Rate$number[Rate$Prefecture=="Tokyo"] <- sum(Covid19_f$Tokyo	)/D
  Rate$number[Rate$Prefecture=="Kanagawa"] <- sum(Covid19_f$Kanagawa	)/D
  Rate$number[Rate$Prefecture=="Niigata"] <- sum(Covid19_f$Niigata	)/D
  Rate$number[Rate$Prefecture=="Toyama"] <- sum(Covid19_f$Toyama	)/D
  Rate$number[Rate$Prefecture=="Ishikawa"] <- sum(Covid19_f$Ishikawa	)/D
  Rate$number[Rate$Prefecture=="Fukui"] <- sum(Covid19_f$Fukui	)/D
  Rate$number[Rate$Prefecture=="Yamanashi"] <- sum(Covid19_f$Yamanashi	)/D
  Rate$number[Rate$Prefecture=="Nagano"] <- sum(Covid19_f$Nagano	)/D
  Rate$number[Rate$Prefecture=="Gifu"] <- sum(Covid19_f$Gifu	)/D
  Rate$number[Rate$Prefecture=="Shizuoka"] <- sum(Covid19_f$Shizuoka	)/D
  Rate$number[Rate$Prefecture=="Aichi"] <- sum(Covid19_f$Aichi	)/D
  Rate$number[Rate$Prefecture=="Mie"] <- sum(Covid19_f$Mie	)/D
  Rate$number[Rate$Prefecture=="Shiga"] <- sum(Covid19_f$Shiga	)/D
  Rate$number[Rate$Prefecture=="Kyoto"] <- sum(Covid19_f$Kyoto	)/D
  Rate$number[Rate$Prefecture=="Osaka"] <- sum(Covid19_f$Osaka	)/D
  Rate$number[Rate$Prefecture=="Hyogo"] <- sum(Covid19_f$Hyogo	)/D
  Rate$number[Rate$Prefecture=="Nara"] <- sum(Covid19_f$Nara	)/D
  Rate$number[Rate$Prefecture=="Wakayama"] <- sum(Covid19_f$Wakayama	)/D
  Rate$number[Rate$Prefecture=="Tottori"] <- sum(Covid19_f$Tottori	)/D
  Rate$number[Rate$Prefecture=="Shimane"] <- sum(Covid19_f$Shimane	)/D
  Rate$number[Rate$Prefecture=="Okayama"] <- sum(Covid19_f$Okayama	)/D
  Rate$number[Rate$Prefecture=="Hiroshima"] <- sum(Covid19_f$Hiroshima	)/D
  Rate$number[Rate$Prefecture=="Yamaguchi"] <- sum(Covid19_f$Yamaguchi	)/D
  Rate$number[Rate$Prefecture=="Tokushima"] <- sum(Covid19_f$Tokushima	)/D
  Rate$number[Rate$Prefecture=="Kagawa"] <- sum(Covid19_f$Kagawa	)/D
  Rate$number[Rate$Prefecture=="Ehime"] <- sum(Covid19_f$Ehime	)/D
  Rate$number[Rate$Prefecture=="Kochi"] <- sum(Covid19_f$Kochi	)/D
  Rate$number[Rate$Prefecture=="Fukuoka"] <- sum(Covid19_f$Fukuoka	)/D
  Rate$number[Rate$Prefecture=="Saga"] <- sum(Covid19_f$Saga	)/D
  Rate$number[Rate$Prefecture=="Nagasaki"] <- sum(Covid19_f$Nagasaki	)/D
  Rate$number[Rate$Prefecture=="Kumamoto"] <- sum(Covid19_f$Kumamoto	)/D
  Rate$number[Rate$Prefecture=="Oita"] <- sum(Covid19_f$Oita	)/D
  Rate$number[Rate$Prefecture=="Miyazaki"] <- sum(Covid19_f$Miyazaki	)/D
  Rate$number[Rate$Prefecture=="Kagoshima"] <- sum(Covid19_f$Kagoshima	)/D
  Rate$number[Rate$Prefecture=="Okinawa"] <- sum(Covid19_f$Okinawa	)/D
  
  return(Rate$number)
  
}


Covid19 <- read_csv("Covid19_p.csv")
Rate <- read_csv("population.csv")

Rate$rate1_p <- cal_number("2021/3/6","2021/4/3")/Rate$value
Rate$rate2_p <- cal_number("2021/7/16","2021/8/13")/Rate$value
Rate$rate3_p <- cal_number("2021/10/29","2021/11/26")/Rate$value
Rate$rate4_p <- cal_number("2022/2/25","2022/3/25")/Rate$value

Covid19 <- read_csv("Covid19_d.csv")

Rate$rate1_d <- 100*cal_number("2021/3/6","2021/4/3")/Rate$value
Rate$rate2_d <- 100*cal_number("2021/7/16","2021/8/13")/Rate$value
Rate$rate3_d <- 100*cal_number("2021/10/29","2021/11/26")/Rate$value
Rate$rate4_d <- 100*cal_number("2022/2/25","2022/3/25")/Rate$value

Covid19 <- read_csv("Covid19_s.csv")

Rate$rate1_s <- 1000000*cal_number2("2021-03-06","2021-04-03")/(29*Rate$value)
Rate$rate2_s <- 1000000*cal_number2("2021-07-16","2021-08-13")/(29*Rate$value)
Rate$rate3_s <- 1000000*cal_number2("2021-10-29","2021-11-26")/(29*Rate$value)
Rate$rate4_s <- 1000000*cal_number2("2022-02-25","2022-03-25")/(29*Rate$value)

Covid19 <- read_csv("Vaccine.csv")
Covid19$Data <- Covid19$date

Rate$rate1_v <- 100*cal_number3("2021-03-06","2021-04-03")
Rate$rate2_v <- 100*cal_number3("2021-07-16","2021-08-13")
Rate$rate3_v <- 100*cal_number3("2021-10-29","2021-11-26")
Rate$rate4_v <- 100*cal_number3("2022-02-25","2022-03-25")

write.csv(Rate, "Rate.csv")
