library(tidyverse)

data1 <- read_csv("Covid19_p.csv")
data1$Date <- as.Date(data1$Date)
data2 <- select(data1, Date, ALL)

data2$Number <- NA

for (i in 2:nrow(data2)) {
  data2$Number[i-1] <- data2$ALL[i]-data2$ALL[i-1]
}

data2 <- filter(data2, Date>="2021-3-1" & Date<="2022-03-31")
data2$Number[242] <- data2$ALL[242]-data2$ALL[241]
data2$Number[242]

f1 <- ggplot(data2, aes(Date, Number))+
  annotate(geom="rect", xmin =as.Date("2021-03-06"), ymin=0, 
           xmax=as.Date("2021-04-03"), ymax=105000, fill="blue", alpha=0.2)+
  annotate(geom="rect", xmin =as.Date("2021-07-16"), ymin=0, 
           xmax=as.Date("2021-08-13"), ymax=105000, fill="blue", alpha=0.2)+
  annotate(geom="rect", xmin =as.Date("2021-10-29"), ymin=0, 
           xmax=as.Date("2021-11-26"), ymax=105000, fill="blue", alpha=0.2)+
  annotate(geom="rect", xmin =as.Date("2022-02-25"), ymin=0, 
           xmax=as.Date("2022-03-25"), ymax=105000, fill="blue", alpha=0.2)+
  geom_text(x=as.Date("2021-03-06"), y=100000, label="Wave 1")+
  geom_text(x=as.Date("2021-07-16"), y=100000, label="Wave 2")+
  geom_text(x=as.Date("2021-10-20"), y=100000, label="Wave 3")+
  geom_text(x=as.Date("2022-02-25"), y=100000, label="Wave 4")+
  geom_line(color="red", linewidth=1)+
  scale_x_date(date_breaks = "1 months", date_labels = "%y/%m")+
  scale_y_continuous(breaks = seq(0, 90000, 10000))+
  xlab("Year/Month")+ylab("Number of Newly Infected")

f1
ggsave("figure1.png", dpi=500)

data1b <- read_csv("Vaccine.csv")
data2b <- select(data1b, date, ALL)

data2b$Date <- as.Date(data2b$date)
data2b$Number <- 100*data2b$ALL

data2b <- filter(data2b, Date>="2021-3-1" & Date<="2022-03-31")

f2 <- ggplot(data2b, aes(Date, Number))+
  annotate(geom="rect", xmin =as.Date("2021-03-06"), ymin=0, 
           xmax=as.Date("2021-04-03"), ymax=100, fill="blue", alpha=0.2)+
  annotate(geom="rect", xmin =as.Date("2021-07-16"), ymin=0, 
           xmax=as.Date("2021-08-13"), ymax=100, fill="blue", alpha=0.2)+
  annotate(geom="rect", xmin =as.Date("2021-10-29"), ymin=0, 
           xmax=as.Date("2021-11-26"), ymax=100, fill="blue", alpha=0.2)+
  annotate(geom="rect", xmin =as.Date("2022-02-25"), ymin=0, 
           xmax=as.Date("2022-03-25"), ymax=100, fill="blue", alpha=0.2)+
  geom_text(x=as.Date("2021-03-06"), y=90, label="Wave 1")+
  geom_text(x=as.Date("2021-07-16"), y=90, label="Wave 2")+
  geom_text(x=as.Date("2021-10-20"), y=90, label="Wave 3")+
  geom_text(x=as.Date("2022-02-25"), y=90, label="Wave 4")+
  geom_line(color="red", linewidth=1)+
  scale_x_date(date_breaks = "1 months", date_labels = "%y/%m")+
  scale_y_continuous(breaks=seq(0,100,10))+
  xlab("Year/Month ")+ylab("% Vaccinated (first shot)")

f2
ggsave("figure2.png", dpi=500)
