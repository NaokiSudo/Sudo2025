library(dplyr)

data1 <- read.csv("SSPW21_panel_edit_coding_SSM_final.csv", fileEncoding = "shift-jis")

table(data1$Q38W4)
data1$vacW1 <- 0
data1$vacW2 <- 0
data1$vacW3 <- 0
data1$vacW4 <- 0

data1$vacW1[data1$Q38W4==1] <- 1
data1$vacW2[data1$Q38W4<=2] <- 1
data1$vacW3[data1$Q38W4<=3] <- 1
data1$vacW4[data1$Q38W4<=4] <- 1

data1$vacW1[is.na(data1$MID_4)] <- NA
data1$vacW2[is.na(data1$MID_4)] <- NA
data1$vacW3[is.na(data1$MID_4)] <- NA
data1$vacW4[is.na(data1$MID_4)] <- NA

data_w1 <- data1
data_w2 <- filter(data1, !is.na(MID_2))
data_w3 <- filter(data1, !is.na(MID_3))
data_w4 <- filter(data1, !is.na(MID_4))

data_w1$Wave <- 1
data_w2$Wave <- 2
data_w3$Wave <- 3
data_w4$Wave <- 4

data_w1 <- select(data_w1, MID,	F1W1,	F2W1,	F3W1,	S1W1,	F4W1,	F5t1W1,	
                  F5t2W1,	Q1_1W1,	Q1_2aW1,	Q1_2at4W1,	Q1_2bW1,	
                  Q1_2bt4W1,	Q2_1W1,	Q2_1c2W1,	Q2_1c3W1,	Q2_1c4W1,	
                  Q2_1c5W1,	Q2_1c6W1,	Q2_1c7W1,	Q2_1c8W1,	Q2_1c9W1,	
                  Q2_1c10W1,	Q2_1c11W1,	Q2_1c12W1,	Q2_1c13W1,	
                  Q2_1c14W1,	Q2_1c15W1,	Q2_2W1,	Q2_3W1,	Q2_3t9W1,	
                  Q2_4W1,	Q3W1,	Q4W1,	Q4t19W1,	Q5W1,	Q6W1,	Q7W1,	Q8W1,	
                  Q8t7W1,	Q9t1W1,	Q9t2W1,	Q9t3W1,	Q10W1,	Q10t9W1,	
                  Q11W1,	Q12W1,	Q13W1,	Q14W1,	Q15W1,	Q15_2W1,	
                  Q16s1W1,	Q16s2W1,	Q16s3W1,	Q16s4W1,	Q17W1,	Q18W1,	
                  Q19s1W1,	Q19s2W1,	Q19s3W1,	Q19s4W1,	Q19s5W1,	
                  Q19s6W1,	Q20W1,	Q21s1W1,	Q21s2W1,	Q21s3W1,	
                  Q22c1W1,	Q22c2W1,	Q22c3W1,	Q22c4W1,	Q22c5W1,	
                  Q22c6W1,	Q22c7W1,	Q22c8W1,	Q22t7W1,	Q23W1,	Q24W1,	
                  Q25s1W1,	Q25s2W1,	Q25s3W1,	Q25s4W1,	Q25s5W1,	
                  Q25s6W1,	Q25s7W1,	Q25s8W1,	Q25s9W1,	Q26s1W1,	
                  Q26s2W1,	Q26s3W1,	Q26s4W1,	Q26s5W1,	Q26s6W1,	
                  Q27s1W1,	Q27s2W1,	Q28aW1,	Q28at30W1,	Q28bW1,	
                  Q28bt30W1,	Q28CW1,	Q29W1,	Q30W1,	Q31s1W1,	Q31s2W1,	
                  Q31s3W1,	Q32s1W1,	Q32s2W1,	Q33W1,	SSM1,	ISCO1,
                  SSM8, Wave, vacW1)

data_w2 <- select(data_w2, MID, F1W2,	F2W2,	F3W2,	S1W2,	F4W2,	F5t1W2,	
                  F5t2W2,	Q1_1W2,	Q1_2aW2,	Q1_2at3W2,	Q1_2bW2,	
                  Q1_2bt3W2,	Q2_1c1W2,	Q2_1c2W2,	Q2_1c3W2,	Q2_1c4W2,	
                  Q2_2W2,	Q2_3W2,	Q2_3t9W2,	Q2_4W2,	Q3t1W2,	Q4W2,	
                  Q4t19W2,	Q5W2,	Q6W2,	Q7W2,	Q8W2,	Q8t7W2,	Q9t1W2,	
                  Q9t2W2,	Q9t3W2,	Q11W2,	Q12W2,	Q13W2,	Q14W2,	Q15W2,	
                  Q15_2W2,	Q16s1W2,	Q16s2W2,	Q16s3W2,	Q16s4W2,	Q17W2,
                  Q18W2,	Q19s1W2,	Q19s2W2,	Q19s3W2,	Q19s4W2,	Q19s5W2,
                  Q19s6W2,	Q20W2,	Q21s1W2,	Q21s2W2,	Q21s3W2,	Q22c1W2,
                  Q22c2W2,	Q22c3W2,	Q22c4W2,	Q22c5W2,	Q22c6W2,	
                  Q22c7W2,	Q22c8W2,	Q21t7W2,	Q23W2,	Q24W2,	Q25s1W2,	
                  Q25s2W2,	Q25s3W2,	Q25s4W2,	Q25s5W2,	Q25s6W2,	
                  Q25s7W2,	Q25s8W2,	Q25s9W2,	Q25s10W2,	Q25s11W2,	
                  Q25s12W2,	Q26s1W2,	Q26s2W2,	Q26s3W2,	Q26s4W2,	
                  Q26s5W2,	Q26s6W2,	Q27s1W2,	Q27s2W2,	Q29W2,	Q30W2,	
                  Q31s1W2,	Q31s2W2,	Q31s3W2,	Q32s1W2,	Q32s2W2,	
                  Q32s3W2,	Q34W2,	Q35W2,	SSM2,	ISCO2,	
                  SSM8_2, Wave, vacW2)

data_w3 <- select(data_w3, MID,	F1W3,	F2W3,	F3W3,	S1W3,	F4W3,	F5t1W3,	F5t2W3,
                  Q1_1W3,	Q1_2aW3,	Q1_2at3W3,	Q1_2bW3,	Q1_2bt3W3,	
                  Q2_1c1W3,	Q2_1c2W3,	Q2_1c3W3,	Q2_1c4W3,	Q2_2W3,	Q2_3W3,	
                  Q2_3t9W3,	Q2_4W3,	Q3t1W3,	Q4W3,	Q4t19W3,	Q5W3,	Q6W3,	
                  Q7W3,	Q8W3,	Q8t7W3,	Q9t1W3,	Q9t2W3,	Q9t3W3,	Q10W3,	
                  Q10t9W3,	Q11W3,	Q12W3,	Q13W3,	Q14W3,	Q15W3,	
                  Q15_2W3,	Q16s1W3,	Q16s2W3,	Q16s3W3,	Q16s4W3,	
                  Q17W3,	Q18W3,	Q19s1W3,	Q19s2W3,	Q19s3W3,	Q19s4W3,	
                  Q19s5W3,	Q19s6W3,	Q20W3,	Q21s1W3,	Q21s2W3,	Q21s3W3,
                  Q22c1W3,	Q22c2W3,	Q22c3W3,	Q22c4W3,	Q22c5W3,	Q22c6W3,
                  Q22c7W3,	Q22c8W3,	Q22t7W3,	Q23W3,	Q24W3,	Q25s1W3,	
                  Q25s2W3,	Q25s3W3,	Q25s4W3,	Q25s5W3,	Q25s6W3,	Q25s7W3,
                  Q25s8W3,	Q25s9W3,	Q25s10W3,	Q25s11W3,	Q25s12W3,	Q26s1W3,
                  Q26s2W3,	Q26s3W3,	Q26s4W3,	Q26s5W3,	Q26s6W3,	Q27s1W3,
                  Q27s2W3,	Q29W3,	Q30W3,	Q31s1W3,	Q31s2W3,	Q31s3W3,	
                  Q32s1W3,	Q32s2W3,	Q32s3W3,	Q35W3,	Q36W3,	Q37W3,	
                  SSM3,	ISCO3,	SSM8_3, Wave, vacW3)

data_w4 <- select(data_w4,MID,	F1W4,	F2W4,	F3W4,	S1W4,	F4W4,	F5t1W4,	
                  F5t2W4,	Q1_1W4,	Q1_2aW4,	Q1_2at3W4,	Q1_2bW4,	
                  Q1_2bt3W4,	Q2_1c1W4,	Q2_1c2W4,	Q2_1c3W4,	Q2_1c4W4,	
                  Q2_2W4,	Q2_3W4,	Q2_3t9W4,	Q2_4W4,	Q3t1W4,	Q4W4,	Q4t19W4,
                  Q5W4,	Q6W4,	Q7W4,	Q8W4,	Q8t7W4,	Q9t1W4,	Q9t2W4,	Q9t3W4,	
                  Q10W4,	Q10t9W4,	Q11W4,	Q12W4,	Q13W4,	Q14W4,	Q15W4,
                  Q15_2W4,	Q16s1W4,	Q16s2W4,	Q16s3W4,	Q16s4W4,	Q17W4,
                  Q18W4,	Q19s1W4,	Q19s2W4,	Q19s3W4,	Q19s4W4,	Q19s5W4,
                  Q19s6W4,	Q20W4,	Q21s1W4,	Q21s2W4,	Q21s3W4,	Q22c1W4,
                  Q22c2W4,	Q22c3W4,	Q22c4W4,	Q22c5W4,	Q22c6W4,	
                  Q22c7W4,	Q22c8W4,	Q22t7W4,	Q23W4,	Q24W4,	Q25s1W4,	
                  Q25s2W4,	Q25s3W4,	Q25s4W4,	Q25s5W4,	Q25s6W4,	
                  Q25s7W4,	Q25s8W4,	Q25s9W4,	Q25s10W4,	Q25s11W4,	
                  Q25s12W4,	Q26s1W4,	Q26s2W4,	Q26s3W4,	Q26s4W4,	
                  Q26s5W4,	Q26s6W4,	Q27s1W4,	Q27s2W4,	Q29W4,	Q30W4,	
                  Q31s1W4,	Q31s2W4,	Q31s3W4,	Q32s1W4,	Q32s2W4,	
                  Q32s3W4,	Q38W4,	Q39W4,	Q36W4,	Q37W4,
                  SSM4,	ISCO4,	SSM8_4, Wave, vacW4)

colnames(data_w1) <- c("MID",       "F1",      "F2",      "F3",      "S1",      "F4",     
                       "F5t1",    "F5t2",    "Q1_1" ,   "Q1_2a",   "Q1_2at4", "Q1_2b",  
                       "Q1_2bt4", "Q2_1c1",    "Q2_1c2",  "Q2_1c3",  "Q2_1c4",  "Q2_1c5", 
                       "Q2_1c6",  "Q2_1c7",  "Q2_1c8",  "Q2_1c9",  "Q2_1c10", "Q2_1c11",
                       "Q2_1c12", "Q2_1c13", "Q2_1c14", "Q2_1c15", "Q2_2",    "Q2_3",   
                       "Q2_3t9",  "Q2_4",    "Q3",      "Q4",      "Q4t19",   "Q5",     
                       "Q6",      "Q7",      "Q8",      "Q8t7",    "Q9t1",    "Q9t2",  
                       "Q9t3",    "Q10",     "Q10t9",   "Q11",     "Q12",     "Q13",    
                       "Q14",     "Q15",     "Q15_2",   "Q16s1",   "Q16s2",   "Q16s3",  
                       "Q16s4",   "Q17",     "Q18",     "Q19s1",   "Q19s2",   "Q19s3",  
                       "Q19s4",   "Q19s5",   "Q19s6",   "Q20",     "Q21s1",   "Q21s2",  
                       "Q21s3",   "Q22c1",   "Q22c2",   "Q22c3",   "Q22c4",   "Q22c5",  
                       "Q22c6",   "Q22c7",   "Q22c8",   "Q22t7",   "Q23",     "Q24",    
                       "Q25s1",   "Q25s2",   "Q25s3",   "Q25s4",   "Q25s5",   "Q25s6",  
                       "Q25s7",   "Q25s8",   "Q25s9",   "Q26s1",   "Q26s2",   "Q26s3",  
                       "Q26s4",   "Q26s5",   "Q26s6",   "Q27s1",   "Q27s2",   "Q28a",   
                       "Q28at30", "Q28b",    "Q28bt30", "Q28C",    "Q29",     "Q30",    
                       "Q31s1",   "Q31s2",   "Q31s3",   "Q32s1",   "Q32s2",   "Q33",    
                       "SSM",      "ISCO",     "SSM8",  "Wave", "vac")

colnames(data_w2) <- c("MID",       "F1",      "F2",      "F3",      "S1",      "F4",     
                       "F5t1",    "F5t2",    "Q1_1",    "Q1_2a",   "Q1_2at3", "Q1_2b",  
                       "Q1_2bt3", "Q2_1c1",  "Q2_1c2",  "Q2_1c3",  "Q2_1c4",  "Q2_2",   
                       "Q2_3",    "Q2_3t9",  "Q2_4",    "Q3t1",    "Q4",      "Q4t19",  
                       "Q5",      "Q6",      "Q7",      "Q8",      "Q8t7",    "Q9t1",   
                       "Q9t2",    "Q9t3",    "Q11",     "Q12",     "Q13",     "Q14",    
                       "Q15",     "Q15_2",   "Q16s1",   "Q16s2",   "Q16s3",   "Q16s4",  
                       "Q17",     "Q18",     "Q19s1",   "Q19s2",   "Q19s3",   "Q19s4",  
                       "Q19s5",   "Q19s6",   "Q20",     "Q21s1",   "Q21s2",   "Q21s3",  
                       "Q22c1",   "Q22c2",   "Q22c3",   "Q22c4",   "Q22c5",   "Q22c6",  
                       "Q22c7",   "Q22c8",   "Q21t7",   "Q23",     "Q24",     "Q25s1",  
                       "Q25s2",   "Q25s3",   "Q25s4",   "Q25s5",   "Q25s6",   "Q25s7",  
                       "Q25s8",   "Q25s9",   "Q25s10",  "Q25s11",  "Q25s12",  "Q26s1",  
                       "Q26s2",   "Q26s3",   "Q26s4",   "Q26s5",   "Q26s6",   "Q27s1",  
                       "Q27s2",   "Q29",     "Q30",     "Q31s1",   "Q31s2",   "Q31s3",  
                       "Q32s1",   "Q32s2",   "Q32s3",   "Q34",     "Q35",     "SSM",     
                       "ISCO",     "SSM8",    "Wave", "vac")

colnames(data_w3)<- c("MID",       "F1",      "F2",      "F3",      "S1",      "F4",     
                      "F5t1",    "F5t2",    "Q1_1",    "Q1_2a",   "Q1_2at3", "Q1_2b",  
                      "Q1_2bt3", "Q2_1c1",  "Q2_1c2",  "Q2_1c3",  "Q2_1c4",  "Q2_2",   
                      "Q2_3",    "Q2_3t9",  "Q2_4",    "Q3t1",    "Q4",      "Q4t19",  
                      "Q5",      "Q6",      "Q7",      "Q8",      "Q8t7",    "Q9t1",   
                      "Q9t2",    "Q9t3",    "Q10",     "Q10t9",   "Q11",     "Q12",    
                      "Q13",     "Q14",     "Q15",     "Q15_2",   "Q16s1",   "Q16s2",  
                      "Q16s3",   "Q16s4",   "Q17",     "Q18",     "Q19s1",   "Q19s2",  
                      "Q19s3",   "Q19s4",   "Q19s5",   "Q19s6",   "Q20",     "Q21s1",  
                      "Q21s2",   "Q21s3",   "Q22c1",   "Q22c2",   "Q22c3",   "Q22c4",  
                      "Q22c5",   "Q22c6",   "Q22c7",   "Q22c8",   "Q22t7",   "Q23",    
                      "Q24",     "Q25s1",   "Q25s2",   "Q25s3",   "Q25s4",   "Q25s5",  
                      "Q25s6",   "Q25s7",   "Q25s8",   "Q25s9",   "Q25s10",  "Q25s11", 
                      "Q25s12",  "Q26s1",   "Q26s2",   "Q26s3",   "Q26s4",   "Q26s5",  
                      "Q26s6",   "Q27s1",   "Q27s2",   "Q29",     "Q30",     "Q31s1",  
                      "Q31s2",   "Q31s3",   "Q32s1",   "Q32s2",   "Q32s3",   "Q35",    
                      "Q36",     "Q37",     "SSM",      "ISCO",    
                      "SSM8",    "Wave", "vac"   )

colnames(data_w4)<- c( "MID",       "F1",      "F2",      "F3",      "S1",      "F4",     
                       "F5t1",    "F5t2",    "Q1_1",    "Q1_2a",   "Q1_2at3", "Q1_2b",  
                       "Q1_2bt3", "Q2_1c1",  "Q2_1c2",  "Q2_1c3",  "Q2_1c4",  "Q2_2",   
                       "Q2_3",    "Q2_3t9",  "Q2_4",    "Q3t1",    "Q4",      "Q4t19",  
                       "Q5",      "Q6",      "Q7",      "Q8",      "Q8t7",    "Q9t1",   
                       "Q9t2",    "Q9t3",    "Q10",     "Q10t9",   "Q11",     "Q12",    
                       "Q13",     "Q14",     "Q15",     "Q15_2",   "Q16s1",   "Q16s2",  
                       "Q16s3",   "Q16s4",   "Q17",     "Q18",     "Q19s1",   "Q19s2",  
                       "Q19s3",   "Q19s4",   "Q19s5",   "Q19s6",   "Q20",     "Q21s1",  
                       "Q21s2",   "Q21s3",   "Q22c1",   "Q22c2",   "Q22c3",   "Q22c4",  
                       "Q22c5",   "Q22c6",   "Q22c7",   "Q22c8",   "Q22t7",   "Q23",    
                       "Q24",     "Q25s1",   "Q25s2",   "Q25s3",   "Q25s4",   "Q25s5",  
                       "Q25s6",   "Q25s7",   "Q25s8",   "Q25s9",   "Q25s10",  "Q25s11", 
                       "Q25s12",  "Q26s1",   "Q26s2",   "Q26s3",   "Q26s4",   "Q26s5",  
                       "Q26s6",   "Q27s1",   "Q27s2",   "Q29",     "Q30",     "Q31s1",  
                       "Q31s2",   "Q31s3",   "Q32s1",   "Q32s2",   "Q32s3",   "Q38",    
                       "Q39",     "Q36",     "Q37",     "SSM",  "ISCO",
                       "SSM8",   "Wave", "vac" )

data_l <- bind_rows(data_w1, data_w2)
data_l <- bind_rows(data_l, data_w3)
data_l <- bind_rows(data_l, data_w4)

nrow(data_l)

data_long <- arrange(data_l, MID, Wave)

write.csv(data_long, "SSPW21_panel_long_final.csv")
