getwd()

setwd("D:/R Assignment Emplay")
 
mydata <- read.csv("Smart_data_v6.csv")

names(mydata)

str(mydata)

summary(mydata)

View(mydata)

Index <- sample(seq_len(nrow(mydata)), size = size)

train_mydata <- mydata[Index, ]

test_mydata <- mydata[-Index, ]





