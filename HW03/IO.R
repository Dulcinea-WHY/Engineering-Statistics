#109612054
#2021.10.18_HW

#install.load package
install.packages("rio")
library(rio)

#import data
getwd()
setwd("C:/R/Engineering_Statistics/10.18")
xlsx <- import("weatherdata.xlsx")

#(a)
rain <- xlsx$Rain
date <- xlsx$Date
#maximum rainfall
index.max <- max(rain)
index.max
#date.time
index.date <- date[which.max(rain)]
index.date
#number
index.number <- length(index.max)
index.number

#(b) 
cat("The data number:", index.number
    , file = "Output.txt"
    , "\n")
cat("The maximum rainfall per 5 min.:", index.max, "mm"
    , file = "Output.txt"
    , "\n"
    , append = TRUE)
cat("The data and time:", index.date
    , file = "Output.txt"
    , "\n"
    , append = TRUE)
