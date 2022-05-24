install.packages("readxl") # for data importing
install.packages("zoo") # for date formatting

library(readxl)
library(zoo)

rm(list=ls()) # clear memory
graphics.off() # clear graphs
cat("\014") # clear console

# import data
Data <- read_excel("/Users/snel/Documents/Pre-Master Econometrics & Management Science (QF)/Time Series Analysis/Assignment_2/EXPORTS.xlsx")
Data$QUARTER <- as.yearqtr(Data$QUARTER) # correctly format date

