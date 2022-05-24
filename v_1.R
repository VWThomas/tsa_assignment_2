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

# ----- 2.A : Data exploration & identifying key features -------- #
# ----- i : Trend ------
# ----- ii : Seasonality ------
# ----- iii : Aberrant observations ------
# ----- iv : Heteroskedasticity ------
# ----- v : Non-linearity ------

# ----- 2.B : ADF test -------- #

# ----- 2.C : Model estimation & mis-specification tests -------- #
# ----- i : No residual autocorrelation ------
# ----- ii : Test of homoskedasticity ------
# ----- iii : Test of normality ------

# ----- 2.D : Model re-estimation & forecasting -------- #
# ----- i : Unbiasedness ------
# ----- ii : Accuracy ------
# ----- iii : Efficiency ------

# ----- 2.E : Moving window forecasting -------- #
