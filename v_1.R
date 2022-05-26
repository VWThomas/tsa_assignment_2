install.packages("readxl")  # for data importing
install.packages("zoo")     # for date formatting
install.packages("ggplot2") # for plotting
install.packages("car")     # linear model test
install.packages("tseries") # for ADF test
install.packages("forecast")# for ARIMA model building

library(readxl)
library(zoo)
library(ggplot2)
library(car)
library(tseries)
library(forecast)


rm(list=ls()) # clear memory
graphics.off() # clear graphs
cat("\014") # clear console

# import data
Data <- read_excel("/Users/ThomasVW/Documents/Time Series Analysis/Assignment 2/EXPORTS.xlsx")
Data$QUARTER <- as.yearqtr(Data$QUARTER) # correctly format date

# ----- 2.A : Data exploration & identifying key features -------- #

#Only use data starting at 1980Q1
Subdata <- Data[5:172,]

#transform data
Subdata$ldiff_EXPORTS <- c(NA, diff(log(Subdata$EXPORTS))) #log of first differences

# ----- i : Trend ------
plot1 <- ggplot(data = Subdata, mapping = aes(x = QUARTER, y = EXPORTS)) + geom_line(lwd=0.5) +
  labs(y = "Exports", x = "Time (quarter)")
plot1 #plot of Exports vs quarter

T <-nrow(Subdata)        # number of time series observations
Subdata$time <- seq(1,T) # linear trend

summary(lm(EXPORTS ~ time, data = Subdata))  # Summary statistics of model with linear time trend
# ----- ii : Seasonality ------

#Create seasonal dummy variables
Subdata$Q1 <- rep(c(1, 0, 0, 0), T/4)
Subdata$Q2 <- rep(c(0, 1, 0, 0), T/4)
Subdata$Q3 <- rep(c(0, 0, 1, 0), T/4)
Subdata$Q4 <- rep(c(0, 0, 0, 1), T/4)

#Vector of quarters plot
q1 <- subset(Subdata$EXPORTS, Subdata$Q1 == 1)
q2 <- subset(Subdata$EXPORTS, Subdata$Q2 == 1)
q3 <- subset(Subdata$EXPORTS, Subdata$Q3 == 1)
q4 <- subset(Subdata$EXPORTS, Subdata$Q4 == 1)

time <- seq(1980,2021)
df1 <- data.frame(q1, q2, q3, q4, time)

plot2 <- ggplot(data = df1, mapping = aes(x=time)) +
  geom_line(aes(y = q1, color = "Q1"), lwd=0.5) +
  geom_line(aes(y = q2, color = "Q2"), lwd=0.5) +
  geom_line(aes(y = q3, color = "Q3"), lwd=0.5) +
  geom_line(aes(y = q4, color = "Q4"), lwd=0.5) +
  labs(y = "Quarterly exports", x = "Time (years)")
scale_color_manual("", breaks = c("Q1","Q2","Q3", "Q4"), values = c("darkred", "steelblue", "green", "yellow"))
plot2

#Vector of quarters plot log of first differences

q1_ld <- subset(Subdata$ldiff_EXPORTS, Subdata$Q1 == 1)
q2_ld <- subset(Subdata$ldiff_EXPORTS, Subdata$Q2 == 1)
q3_ld <- subset(Subdata$ldiff_EXPORTS, Subdata$Q3 == 1)
q4_ld <- subset(Subdata$ldiff_EXPORTS, Subdata$Q4 == 1)

df2 <- data.frame(q1_ld, q2_ld, q3_ld, q4_ld, time)

plot3 <- ggplot(data = df2, mapping = aes(x=time)) +
  geom_line(aes(y = q1_ld, color = "Q1"), lwd=0.5) +
  geom_line(aes(y = q2_ld, color = "Q2"), lwd=0.5) +
  geom_line(aes(y = q3_ld, color = "Q3"), lwd=0.5) +
  geom_line(aes(y = q4_ld, color = "Q4"), lwd=0.5) +
  labs(y = "Quarterly exports (log of 1st differences", x = "Time (years)")
scale_color_manual("", breaks = c("Q1","Q2","Q3", "Q4"), values = c("darkred", "steelblue", "green", "yellow"))
plot3


#regression of log of first differences
fit.ldiff_EXPORTS <- lm(ldiff_EXPORTS[2:T] ~ Q1[2:T] + Q2[2:T] + Q3[2:T] + Q4[2:T] - 1, data = Subdata)
summary(fit.ldiff_EXPORTS)

# ----- iii : Aberrant observations ------


ldiff_EXPORTS_na <- subset(Subdata$ldiff_EXPORTS, Subdata$DREC != 1)
time_na <- subset(Subdata$QUARTER, Subdata$DREC != 1)
data_na = data.frame(ldiff_EXPORTS_na, time_na)

sd(Subdata$ldiff_EXPORTS[2:168])
sd(data_na$ldiff_EXPORTS_na[2:148])

plot4 <- ggplot(data = data_na, mapping = aes(y = ldiff_EXPORTS_na, x = time_na)) + geom_line(lwd=0.5)
plot4
# ----- iv : Heteroskedasticity ------

# plot of first differences
plot5 <- ggplot(data = Subdata, mapping = aes(x = QUARTER, y = ldiff_EXPORTS)) + geom_line(lwd=0.5)
plot5 

#Check for differences in residual stand. dev. between suspected periods
sd(Subdata$ldiff_EXPORTS[2:91])   #1980Q2 - 2002Q3
sd(Subdata$ldiff_EXPORTS[92:168]) #2002Q4 - 2021Q4

#Check for differences in residual stand. dev. when omitting probable aberrant observations
sd(data_na$ldiff_EXPORTS_na[2:79]) #1980Q2 - 2002Q3
sd(data_na$ldiff_EXPORTS_na[80:148])#2002Q4 - 2021Q4

# ----- v : Non-linearity ------
Subdata$IDREC <- c(1 - Subdata$DREC)
fit.nonlinear <- lm(ldiff_EXPORTS[2:T] ~ DREC[2:T] + IDREC[2:T] - 1, data = Subdata)
summary(fit.nonlinear)
linearHypothesis(fit.nonlinear, c("DREC[2:T] = IDREC[2:T]"))

# ----- 2.B : ADF test -------- #

#Create log of seasonally adjusted export series
Subdata$log_EXPORTSSA <- c(log(Subdata$EXPORTSSA))

#Plot of log seasonally adjusted export series
plot6 <- ggplot(data = Subdata, mapping = aes(y = log_EXPORTSSA, x = QUARTER)) + geom_line(lwd=0.5)
plot6

#Check for lag choice and perform ADF test
pacf(Subdata$log_EXPORTSSA)
adf.test(Subdata$log_EXPORTSSA, k = 1)

#ADF test for sample period 1980Q1 - 2011Q4 
sub_log_EXPORTSSA = c(Subdata$log_EXPORTSSA[1:128])
pacf(sub_log_EXPORTSSA)
adf.test(sub_log_EXPORTSSA, k = 1)
# ----- 2.C : Model estimation & mis-specification tests -------- #

#AR(2) model with intercept and deterministic trend
trend <- seq_along(Subdata$log_EXPORTSSA)
AR_2 <- arima(Subdata$log_EXPORTSSA, order = c(2, 0, 0), xreg = trend)

#Find roots of phi(z)=1-ar1*z-ar2*z^2
summary(AR_2)
checkresiduals(AR_2)

# ----- i : No residual autocorrelation ------

u <- AR_2$residuals # residuals of AR(3)
acf(u)   # check if residuals are autocorrelated 

# ----- ii : Test of homoskedasticity ------

acf(u^2) # Check for autocorrelation in squared residuals

# ----- iii : Test of normality ------

skewness(u)
kurtosis(u)
jarque.bera.test(u)

# remove 2 suspected aberrant observations and test again

which.max(u)
which.min(u)

skewness(u[-c(162,163)])
kurtosis(u[-c(162,163)])

jarque.bera.test(u[-c(162,163)])

# ----- 2.D : Model re-estimation & forecasting -------- #

# create new subset for the sample period 1980Q1 – 1999Q4
Sub2data <- Subdata[1:80,]

# re-estimate AR(2) parameters for new subset
sub_trend <- seq_along(Sub2data$log_EXPORTSSA)
sub_AR_2 <- arima(Sub2data$log_EXPORTSSA, order = c(2, 0, 0), xreg = sub_trend)


# create one-step ahead point forecasts for the period 2000Q1 – 2021Q4
H <- 88
sub_AR_2_pred <- rep(0, H) # empty vector for point forecasts
sub_AR_2_lower <- rep(0, H) # empty vector for lower bound forecasts
sub_AR_2_upper <- rep(0, H) # empty vector for upper bound forecasts

for (t in 1:H) {
  trend <- seq_along(Subdata$log_EXPORTSSA[(t):(T-H+t-1)]) 
  sub_AR_2_ts <- Arima(Subdata$log_EXPORTSSA[(t):(T-H+t-1)] , order = c(2, 0, 0), xreg = trend) # AR(2) model
  sub_AR_forecast <- forecast(sub_AR_2_ts, h = 1, xreg = trend[80])
  sub_AR_2_pred[t] <- as.numeric(sub_AR_forecast$mean[1]) # point forecasts
  sub_AR_2_lower[t] <- as.numeric(sub_AR_forecast$lower[2]) # lower bound 95%
  sub_AR_2_upper[t] <- as.numeric(sub_AR_forecast$upper[2]) # upper bound 95%
}


# don't need this plot part, just for showcasing that it works
forecast_timeline <- Subdata$QUARTER[(T-H+1):T]
dataact <- Subdata$log_EXPORTSSA[(T-H+1):T]
forecast_plot <- data.frame(forecast_timeline, sub_AR_2_pred, sub_AR_2_lower, sub_AR_2_upper, dataact)

plot7 <- ggplot(data = forecast_plot, mapping = aes(x = forecast_timeline)) +
  geom_line(aes(y = sub_AR_2_pred, color = "Point Forecast"), lwd = 0.5) +
  geom_line(aes(y = dataact, color = "Actual Data"), lwd = 0.5) +
  geom_line(linetype = "dashed", aes(y = sub_AR_2_lower, color = "95% Interval Forecast Bounds"), lwd = 0.5) +
  geom_line(linetype = "dashed", aes(y = sub_AR_2_upper, color = "95% Interval Forecast Bounds"), lwd = 0.5) +
  scale_colour_manual("", breaks = c("Point Forecast", "Actual Data", "95% Interval Forecast Bounds"),
                      values = c("darkred", "seagreen","black"))

plot7 

# convert forecasts to quarterly growth rates
qgr <- rep(0, H) # create empty vector for quarterly growth rates

for (t in 1:H) {
  qgr[t] = 100 * (sub_AR_2_pred[t] - Subdata$log_EXPORTSSA[t+79])
}
qgr_timeline <- Subdata$QUARTER[(T-H+1):T]

qgr_plot <- data.frame(qgr, qgr_timeline)

plot8 <- ggplot(data = qgr_plot, mapping = aes(y = qgr, x = qgr_timeline)) + geom_line(lwd=0.5)
plot8

# ----- i : Unbiasedness ------
# ----- ii : Accuracy ------
# ----- iii : Efficiency ------
# ----- 2.E : Moving window forecasting -------- #
