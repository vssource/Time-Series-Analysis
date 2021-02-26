library(quantmod)
library(forecast)

goog <- read.csv("GOOG.csv")
tail(goog,100)
#keeping origial dataset and copying weekly.data dataframe for analysis purpose
weekly.data <- goog[,c(1,6)]
macd <- MACD( weekly.data$Adj.Close, 12, 26, 9, maType="EMA" , percent = FALSE)
weekly.data$MACDLine <- macd[,1]
weekly.data$SignalLine <- macd[,2]

#remove na
summary(weekly.data)
dim(weekly.data)
weekly.data <- weekly.data[complete.cases(weekly.data$MACDLine,weekly.data$SignalLine), ]
tail(weekly.data)


#create timeseris data
macdline.ts <- ts(weekly.data$MACDLine, start = c(2016,1), frequency = 52)
signalline.ts <- ts(weekly.data$SignalLine, start = c(2016,1), frequency = 52)

macdline.stl <- stl(macdline.ts, s.window = "periodic")
autoplot(macdline.stl, main = "MACDLine Time Series Components")

signalline.stl <- stl(signalline.ts, s.window = "periodic")
autoplot(signalline.stl, main = "SignalLine Time Series Components")

Acf(macdline.ts, lag.max = 52, main = "Autocorrelation of MACDLine")
Acf(signalline.ts, lag.max = 52, main = "Autocorrelation of SignalLine")


#Approch1: Apply AR(1) to check predicatability of data
macd.ar1 <- Arima(macdline.ts, order = c(1,0,0))
signal.ar1 <- Arima(signalline.ts, order = c(1,0,0))

summary(macd.ar1)
summary(signal.ar1)

#Approch2: by differencing of lag-1

# Create differenced data using (lag-1).
diff.macd.ts <- diff(macdline.ts, lag = 1)
diff.signal.ts <- diff(signalline.ts, lag = 1)

# Use Acf() function to identify autocorrealtion for differenced 
# plot autocorrelation for different lags 
Acf(diff.macd.ts, lag.max = 104, main = "Autocorrelation for Differenced MACDLine")
Acf(diff.signal.ts, lag.max = 104, main = "Autocorrelation for Differenced SignalLine")


#plot(macdline.ts, main = "MACD Indicator", xlab = "Time", ylab = "MACD VALUEs", ylim = c(-50,100), col = "blue", lwd = 2)
#lines(signalline.ts, col = "red", lwd = 2)

#Partition Data ####
set.seed(123)
#total datapoints are 226 and 40% of 226 is 50 for validation period
valid.rows <- 50 
train.rows <- length(macdline.ts) - valid.rows
macdline.train <- window(macdline.ts, start = c(2016, 1), end = c(2016, train.rows))
macdline.valid <- window(macdline.ts, start = c(2016, train.rows + 1))
signalline.train <- window(signalline.ts, start = c(2016, 1), end = c(2016, train.rows))
signalline.valid <- window(signalline.ts, start = c(2016, train.rows + 1))


plot(macdline.train, 
     xlab = "Time", ylab = "MACD Data", ylim = c(-50,150), bty = "l", col = "blue",
     xlim = c(2016, 2021), main = "Partition of Data", lwd = 2) 
#axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(signalline.train, col = "red", lwd=2)
lines(macdline.valid, col = "blue", lwd = 2)
lines(signalline.valid, col = "red", lwd = 2)
legend(c(2016.50,2016.50),c(130,130), legend = c("MACDLine Training Data", "SignalLine Training Data", 
                                                 "MACDLine Validation Data", "SignalLine Validation Data"), 
       col = c("blue", "red", "blue", "red"), 
       lty = c(1,1,1,1), lwd =c(2,2,2,2), bty = "n")
lines(c(2016, 2016), c(-50, 150))
lines(c(2019.442, 2019.442), c(-50, 150))
text(2017, 140, "Training")
text(2020.10, 140, "Validation")
arrows(2019.442, 142, 2016, 142, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2020.385, 142, 2019.443, 142, code = 3, length = 0.1, lwd = 1, angle = 30)


#### Model 1: Regression with 3 degree polynomial Trend ####
macdline.reg <- tslm(macdline.train ~ trend + I(trend^2) + I(trend^3))
signalline.reg <- tslm(signalline.train ~ trend + I(trend^2) + I(trend^3))

summary(macdline.reg)
summary(signalline.reg)

#forecast of regression data
macdline.reg.pred <- forecast(macdline.reg, h = valid.rows, level = 0)
signalline.reg.pred <- forecast(signalline.reg, h = valid.rows, level = 0)

plot(macdline.ts, 
     xlab = "Time", ylab = "MACD Data", ylim = c(-50,150), bty = "l", col = "blue",
     xlim = c(2016, 2021), main = "3 degree polynomial Trend for Training and Validation Data", lwd = 2) 
#axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(signalline.ts, col = "red", lwd=2)
lines(macdline.reg.pred$fitted, col = "blue", lwd = 2, lty = 2)
lines(signalline.reg.pred$fitted, col = "red", lwd = 2, lty = 2)
lines(macdline.reg.pred$mean, col = "blue", lwd = 2, lty = 5)
lines(signalline.reg.pred$mean, col = "red", lwd = 2, lty = 5)
#lines(macdline.valid, col = "black", lty = 1)
#lines(signalline.valid, col = "black", lty = 1)
legend(c(2016.50,2016.50),c(130,130), legend = c("MACDLine Time Series", "SignalLine Time Series", "Linear Regression for MACDline_Training Data",
                                 "Linear Regression for Signalline_Training Data",
                               "Linear Forecast for MACDline_Validation Data", "Linear Forecast for Signaliline_Validation Data"), 
       col = c("blue", "red", "blue", "red", "blue","red"), 
       lty = c(1,1,2,2,5,5), lwd =c(2,2,2,2,2,2), bty = "n")
lines(c(2016, 2016), c(-50, 150))
lines(c(2019.442, 2019.442), c(-50, 150))
text(2017, 140, "Training")
text(2020.10, 140, "Validation")
arrows(2019.442, 142, 2016, 142, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2020.385, 142, 2019.443, 142, code = 3, length = 0.1, lwd = 1, angle = 30)

#Residuals for Linear Regression
acf(macdline.reg.pred$residuals, lag.max = 104, main = "MACDLine: Residuals of Regression model")
acf(signalline.reg.pred$residuals, lag.max = 104, main = "SignalLine: Residuals of Regression model")

#Apply AR(1) for residuals of residuals
macdline.res.ar3 <- arima(macdline.reg.pred$residuals, order = c(3,0,0))
summary(macdline.res.ar3)
signalline.res.ar3 <- arima(signalline.reg.pred$residuals, order = c(3,0,0))
summary(signalline.res.ar3)

acf(macdline.res.ar3$residuals, lag.max = 104, main = "MACDLine: Residuals of Residuals Regression model")
acf(signalline.res.ar3$residuals, lag.max = 104, main = "SignalLine: Residuals of Residuals Regression model")

#forecast residuals of residuals
macdline.res.ar3.pred <- forecast(macdline.res.ar3, h = valid.rows, level = 0)
signalline.res.ar3.pred <- forecast(signalline.res.ar3, h = valid.rows, level = 0)


#two-level combine forecast for validation data of regression model + residuals of regression model
macdline.2level.valid.pred <- macdline.reg.pred$mean + macdline.res.ar3.pred$mean
signalline.2level.valid.pred <- signalline.reg.pred$mean + signalline.res.ar3.pred$mean

regression.valid.df <- data.frame(macdline.valid, macdline.reg.pred$mean, macdline.res.ar3.pred$mean, macdline.2level.valid.pred , 
                       signalline.valid, signalline.reg.pred$mean , signalline.res.ar3.pred$mean, signalline.2level.valid.pred )
names(regression.valid.df) <- c("MACDLine validation data", "MACDLine Reg.Forecast", "MACDLine AR(1) Forecast", "MACDLine Combined.Forecast",
                     "SignalLine validation data", "SignalLine Reg.Forecast", "SignalLine AR(1) Forecast", "SignalLine Combined.Forecast")
regression.valid.df

round(accuracy(macdline.2level.valid.pred, macdline.valid),3)
round(accuracy(signalline.2level.valid.pred, signalline.valid),3)
round(accuracy(snaive(macdline.ts)$fitted, macdline.ts),3)
round(accuracy(naive(macdline.ts)$fitted, macdline.ts),3)

#### Model 2 - Holt - Winter's Model - Automatic on training data ####
macdline.hw.ZZZ <- stlf(macdline.train, etsmodel = "ZZZ", h = valid.rows, level = 0)
signalline.hw.ZZZ <- stlf(signalline.train, etsmodel = "ZZZ", h = valid.rows, level = 0)

macdline.hw.ZZZ$model
signalline.hw.ZZZ$model

plot(macdline.ts, 
     xlab = "Time", ylab = "MACD Data", ylim = c(-50,150), bty = "l", col = "blue",
     xlim = c(2016, 2021), main = "Holt Model forecast for Training and Validation Data", lwd = 2) 
lines(signalline.ts, col = "orange", lwd=2)
lines(macdline.hw.ZZZ$fitted, col = "blue", lwd = 2, lty = 4)
lines(signalline.hw.ZZZ$fitted, col = "red", lwd = 2, lty = 4)
lines(macdline.hw.ZZZ$mean, col = "blue", lwd = 2, lty = 2)
lines(signalline.hw.ZZZ$mean, col = "red", lwd = 2, lty = 2)
legend(c(2016.50, 2016.50), c(70, 130), legend = c("MACDLine Time Series", "SignalLine Time Series", "MACDline_Training Data",
                                 "Signalline_Training Data",
                                 "Forecast for MACDline_Validation Data", "Forecast for Signaliline_Validation Data"), 
       col = c("blue", "orange", "blue", "red", "blue","red"), 
       lty = c(1,1,4,4,2,2), lwd =c(2,2,2,2,2,2), bty = "n")
lines(c(2016, 2016), c(-50, 150))
lines(c(2019.442, 2019.442), c(-50, 150))
text(2017, 140, "Training")
text(2020.10, 140, "Validation")
arrows(2019.442, 142, 2016, 142, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2020.385, 142, 2019.443, 142, code = 3, length = 0.1, lwd = 1, angle = 30)

#point forecast for validation data
Holt.pred.df <- data.frame(macdline.hw.ZZZ$mean, signalline.hw.ZZZ$mean)
names(Holt.pred.df) <- c("Forecast for MACDline_Validation Data","Forecast for Signaliline_Validation Data")
Holt.pred.df

#accuracy of HW model ####
round(accuracy(macdline.hw.ZZZ, macdline.valid),3)
round(accuracy(signalline.hw.ZZZ, signalline.valid),3)


#### MOdel 3- Arima model on training data ####

# Use Arima() function to fit ARIMA c(3,0,1) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
macd.train.arima <- Arima(macdline.train, order = c(3,0,1)) 
signal.train.arima <- Arima(signalline.train, order = c(3,0,1)) 
summary(macd.train.arima)
summary(signal.train.arima)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
macd.train.arima.pred <- forecast(macd.train.arima, h = valid.rows, level = 0)
macd.train.arima.pred

signal.train.arima.pred <- forecast(signal.train.arima, h = valid.rows, level = 0)
signal.train.arima.pred

# Use Acf() function to create autocorrelation chart of ARIMA(3,0,1)
# model residuals.
Acf(macd.train.arima$residuals, lag.max = 104, 
    main = "MACDLine: Autocorrelations of ARIMA Model Residuals")

Acf(signal.train.arima$residuals, lag.max = 104, 
    main = "SignalLine: Autocorrelations of ARIMA Model Residuals")

round(accuracy(macd.train.arima.pred$mean, macdline.valid), 3)
round(accuracy(signal.train.arima.pred$mean, macdline.valid), 3)


#### Model 4 - Auto Arima model on training data ####

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
macd.train.auto.arima <- auto.arima(macdline.train)
summary(macd.train.auto.arima)

signal.train.auto.arima <- auto.arima(signalline.train)
summary(signal.train.auto.arima)


# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
macd.train.auto.arima.pred <- forecast(macd.train.auto.arima, h = valid.rows, level = 0)
macd.train.auto.arima.pred

signal.train.auto.arima.pred <- forecast(signal.train.auto.arima, h = valid.rows, level = 0)
signal.train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(macd.train.auto.arima$residuals, lag.max = 104, 
    main = "MACDLine: Autocorrelations of Auto ARIMA Model Residuals")

Acf(signal.train.auto.arima$residuals, lag.max = 104, 
    main = "SignalLine: Autocorrelations of Auto ARIMA Model Residuals")

round(accuracy(macd.train.auto.arima.pred$mean, macdline.valid), 3)
round(accuracy(signal.train.auto.arima.pred$mean, macdline.valid), 3)


# compare accuracy of all 4 models for MACDLine ####
#1. two-level combined (Regression trend with 3rd order polynomial + AR(3) of Residuals)
#2. Auto HW Model ZZZ
#3. ARIMA (3,0,1)
#4. AUTO ARIMA
round(accuracy(macdline.2level.valid.pred, macdline.valid),3)
round(accuracy(macdline.hw.ZZZ$mean, macdline.valid),3)
round(accuracy(macd.train.arima.pred$mean, macdline.valid), 3)
round(accuracy(macd.train.auto.arima.pred$mean, macdline.valid), 3)

# compare accuracy of all 5 models for MACDLine ####
round(accuracy(signalline.2level.valid.pred, signalline.valid),3)
round(accuracy(signalline.hw.ZZZ$mean, signalline.valid),3)
round(accuracy(signal.train.arima.pred$mean, macdline.valid), 3)
round(accuracy(signal.train.auto.arima.pred$mean, macdline.valid), 3)


#### For Entire data Set ####
#BEST MODEL : AUTO ARIMA
## Auto Arima Model ####

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
macd.auto.arima <- auto.arima(macdline.ts)
summary(macd.auto.arima)

signal.auto.arima <- auto.arima(signalline.ts)
summary(signal.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
macd.auto.arima.pred <- forecast(macd.auto.arima, h = 8, level = 0)
signal.auto.arima.pred <- forecast(signal.auto.arima, h = 8, level = 0)

final.forecast.df <- data.frame(macd.auto.arima.pred$mean,signal.auto.arima.pred$mean)
names(final.forecast.df) <- c("Forecast for MACDLine","Forecast for SignalLine")
final.forecast.df

plot(macdline.ts, 
     xlab = "Time", ylab = "MACD Data", ylim = c(-50,150), bty = "l", col = "blue",
     xlim = c(2016, 2021), main = "Forecast for entire dataset", lwd = 2) 
lines(signalline.ts, col = "red", lwd=2)
lines(macd.auto.arima.pred$mean, col = "blue", lwd = 2, lty = 2)
lines(signal.auto.arima.pred$mean, col = "red", lwd = 2, lty = 2)
legend(c(2016.50, 2016.50), c(70, 125), legend = c("MACDLine Time Series", "SignalLine Time Series",
                                 "Auto ARIMA Forecast for MACDline", "Auto ARIMA Forecast for Signaliline"), 
       col = c("blue", "red", "blue", "red"), 
       lty = c(1,1,2,2), lwd =c(2,2,2,2), bty = "n")
lines(c(2016, 2016), c(-50, 150))
lines(c(2020.400, 2020.400), c(-50, 150))
text(2018, 130, "Historical Data")
text(2020.80, 130, "Future")
arrows(2020.400, 140, 2016, 140, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2021, 140, 2020.400, 140, code = 3, length = 0.1, lwd = 1, angle = 30)

#1. Naive
#2. Auto ARIMA
round(accuracy(naive(macdline.ts)$fitted, macdline.ts),3)
round(accuracy(macd.auto.arima.pred$fitted, macdline.valid), 3)

round(accuracy(naive(signalline.ts)$fitted, signalline.ts),3)
round(accuracy(signal.auto.arima.pred$fitted, signalline.ts), 3)
