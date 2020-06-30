library(TSA)
library(forecast) library(lmtest)
library(fGarch) library(CombMSC)
library(fUnitRoots)

penguin_arrival <- read.csv("C:/Users/santo/Downloads/penguindataset.csv") 
head(penguin_arrival)

str(penguin_arrival) # to find the structure of the dataset

# this chunk will convert the dataset from dataframe to time series
penguin_ts = ts(as.vector(penguin_arrival$Number),start=c(2000,1), end=c(2006,12), frequency= 12)
head(penguin_ts)
summary(penguin_ts) # to generate descriptive summary of the data #Time series plot
plot(penguin_ts,ylab='Count of Penguins arriving',xlab='Year(Time)',type='o', main = "Fig 1.Time series plot of monthly arrival of penguin in New zealand.")

# Autocovariance test
y = penguin_ts
x = zlag(penguin_ts) index = 2:length(x)
cor(y[index],x[index])


## scattter plot
plot(y=penguin_ts,x=zlag(penguin_ts), ylab='Penguin arrival', xlab = 'Year(Time)', main = 'Fi g 2.Scatter plot of Penguin Arrival in New zealand')

#Taking the intervention point
Penguin_ts_IP = ts(penguin_ts[50:84],start=c(2004,1), frequency=12)


# Time series plot
plot(Penguin_ts_IP,type="o",xlab='Year(Time)',at = seq(2004, 2007, by = 1), ylab='Count of Pe nguins arriving',main="Fig 3.Time series plot of monthly arrival of penguin in New zealand.")

# Autocovariance test
y = Penguin_ts_IP
x = zlag(Penguin_ts_IP) index = 2:length(x)
cor(y[index],x[index])

#scatter plot
plot(y=Penguin_ts_IP,x=zlag(Penguin_ts_IP), ylab='Penguin arrival', xlab = 'Year(Time)', main
= 'Fig 4.Scatter plot of Penguin Arrival in New zealand')

#ACF and PACF plots of original time series data
par(mfrow=c(1,2) , cex.lab = 0.9, cex.main=0.9, cex.axis = 0.9,mar = c(4,5,4,2)) acf(Penguin_ts_IP,	lag.max = 36,main="Fig 5.ACF Plot for data series (2000-06)")
pacf(Penguin_ts_IP,	lag.max = 36,main="Fig 6.PACF Plot for data series (2000-06)")

#Seasonal differencing with D = 1 and fit a white noise model
m1 = arima(Penguin_ts_IP,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12))
res_m1 = residuals(m1)
#Plot Time series graph and ACF and PACF plot
par(mfrow=c(3,1),mar = c(2,5,4,2))
plot(res_m1,at = seq(2004, 2007, by = 1),main="Fig 7.The Residual Time Series Plot",xlab='Yea r(Time)',ylab='Count of Penguins')
acf(res_m1,lag.max=36,main = "Fig 8.The ACF Plot of the residuals",xlab='Lag')
pacf(res_m1, lag.max=36,main = "Fig 9.The PACF Plot of the residuals",xlab='Lag') par(mfrow=c(1,1))

m3 = arima(Penguin_ts_IP,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12)) res_m3 = residuals(m3)
# plotting time series and ACF and PACF plot
par(mfrow=c(3,1),mar = c(2,5,4,2))
plot(res_m3,at = seq(2004, 2007, by = 1),type='l',ylab='Electric and gas utilities productio n',main = 'Time series graph of residuals' )
acf(res_m3,lag.max=36)
pacf(res_m3, lag.max=36) par(mfrow=c(1,1))

data_penguin_transform = BoxCox.ar(Penguin_ts_IP, method = "yule-walker") mtext("Fig 7: BoxCox Treansformation", outer=TRUE, cex=1,line=-3)

data_penguin_transform$ci

## Creating model with Seasonal component D=1
m4 = arima(Penguin_ts_IP,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12)) res_m4 = residuals(m4)
#Plotting ACF and PACF
par(mfrow=c(3,1),mar = c(2,5,4,2))
plot(res_m4,at = seq(2004, 2007, by = 1),type='l',ylab='Electric and gas utilities productio n',main = 'Time series graph of residuals' )
acf(res_m4,lag.max=36)
pacf(res_m4, lag.max=36) par(mfrow=c(1,1))

#This chunk generates the EACF plot for the series
eacf(res_m4,ar.max = 6, ma.max = 6)

res = armasubsets(y=res_m1 ,nar=12,nma=12,y.name='test',ar.method='yule-walker') #Generate BI C
plot(res)

#This chunk fits the first candidate model and performs coeftest on the fit and plots the ACF and PACF for the residual
model1_001_010 = arima(Penguin_ts_IP,order=c(0,0,1),seasonal=list(order=c(0,1,0), period=12))
#model fitting
coeftest(model1_001_010) #coeftest

#generate the residual
res.m1 = residuals(model1_001_010);
#generate the ACF and PACF
par(mfrow=c(2,1),mar = c(2,1,4,2))
acf(res.m1, lag.max = 36, main = "Fig 10.ACF Plot for SARIMA(0,0,1)x(0,1,0)")
pacf(res.m1, lag.max = 36, main = "Fig 11.PACF Plot for SARIMA(0,0,1)x(0,1,0)")

#This chunk fits the first candidate model and performs coeftest on the fit and plots the ACF and PACF for the residual
model2_002_010 = arima(Penguin_ts_IP,order=c(0,0,2),seasonal=list(order=c(0,1,0), period=12))

#model fitting
coeftest(model2_002_010) #coeftest

#generate the residual
res.m2 = residuals(model2_002_010);
#generate the ACF and PACF
par(mfrow=c(2,1),mar = c(2,3,4,2))
acf(res.m2, lag.max = 36, main = "Fig 12.ACF Plot for SARIMA(0,0,2)x(0,1,0)")
pacf(res.m2, lag.max = 36, main = "Fig 13.PACF Plot for SARIMA(0,0,2)x(0,1,0)")

#This chunk fits the first candidate model and performs coeftest on the fit and plots the ACF and PACF for the residual
model3_101_010 = arima(Penguin_ts_IP,order=c(1,0,1),seasonal=list(order=c(0,1,0), period=12))
#model fitting
coeftest(model3_101_010) #coeftest

#generate the residual
res.m3 = residuals(model3_101_010);
#generate the ACF and PACF
par(mfrow=c(2,1),mar = c(2,2,4,2))
acf(res.m3, lag.max = 36, main = "Fig 14.ACF Plot for SARIMA(1,0,1)x(0,1,0)")
pacf(res.m3, lag.max = 36, main = "Fig 15.PACF Plot for SARIMA(1,0,1)x(0,1,0)")

#This chunk fits the first candidate model and performs coeftest on the fit and plots the ACF and PACF for the residual
model4_102_010 = arima(Penguin_ts_IP,order=c(1,0,2),seasonal=list(order=c(0,1,0), period=12))
#model fitting
coeftest(model4_102_010) #coeftest

#generate the residual
res.m4 = residuals(model4_102_010);
#generate the ACF and PACF
par(mfrow=c(2,1),mar = c(2,5,4,2))
acf(res.m4, lag.max = 36, main = "Fig 16.ACF Plot for SARIMA(1,0,2)x(0,1,0)")
pacf(res.m4, lag.max = 36, main = "Fig 17.PACF Plot for SARIMA(1,0,2)x(0,1,0)")

# user defined function to calculate AIC score
sort.score <- function(x, score = c("bic", "aic")){
if (score == "aic"){
x[with(x, order(AIC)),]
} else if (score == "bic") { x[with(x, order(BIC)),]
} else {
warning('score = "x" only accepts valid arguments ("aic","bic")')
}
}

#This chunk performs the AIC scoring for the candidate models
sort.score(AIC(model1_001_010, model2_002_010, model3_101_010, model4_102_010),score="aic")

residual.analysis <- function(model, std = TRUE){
library(TSA)
library(FitAR)
if (std == TRUE){
res.model = rstandard(model)
}else{
res.model = residuals(model)
}


par(mfrow=c(3,2),mar = c(2,6,4,2))
plot(res.model,type='o',at = seq(2004, 2007, by = 1),ylab='Standardised residuals', main="Fig 18.Time series plot of standardised residuals")
abline(h=0) k=0
hist(res.model,main="Fig 20.Histogram of standardised residuals") qqnorm(res.model,main="Fig 19.QQ plot of standardised residuals") qqline(res.model, col = 2)
acf(res.model,main="Fig 21.ACF of standardised residuals") print(shapiro.test(res.model))
LBQPlot(res.model, lag.max = length(model$residuals)-1 , StartLag = k + 1, k = 0, SquaredQ = FALSE)
}

#This chunk performs residual analysis
residual.analysis(model3_101_010)

m1.penguin = Arima(Penguin_ts_IP,order=c(1,0,1),seasonal=list(order=c(0,1,0), period=12)) future=forecast(m1.penguin, h = 24)

summary(future)

plot(future,xlab=("Year(Time)"),ylab="Freq",main="Fig 22.Forecasting for the next 2 years")
