# loading library
library(fpp2)
library(forecast)
library(ggplot2)
library(tidyverse)
library(xts)

#loading data set
setwd("D:\\DSTI\\15 - Time Series\\")
my_data = read.csv("Elec-train.csv")

#Explore data
str(my_data)
summary(my_data)
head(my_data)
dim(my_data)

#Data wrangling
#Conversion field timeStamp in date format
start.date <- strptime("2010-1-1 1:15", "%Y-%m-%d %H:%M")
start.date <- format(start.date, "%Y-%m-%d %H:%M")

end.date <- strptime("2010-2-17 23:45", "%Y-%m-%d %H:%M")
end.date <- format(end.date, "%Y-%m-%d %H:%M")

my_date = seq.POSIXt(as.POSIXct(start.date), as.POSIXct(end.date), by = "15 min")

my_data$Timestamp = my_date
str(my_data)


#Data splitting between :
#my_data1 used to train and test forecasting
#my_data2 used for forecasting

my_data1 = na.omit(my_data)
my_data2 = my_data %>% filter(is.na(Power..kW.))


#Dataframe used for forecasting without Temperature
#my_data1 = my_data1 %>% select(1:3)  #???
#my_data2 = my_data2 %>% select(1:3)  #???

#Training (80%)and testing (20%) splitting of my_data1
#What about set.seed???
#my_data1_test = my_data1 %>% top_n(nrow(my_data1)/100*20, my_data1$Timestamp)
#my_data1_test  = my_data1 %>% top_n(nrow(my_data1)/100*80, my_data1$Timestamp)
#my_data1_train  = setdiff(my_data1,my_data1_test)


# Fourier analysis trial
# out of scoop ofthe exam
library(stats)
my_data1_f = fft(my_data1$Power..kW.) 
#summary(my_data1_f)
#head(my_data1_f)

plot(Mod(my_data1_f))
     


#time serie creation and visualization
#Serie seems to have a period of 24 hours
my_serie = ts(my_data1$Power..kW.,start= min(my_data1$Timestamp), freq=24*60/15)

# creating serie my_serie_xts for better visualization purpose
my_serie_xts = xts(my_data1$Power..kW., order.by =my_data1$Timestamp)

plot(my_serie_xts)
acf(my_serie_xts)
pacf(my_serie_xts)

#
ggseasonplot(my_serie)

#manual seasonal plot
plot(my_serie[1:96],type="l",ylim=c(min(my_serie),max(my_serie)))
for (i in 1:47) lines(my_serie[(1+96*i):(96*(i+1))])




#Some  statistic for time serie
mean(my_serie)
var(my_serie)
my_acf = acf(my_serie,type="cor", plot = FALSE)
my_acf$acf[1:10,1,1]

#in the acf graph, there are high positive correlations 
# that only slowly decline with increasing lags.This indicates a lot of autocorrelation
#and you will need to take that into account in your modeling.
#It seems that there is no long-term (linear) trend. 
#But it seems that there is a seasonal pattern, we can see it with the seasonal plot
# and by seeing auto-correlation table
# We guess a 24 hours period for the serie
#We noticed that there are some period of pic consumption (16h to 23 h)
# and period of less consumption (23h to 6h)
#We also noticed some cyclical  pattern which can be more detailed  using spectral analysis 
# with Fast Fourier Analysis



#Splitting between train and test
#We will use about 80% for the training data, to estimate model parameter
#and the 20% most recent, to evaluate forecast accuracy

my_serie_test = tail(my_serie,901)
my_serie_train  = head(my_serie, 3606)

#We can try to remove the linear trend
tmp1 =  diff(my_serie_train, lag = 96) 
ggtsdisplay(tmp1)
Box.test(tmp1, lag = 96)
autoplot(tmp1)

ggseasonplot(tmp1)
fit=Arima(tmp1, order=c(0,0,96), seasonal=c(0,1,1),lambda = "auto")
prev=forecast(fit,h=901)

cat('Arima: ',sqrt(mean((fit$mean-my_serie_test)^2)),'\n')

#Building and esting models 
#We will use several models and compare them to select the best one

fit1=holt(my_serie_train,h=901, damped=FALSE)
fit2=holt(my_serie_train,h=901, damped=TRUE)
fit3=auto.arima(my_serie_train)
prev3=forecast(fit3,h=901)
fit4=HoltWinters(my_serie_train,  seasonal = "additive")
prev4 = predict(fit4,n.ahead=901)



#We encounter issue with  freq > 24 for seasonal hw  damped 
#We use  seasonal HolWinters as workaround

#RMSE 
cat('Holt: ',sqrt(mean((fit1$mean-my_serie_test)^2)),'\n')
cat('Damped Holt: ',sqrt(mean((fit2$mean-my_serie_test)^2)),'\n')
cat('auto.arima: ',sqrt(mean((prev3$mean-my_serie_test)^2)),'\n')
cat('seasonal Holt-Winters: ',sqrt(mean((prev4-my_serie_test)^2)),'\n')

#We notice that the better model is auto.arima  with perform a RMSE=16.70526 
#Cross validation 
# It could better to use cross validation for better model selection


autoplot(my_serie_test) +
  autolayer(fit1$mean,series="Holt") +
  autolayer(fit2$mean,series="Damped Holt")+
  autolayer(prev3$mean,series="auto.arima")+
  autolayer(prev4,series="seasonal Holt-Winters")
 
#let's forecast 96 data using auto.arima
#now we will use all data1 set as training ()
fit6=auto.arima(my_serie)
prev6=forecast(fit6,h=96)

#Checking model
summary(fit6)
checkresiduals(fit6)
prev6$mean

#Forecasting with covariates
#Here we will use dynamic regression by using variable temperature


fit7=auto.arima(my_serie, ,xreg=my_data1$Temp..C..)
prev7=forecast(fit7,h=96,xreg=my_data2$Temp..C..)


summary(fit7)
checkresiduals(fit7)
prev7$mean






