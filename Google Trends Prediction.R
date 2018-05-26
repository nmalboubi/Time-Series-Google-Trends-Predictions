
#Importing Data into R
#Cleaning up Data
Data1 = read.csv ("C:\\Users\\Owner\\Desktop\\q1train.csv")
Data1 = ts(Data1)
Data1 = Data1[,2]



Data2 = read.csv ("C:\\Users\\Owner\\Desktop\\q2train.csv")
Data2 = ts(Data2)
Data2 = Data2[,2]


Data3 = read.csv ("C:\\Users\\Owner\\Desktop\\q3train.csv")
Data3 = ts(Data3)
Data3 = Data3[,2]


Data4 = read.csv ("C:\\Users\\Owner\\Desktop\\q4train.csv")
ts(Data4)
Data4 = Data4[,2]


Data5 = read.csv ("C:\\Users\\Owner\\Desktop\\q5train.csv")
Data5 = ts(Data5)
Data5 = Data5[,2]




####################################################################################
#Function
timeseries_cv = function(data,order1,order2){
  cv_score = vector()
  num_years = length(Data3) %/% 52
  for(ii in 2:(num_years-1)){
    train_idx = 1:c(52*ii)
    test_idx = (52*ii+1):(52*(ii+1))
    print(ii*52)
    model = arima(data[train_idx],order = order1, seasonal = list(order=order2,period=52), method ="ML")
    print('testing')
    preds = predict(model,n.ahead = 52)
    print(preds$pred)
    cv_score[ii] = sqrt(mean((preds$pred - data[test_idx])^2))
  }
  return(cv_score)
}

#############################################################
#Data Trend 1
Data1 = read.csv ("C:\\Users\\Owner\\Desktop\\q1train.csv")
Data1 = ts(Data1)
Data1 = Data1[,2]


plot(Data1, type="l", xlab = "Time", ylab="Data", main = "Google Trends Data 1")

Data1 = Data1 [16:431]

diff1 = diff(Data1)
diff2 = diff(diff1, lag = 52)



acf(diff1, lag.max = 200)
pacf(diff1, lag.max = 200)

acf(diff2, lag.max = 200)
pacf(diff2, lag.max = 200)



#MA(1) X MA(1) (52)
pre011011 = timeseries_cv(Data1,c(0,1,1),c(0,1,1))
mean (pre011011, na.rm = TRUE)
#3.371581

lest1 = arima(Data1,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 52))$aic
#1759.21


#MA(2) X AR(2) (52)
pre012210 = timeseries_cv(Data1, c(0,1,2), c(2,1,0))
mean(pre012210, na.rm = TRUE)
#3.543926




#MA(2) X MA(1) (52)
pre012011 = timeseries_cv(Data1, c(0,1,2), c(0,1,1))
mean (pre012011, na.rm = TRUE)
#3.426721

lest2 = arima(Data1, order = c(0,1,2), seasonal = list(order = c(0,1,1), period = 52))$aic
#1753.399


#MA(1) X AR(1) (52)
pre011110 = timeseries_cv (Data1, c(0,1,1), c(1,1,0))
mean(pre011110, na.rm = TRUE)
#3.535633

lest3 = arima(Data1, order = c(0,1,2), seasonal = list(order = c(0,1,1), period = 52))$aic
#1753.399


model1 = arima(Data1,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 52))
preds1 = predict(model1,n.ahead=104)
plot(preds1$pred, main = "Prediction for Data 1", ylab = 'Predicted Data')
breakpoint = length(Data1)
combine = ts(c(Data1,preds1$pred))
plot(combine,col='black', ylab = "Data", main = "Combined Predicted Data with Original Data")
points(preds1$pred, col='red', type='l')
abline(v=length(Data2))

NimaMalboubiPredictions1 = preds1$pred


#############################################################
#Data Trend 2
Data2 = read.csv ("C:\\Users\\Owner\\Desktop\\q2train.csv")
Data2 = ts(Data2)
Data2 = Data2[,2]


plot(Data2, type="l", xlab = "Time", ylab="Data", main = "Google Trends Data 2")

Data2 = log(Data2)

Data2 = Data2 [ 16:431]

diff1 = diff(Data2)
diff2 = diff(diff1, lag = 52)

acf(diff1, lag.max = 200)
pacf(diff1, lag.max = 200)



acf(diff2, lag.max = 200)
pacf(diff2, lag.max = 200)

#MA(2) X MA(1) (52)
lam012011 = timeseries_cv(Data2,c(0,1,2),c(0,1,1))
mean (lam012011, na.rm = TRUE)
# 0.04836047

est1 = arima(Data2,order=c(0,1,2),seasonal = list(order=c(0,1,1),period = 52))$aic
#-1351.315


#MA(1) X MA(1) (52)
lam011011 = timeseries_cv(Data2,c(0,1,1),c(0,1,1))
mean (lam011011, na.rm = TRUE)
#0.04571412

est2 =  arima(Data2,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 52))$aic
#-1341.881


model1 = arima(Data2,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 52))
preds1 = predict(model1,n.ahead=104)
plot(preds1$pred, main = "Prediction for Data 2", ylab = 'Predicted Data')
breakpoint = length(Data2)
combine = ts(c(Data2,preds1$pred))
plot(combine,col='black', ylab = "Data", main = "Combined Predicted Data with Original Data")
points(preds1$pred, col='red', type='l')
abline(v=length(Data2))

NimaMalboubiPredictions2 = exp(preds1$pred) 

##################################################################################
#Data 3
#no constant variance, so I took the log of the data
#Cleaning up the Data
Data3 = read.csv ("C:\\Users\\Owner\\Desktop\\q3train.csv")
Data3 = ts(Data3)
Data3 = Data3[,2]


Data3 = Data3[16:431]
Data3 = log(Data3)
Data3 = ts(Data3)

plot(Data3)
Diff1 = diff(Data3, lag=1)
plot(Diff1)
Diff2 = diff(Diff1, lag= 52)

acf(Diff1, lag.max = 200)
pacf (Diff1, lag.max = 200)

acf(Diff2, lag.max=200)
pacf(Diff2, lag.max = 200)


plot(Diff1)
plot(Diff2)
    


#Model 1: MA1 with MA1 with seasonality


# First 16 weeks didnt look like the same pattern
#Data3 = ts(Data3[16:length(Data3)])

#MA 1 X MA1 (52)
mod011011 = timeseries_cv(Data3,c(0,1,1),c(0,1,1))

mean(mod011011, na.rm=TRUE)
#0.08488187


#MA1 X  MA2 (52)
mod011012 = timeseries_cv(Data3,c(0,1,1),c(0,1,2))
mean (mod011012, na.rm = TRUE)
#0.08437546


###############
#MA2 X MA1 (52)
mod012011 = timeseries_cv(Data3,c(0,1,2),c(0,1,1))
mean(mod012011, na.rm = TRUE) 
#0.08511763


########
#MA1 X AR1 (52)
mod011110 = timeseries_cv(Data3,c(0,1,1),c(1,1,0))
mean (mod011110, na.rm = TRUE) 
#0.08385455

#MA1 X AR2 (52)
mod012210 = timeseries_cv(Data3,c(0,1,1),c(2,1,0))
mean (mod012210, na.rm = TRUE)
#0.08496781


#MA2 X AR(1) (52)
mod012110 = timeseries_cv(Data3, c (0,1,2), c(1,1,0))
mean (mod012110, na.rm = TRUE)
# 0.08398149


#acf, pacf, a plot of actual data, plot of logged data, plot of twice differenced data, qqplot to compare for normality, and a plot of predictions

test1 = arima(Data3,order=c(0,1,2),seasonal = list(order=c(1,1,0),period = 52))$aic
test2 = arima(Data3,order=c(0,1,1),seasonal = list(order=c(0,1,2),period = 52))$aic

model1 = arima(Data3,order=c(0,1,1),seasonal = list(order=c(1,1,0),period = 52))

preds = predict(model1,n.ahead=104)
plot(preds$pred)
breakpoint = length(Data3)
combine = ts(c(Data3,preds$pred))
plot(combine , col='black')
points (preds$pred, col ='red', type = 'l')
abline (v = length (Data5))

preds1 = exp(preds$pred)

NimaMalboubiPredictions3 = preds1



#preds = exp(preds) remember to return to original values
#test out one more model


################################################################
#Data 4
Data4 = read.csv ("C:\\Users\\Owner\\Desktop\\q4train.csv")
ts(Data4)
Data4 = Data4[,2]


Data4 = ts(Data4)
plot(Data4)
diff1 = diff(Data4)
plot(diff1)

#Seasonality
diff2 = diff(diff1, lag=52)

plot(diff2)
     
acf(diff2, lag.max=200)
pacf(diff2, lag.max = 200)
     
#MA1 MA52
#MA1 AR52 (#try seasonal AR(1) and season AR2)


length(Data4)
Data4 = Data4[16:431]
Data4 = ts(Data4)
plot(Data4)


#MA1 MA52
sim011011 = timeseries_cv(Data4,c(0,1,1),c(0,1,1))
sim011011
#NA 4.507542 3.614420 4.500713 3.389014 2.531856 4.135798 
mean (c(4.507542, 3.614420, 4.500713, 3.389014, 2.531856, 4.135798))
#3.779891

#MA1 AR1 (52)
arima(Data_4[1:104], order = c(0,1,1), seasonal = list(order=c(1,1,0), period = 52), method = "ML")
sim011101 = timeseries_cv(Data4, c(0,1,1), c(1,1,0))
sim011101

#NA 6.902317 4.203004 3.755055 4.650984 3.382135 4.706116
mean (c(6.902317, 4.203004, 3.755055, 4.650984, 3.382135, 4.706116))
#4.599935

#MA1 AR2(52)
sim011102 = timeseries_cv(Data4, c(0,1,1), c(2,1,0))
sim011102
#NA 6.896309 4.382333 3.957190 4.212345 3.437361 5.264313
mean (c(6.896309, 4.382333, 3.957190, 4.212345, 3.437361, 5.264313))
#4.691642


#function that tries to find arima model that best fits the data





#MA2 vs MA52
sim012011 = timeseries_cv(Data4, c(0,1,2), c(0,1,1))
sim012011
#NA 4.709601 3.306289 6.020375 2.697358 2.614241 3.161679
mean(c(4.709601, 3.306289, 6.020375, 2.697358, 2.614241, 3.161679))
#3.75159


#MA2 vs AR1
sim012101 = timeseries_cv(Data4, c(0,1,2), c(1,1,0))
sim012101
#NA 7.378964 4.074700 3.873457 4.216953 3.217546 4.242146
mean (c(7.378964, 4.074700, 3.873457, 4.216953, 3.217546, 4.242146))
# 4.500628



#MA2 vs AR2
sim012210 = timeseries_cv(Data4, c(0,1,2), c(2,1,0))
sim012210
#NA 5.466086 5.459263 6.055355 2.767598 2.594411 3.255259
mean(c(5.466086, 5.459263, 6.055355, 2.767598, 2.594411, 3.255259))
#4.266329

#MA3 vs MA 52
sim013011 = timeseries_cv(Data4, c(0,1,3), c(0,1,1))
sim013011
#NA 4.812432 3.292961 6.474779 2.712052 2.718280 3.043931
mean(sim013011, na.rm = TRUE)
#3.842406




model2 = arima(Data4,order=c(0,1,2),seasonal = list(order=c(0,1,1),period = 52))
preds2 = predict(model2,n.ahead=104)
plot(preds2$pred)
breakpoint = length(Data4)
combine = ts(c(Data4,preds2$pred))
plot(combine,col='black')
points(preds2$pred, col='red', type='l')
abline(v=length(Data4))

NimaMalboubiPredictions4 = preds2$pred

################################################################
#Data5 
Data5 = read.csv ("C:\\Users\\Owner\\Desktop\\q5train.csv")
Data5 = ts(Data5)
Data5 = Data5[,2]


plot(Data5, type= "l",xlab = "Time", ylab="Data", main = "Google Trends Data 5")
abline(v=16, col = 'red')
points(Data5[1:15], col='red', type ='l')
Data5 = Data5[16:431]
Data5 = ts(Data5)
plot(Data5, type= "l",xlab = "Time", ylab="Data", main = "Google Trends Data 5")


#normalizing the variance by taking the log
Data5 = log(Data5)
plot(Data5, type = 'l', xlab = "Time", ylab = "Data", main = "Log Transformation of Google Trends Data5")

#Differencing the Trend
diff1 = diff(Data5)

#ACF and PACF after differencing data for trend
acf(diff1, lag.max=200, main = 'ACF of Differenced Data')
pacf(diff1, lag.max=200, main = "PACF of Differenced Data")
#Seasonality is Present. 

#Graph of first differenced Data
plot(ts(diff1), xlab = "Time", ylab = "Data", main = "Data After First Order Differencing")

#Differencing for Seasonality
diff2 = diff(diff1, lag = 52)

#Graph of 2nd differenced Data
plot(diff2, type ='l', xlab = "Time", ylab = "Data", main = "Data After 2nd Order Differencing")

#ACF and PACF of 2nd differenced Data
acf(diff2, lag.max = 200, main = 'ACF of 2nd Order Differenced Data')
pacf(diff2, lag.max = 200, main = 'PACF of 2nd Order Differenced Data')




#looks like an MA1 mixed with MA1 (52)
test1 = arima(Data5,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 52))$aic
#-1326.744
mud011011 = timeseries_cv(Data5,c(0,1,1),c(0,1,1))
mean (mud011011, na.rm =TRUE)
#0.05938427


#MA(1) X AR(1) (52) because the PACF values are mainly insignificant after the 5th lag.
mud011110 = timeseries_cv(Data5,c(0,1,1),c(1,1,0))
mean (mud011110, na.rm = TRUE)
#0.06046347
test2 = arima(Data5,order=c(0,1,1),seasonal = list(order=c(1,1,0),period = 52))$aic
#1329.952

#AR(4) X MA(1) (52)
mud410011 = timeseries_cv(Data5,c(0,1,4),c(0,1,1))
mean(mud410011, na.rm =TRUE)
#0.06818945
test3 = arima(Data5,order=c(4,1,0),seasonal = list(order=c(0,1,1),period = 52))$aic
#-1310.106





model3 = arima(Data5,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 52))
preds3 = predict(model3,n.ahead=104)
plot(preds3$pred, main = "Prediction for Data 5", ylab = 'Predicted Data')
breakpoint = length(Data5)
combine = ts(c(Data5,preds3$pred))
plot(combine,col='black', ylab = "Data", main = "Combined Predicted Data with Original Data")
points(preds3$pred, col='red', type='l')
abline(v=length(Data5))

NimaMalboubiPredictions5 = exp(preds3$pred)



#check once with log, and once without log

#########################################################################################
#R Check
#double letters (aa) represent data that is not log transformed
#single letters (a) represent data that is log transformed
Data5 = read.csv ("C:\\Users\\Owner\\Desktop\\q5train.csv")
Data5 = ts(Data5)
Data5 = Data5[,2]

Data5 = Data5 [16:431]
#1732.409

Data5 = log(Data5)
a = timeseries_cv1 (Data5, c(0,1,1), c(0,1,1))
mean (a, na.rm = TRUE)
#3.703127

aa = timeseries_cv(Data5, c(0,1,1), c(0,1,1))
mean(aa, na.rm=TRUE)
#3.756551


b = timeseries_cv1(Data5, c(0,1,1), c (1,1,0))
mean(b, na.rm = TRUE)
#3.774563

bb = timeseries_cv(Data5, c (0,1,1), c(1,1,0))
mean (bb, na.rm = TRUE)
#3.836105

c = timeseries_cv1(Data5, c(4,1,0), c(0,1,1))
mean(c, na.rm = TRUE)
#3.627442

cc = timeseries_cv(Data5, c(4,1,0), c(0,1,1))
mean(cc, na.rm = TRUE)
#3.77693


timeseries_cv1 = function(data,order1,order2){
  cv_score = vector()
  num_years = length(Data3) %/% 52
  for(ii in 2:(num_years-1)){
    train_idx = 1:c(52*ii)
    test_idx = (52*ii+1):(52*(ii+1))
    print(ii*52)
    model = arima(data[train_idx],order = order1, seasonal = list(order=order2,period=52), method ="ML")
    print('testing')
    preds = predict(model,n.ahead = 52)
    predics1 = exp(preds$pred)
    print(preds$pred)
    cv_score[ii] = sqrt(mean((predics1 - exp(data)[test_idx])^2))
  }
  return(cv_score)
}


