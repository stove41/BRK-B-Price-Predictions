library(fpp2)
library(forecast)
library(ggplot2)
library(ggfortify)
library(fGarch)
library(Metrics)
library(quantmod)

getwd()
setwd("~/Desktop")
data=read.csv("BRK-B.csv")
data$DATE <- format(data$DATE, "%Y-%m-%d") # change to Date variable
series=ts(data[,6], start = c(2007, 1), freq = 252)
autoplot(series)

#partition training data and validation data
train=window(series, end=c(2019, 252), freq = 252)
valid=window(series, start= c(2020, 1), freq = 252)

#multiple seasonal model
msts=msts(series,seasonal.periods = c(7,252))
msts_train=msts(train, seasonal.periods=c(7,252))
msts_valid=msts(valid, seasonal.periods=c(7,252))

#create tbats model
tbats_m1=tbats(msts_train)
#check components of model
components = tbats.components(tbats_m1)
plot(components)
plot(msts, col = 'red')
lines(tbats$fitted.values, col = 'blue')
tbats_preds=forecast(tbats_m1, h = 57)
lines(tbats_preds$mean, col = 'green')
accuracy(tbats_preds$mean, msts_valid)
#RMSE 24.56

#create AR and ARIMA models
ar1=arima(train, order=c(1,0,0))
ar1
plot(ar1$residual,type='l')
Box.test(ar1$residual,lag=20,type='Ljung') 

arima1=arima(series, order=c(2,0,0))
arima1
tsdiag(arima1)
plot(arima1$residual,type='l')
Box.test(arima1$residual,lag=20,type='Ljung')

arima2=arima(series, order=c(2,0,2))
arima2
tsdiag(arima2)
plot(arima2$residual,type='l')
Box.test(arima2$residual,lag=20,type='Ljung')

ar_pred=forecast(ar)
summary(ar)
#RMSE 0.50

arima1_pred=forecast(arima1)
summary(arima1)
#RMSE 0.50

arima2_pred=forecast(arima2)
summary(arima2)
#0.49

#Tried auto-arima to see what it gives us. (1,1,1)
auto_arima = auto.arima(train)
summary(auto_arima)
plot(train, col = 'blue', xlim = c(2007, 2021))
lines(valid, col = 'green')
lines(auto_arima$fitted, col = 'red')
auto_preds = forecast(auto_arima, h = 57)
lines(auto_preds$mean, col = 'red')

#I ran this loop with i in each place and looked at the tsdiag outputs for each and it looks like a (4,1,3) model is decent.
for(i in 0:4){
  arima_m = arima(train, order = c(4, 1, i))
  tsdiag(arima_m)
}

arima2 = arima(train, order = c(4,1,3), optim.control = list(maxit = 1000))
arima2
tsdiag(arima2)
arima2_preds = forecast(arima2, h = 57)
accuracy(arima2_preds$mean, valid)
#Test RMSE of 24.69
plot(arima2_preds)


#Make predictions using full time series.

arima_final = arima(series, order = c(4,1,3))
tsdiag(arima_final)
final_preds = forecast(arima_final, h = 30)
plot(final_preds)


#3 month predictions
final_preds90 = forecast(arima_final, h = 90)
plot(final_preds90)




##############################################
#More ARIMA
library(dplyr)
library(forecast)
library(quantmod)
library(sys)
library(fGarch)
library(Metrics)
library(caret)
Sys.setenv(TZ = 'UTC')
Sys.timezone()
setwd('C:/Users/Steve/dev/Grad School/BANA6630/Homework/Final Project/')

getSymbols('BRK-B', from = '2007-01-01', to = '2020-03-31')
head(`BRK-B`)

monthly = to.monthly(`BRK-B`)
monthly = Ad(monthly)
chartSeries(monthly)


#EDA
#Check acf/pacf
acf(monthly)
pacf(monthly)
#Strong AR effect
hist(monthly)
ln_month = log(monthly)
hist(ln_month)

#Try autoarima
aa = auto.arima(ln_month)
summary(aa)
acf(aa$residuals)
pacf(aa$residuals)
Box.test(aa$residuals)
acf(aa$residuals^2)
Box.test(aa$residuals^2)
#slight Garch effect

#Partition data
train = ln_month['2007-01/2019-12']
test = ln_month['2020-01/2020-03']

#Create ARIMA model
m1 = arima(train, order = c(3,1,0), seasonal = list(order = c(2,0,0)))
summary(m1)
m1_preds = forecast(m1, h = 3)
accuracy(m1_preds$mean, test)
plot(m1$residuals)
acf(m1$residuals)
Box.test(m1$residuals, lag = 10, type = 'Ljung')
acf(m1$residuals^2)
pacf(m1$residuals^2)
Box.test(m1$residuals^2, type = 'Ljung')
#Very small GARCH effect.
ar_true_preds = exp(m1_preds$mean)
ar_true_preds
test
p = c(227.3248, 227.5615, 227.1384)
t= c(224.43, 206.34, 183.18)
monthly['2020-01/2020-03']
rmse(t, p)

#Full series predictions
m2 = arima(ln_month, order = c(3,1,0), seasonal = list(order = c(2,0,0)))
summary(m2)
m2_preds = forecast(m2, h = 3)
plot(m2_preds)
t_preds = exp(m2_preds$mean)
t_preds


# Regression
library(data.table)
library(Metrics)
library(zoo)

unemployment <- read.csv(file = "Unemployment.csv", header = TRUE, stringsAsFactors = FALSE)
consumer.sent <- read.csv(file = "UMCSENT.csv", header = TRUE, stringsAsFactors = FALSE)
sahm <- read.csv(file = "SAHMREALTIME.csv", header = TRUE, stringsAsFactors = FALSE)


getSymbols('brk-b')
berkshire <- `BRK-B`[,6]
ep <- endpoints(berkshire, on = 'months')
berk <- period.apply(berkshire, INDEX = ep, FUN = max)
# berk has data through beginning april, need to stop at March to match economic indicators
berk <- berk[1:(nrow(berk)-2),]

dat <- data.frame(berk, unemployment, consumer.sent, sahm)
dat <- dat[,c(1,3,5,7)]
dat$Recip.Unemploy <- 1/dat$Unemployment # reciprocal transformation

cor(dat)
plot(dat)
# BRK looks like a log function of consumer sentiment
# unemplpyment needs log tranformation
names(dat)

fit <- lm(BRK.B.Adjusted ~ Unemployment + UMCSENT + SAHMREALTIME ,data = dat)
summary(fit)
# Sahm rule not signficant, removing increases adj-R^2. 

fit1 <- lm(BRK.B.Adjusted ~ log(Unemployment) + UMCSENT + SAHMREALTIME ,data = dat)
summary(fit1)

fit2 <- lm(BRK.B.Adjusted ~ Unemployment + sqrt(UMCSENT),data = dat)
summary(fit2)

fit3 <- lm(BRK.B.Adjusted ~ Recip.Unemploy + sqrt(UMCSENT),data = dat)
summary(fit3)
# Reciprovcal transformation (1/X2) for unemployment improved model adj-R^2, has lower p-value compared to non-transformed variable.

plot(fit3$residuals)
par(mfrow = c(2,2))
plot(fit3)
# there is a significant pattern in the residuals, model needs improvement. Residuals show significant deviation from a normal distribution - strong skew. 
# all assumptions of linear regression model are violated.
# correlation between economic indicators will be hard to overcome, linear regression is not a good method for this data.

# predicition 
ss <- sample(1:2, size = nrow(dat), replace = T, prob=c(0.8,0.2))
train <- dat[ss==1,]
test <- dat[ss==2,]

m1 <- lm(BRK.B.Adjusted ~ Recip.Unemploy + sqrt(UMCSENT),data = train)
summary(m1)

x_test <- test[,c(3,5)]
y_test <- test[,1]
predictions <- predict(m1, x_test)
rmse(y_test, predictions)
# RMSE 25.71417 for test data set

thirty_day_forecast <- predict(m1, dat[nrow(dat), c(3,5)])
plot(thirty_day_forecast)
rmse(dat[nrow(dat),1], thirty_day_forecast)
# 55.72799 RMSE for April 30th prediction

############################################################
#Random Forest model
library(dplyr)
library(forecast)
library(fpp2)
library(quantmod)
library(caret)
library(tidymodels)
library(sys)
Sys.setenv(TZ = 'UTC')
Sys.timezone()
setwd('C:/Users/Steve/dev/Grad School/BANA6630/Homework/Final Project/')

getSymbols('BRK-B', from = '2007-01-01', to = '2020-04-30')

adj_cl = Ad(`BRK-B`)
chartSeries(adj_cl)
ema20 = EMA(adj_cl, n = 20, a = 0.2)
chartSeries(ema20)
ema20 = ema20 %>% na.omit()
chartSeries(monthly)

#calculate targets for 30 day forecast
diff30 = ema20$EMA - lag(ema20$EMA, 30)
change = ifelse(diff30 >= ema20*.10, 'big_g', ifelse(diff30 > 0, 'gain', ifelse(diff30 <= -(ema20 * .10), 'big_l', 'loss')))
change = change %>% na.omit()
change = as.vector(change)
nas = rep(NA, 30)
change = append(change, nas)

#Calculate targets for 90 day forecast
diff90 = ema20$EMA - lag(ema20$EMA, 90)
change90 = ifelse(diff90 >= ema20*.10, 'big_g', ifelse(diff90 > 0, 'gain', ifelse(diff90 <= -(ema20 * .10), 'big_l', 'loss')))
change90 = change90 %>% na.omit()
change90 = as.vector(change90)
nas90 = rep(NA, 90)
change90 = append(change90, nas90)

#Calculate technical indicators
rsi = RSI(ema20)
macd = MACD(ema20)
roc = ROC(ema20)
wpr = WPR(ema20)
mo = momentum(ema20)


#Create combined XTS object
new = merge(ema20, diff30, diff90, rsi, macd, roc, wpr, mo)
new = new %>% as.data.frame()
new['change'] = change
new['change90'] = change90
new$change = new$change %>% as.factor()
new$change90 = new$change90 %>% as.factor()
new = new %>% rename(diff30 = EMA.1, diff90 = EMA.2, roc = EMA.3, wpr = EMA.4, mo = EMA.5)

#Create 30 day dataset
da30 = new %>% select(-EMA, -diff30, -diff90, -change90)
da30 = da30 %>% na.omit()
#create 90 day dataset
da90 = new %>% select(-EMA, -diff30, -diff90, -change)
da90 = da90 %>% na.omit()

#Create predictors for 30 and 90 day predictions
predictors = new[length(new$EMA), 4:9]

#partition datasets
split30 = da30 %>% initial_split()
train30 = split30 %>% training()
test30 = split30 %>% testing()

split90 = da90 %>% initial_split()
train90 = split90 %>% training()
test90 = split90 %>% testing()

#one month rf model
rf30 = train(change ~ ., data = train30, method = 'ranger')
preds30 = predict(rf30, test30)
confusionMatrix(preds30, test30$change)
#74% accuracy against test set Much lower for big_gain, ~59%

#Make 1-month predictions with RF
rf30_full = train(change ~ ., data = da30, method = 'ranger')
one_month_pred = predict(rf30_full, predictors)
one_month_pred
#RF predicts a gain in 30 days. This means a gain of less than 10% of the price on 4/29/20


#3 month rf model
train90
rf90 = train(change90 ~ ., data = train90, method = 'ranger')
preds90 = predict(rf90, test90)
confusionMatrix(preds90, test90$change)
#67% accuracy against test set. Fairly even across groups

#Make 3 month predictions with RF
rf90_full = train(change90 ~., data = da90, method = 'ranger')
three_month_pred = predict(rf90_full, predictors)
three_month_pred
#predicts loss in 3 months


#################################################################
#Neural net model

monthly_nn = nnetar(train, repeats = 20, p=11, P = 1, size =7)
monthly_nn_preds = forecast(monthly_nn,h=3)
true_nn_preds = exp(monthly_nn_preds$mean)
true_nn_preds
t= c(224.43, 206.34, 183.18)
p = c(213.5837, 219.1553, 208.4970)
rmse(p, t)

#Build model on full dataset
nn_full = nnetar(ln_month, repeats = 20, p = 11, P = 1, size = 7)
full_preds = forecast(nn_full, h = 3)
true_full = exp(full_preds$mean)
true_full



#Calculate test RMSE of weighted average
rmse_weights = c(71.768, 82.462)

test_arima30 = true_preds[1] * rmse_weights[1]
test_arima60 = true_preds[2] * rmse_weights[1]
test_arima90 = true_preds[3] * rmse_weights[1]
test_nn30 = true_nn_preds[1] * rmse_weights[2]
test_nn60 = true_nn_preds[2] * rmse_weights[2]
test_nn90 = true_nn_preds[3] * rmse_weights[2]

test_pred30 = (test_arima30 + test_nn30) / total_weight
test_pred60 = (test_arima60 + test_nn60) / total_weight
test_pred90 = (test_arima90 + test_nn90) / total_weight
test_preds = c(test_pred30, test_pred60, test_pred90)
rmse(test_preds, t)


#calculate weighted average from neural net and arima forecasts

#1-month wieghted pred
arima30 = t_preds[1] * rmse_weights[1]
nn30 = true_full[1] * rmse_weights[2]
#3 month weighted pred
arima90 = t_preds[3] * rmse_weights[1]
nn90 = true_full[3] * rmse_weights[2]
total_weight = sum(rmse_weights)


pred30 = (arima30 + nn30)/total_weight
pred90 = (arima90 + nn90)/total_weight
pred30
pred90
