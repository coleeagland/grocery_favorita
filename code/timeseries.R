#time series
#Looking by item.store now
head(train.small)
train.small <- train.small %>% arrange(year, week)
train.small

newdat <- data.frame(year = c(rep(2014,time=length(13:52)),
                              rep(2015, times = 52),
                              rep(2016, times = 52),
                              rep(2017, times = length(1:33))),
                     week = c(13:52,rep(1:52,times=2),1:33))

newdat <- data.frame(date = c(time=seq(min(train.small$date),max(train.small$date),by="days")))

newdat <- left_join(newdat,storeholidays %>% filter(store.nbr==1))

newdat <- left_join(newdat, train.small)


newdat <- left_join(newdat,train.small[,c("year","week","unit.sales","onpromotion","holiday","earthquake.marker","oil.4wprior")])
newdat$unit.sales <- sjmisc::replace_na(newdat$unit.sales, value = 0)
newdat

gam.ts <- gam(unit.sales ~
                #                s(week, k = 15)
                #              + onpromotion
                #              + holiday
                #              + earthquake.marker
                #              + s(oil.4wprior, k = 25),
                ,              data = newdat,
              family = gaussian)
summary(gam.ts)
plot(gam.ts)
grocery.ts <- ts(newdat$unit.sales, frequency = 52, start = c(2014,13),
                 end = c(2017,33))
grocery.ts
bestfit <- list(aicc=Inf)
for (i in 1:25)
{
  fit <- auto.arima(grocery.ts, xreg=fourier(grocery.ts, K=i), 
                    seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
summary(bestfit)



plot(fc)
ts.plot(grocery.ts)
ts.plot(diff(log(grocery.ts)))

grocery.ts

start(grocery.ts)
end(grocery.ts)
frequency(grocery.ts)
time(grocery.ts)
deltat(grocery.ts)
cycle(grocery.ts)
autoplot(grocery.ts)
ggseasonplot(grocery.ts) #try with polar=TRUE argument
ggsubseriesplot(grocery.ts) #mini time plots for each season (mean blue line)
ggAcf(grocery.ts) #forecast package
gglagplot(grocery.ts) #ggfortify or forecast package
Box.test(diff(goog), lag = 10, type = "Ljung") #Does the data behave like white noise? Ha=Data exhibits serial correlation
window(train.ts, end=2016)

library(forecast)
end(train.ts)

##Some actual forecasting
naive() #use previous period forecast
snaive() #use previous matching season to forecast
  #Use autoplat to look at results of forecast objects

#Essential assumptions:
#1 Residuals should be uncorrelated
#2 Residuals should have mean zero
#Nonessential:
#3 Residuals should have constant variance
#4 They should be normally distributed (not like white noise, though the first 3 are satisfied by white noise data)

checkresiduals(fc obj) #timeplot acf histogram ljung-boxtest
accuracy(fc,test) #Gives many different types of error stats

grocery.fcst <- HoltWinters(grocery.ts, beta = FALSE, gamma = FALSE)
plot(grocery.fcst)

grocery.fcst

grocery.ts[20:30] %>% mean()
grocery.ts[124:134] %>% mean()

plot(forecast::forecast(grocery.fcst,h=8))
ets(grocery.ts, model = "AAA")
grocery.ts.comp <- decompose(grocery.ts)
plot(grocery.ts.comp)
#For some reason there's a big spike in 2016, and a few dead spots. This is at a single store


####DATACAMP

#generate white noise data
save <- arima.sim(list(order = c(0,0,0)), n=50, mean=4, sd=2)
plot(save)

#fit white noise model
arima(save, order = c(0,0,0))
0.2645*7

mean(save)
var(save)
