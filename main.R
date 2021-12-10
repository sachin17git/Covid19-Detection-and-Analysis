library(data.table)
library(dplyr)
library(plotly)
library(zoo)
library(ggplot2)
library(tsbox)
library(ggfortify)
library(forecast)
library(tseries)
library(prophet)

dataset <- fread("owid-covid-data-USA.csv")
columns <- c("date","total_deaths","new_deaths","total_cases","new_cases")
data2 <- select(dataset, colnames<-columns)

d_col <- as.Date(data2$date, format = "%m/%d/%Y")
day <- as.numeric(format(d_col, "%d"))
month <- as.numeric(format(d_col, "%m"))
year <- as.numeric(format(d_col, "%Y"))

data2 <- cbind(data2, d_col, day, month, year)

####################### Missing values plot #################################

new_columns = c("date","total_deaths","new_deaths","total_cases","new_cases","d_col", "day","month","year")
sum_na = colSums(is.na(data2))
temp = data.frame(sum_na)
#row.names(temp) = new_columns

temp %>%
  plot_ly(x=new_columns,y=~sum_na, type='bar') %>%
  layout(title = 'Missing values count')

######################## Fill missing cells ########################################

data2$total_deaths = ifelse(is.na(data2$total_deaths),
                            0, data2$total_deaths)
data2$new_deaths = ifelse(is.na(data2$new_deaths),
                            0, data2$new_deaths)
data2$new_cases = ifelse(is.na(data2$new_cases),
                            0, data2$new_cases)

################ Applying rolling mean for new deaths and cases #######################
data2 <- data2 %>%
  dplyr::mutate(new_death_roll12 = zoo::rollmean(new_deaths,k=12, fill=NA)) %>%
  dplyr::mutate(new_cases_roll12 = zoo::rollmean(new_cases,k=12, fill=NA))

############################# EDA #########################################
temp = data2 %>%
        select(new_death_roll12, new_cases_roll12)

row.names(temp) <- data2$d_col
n_days = 1:dim(temp)[1]
deathsRoll12=temp$new_death_roll12
casesRoll12 = temp$new_cases_roll12
rownames(data2) = data2$d_col
#plot_ly(temp,x=~n_days,y=~deathsRoll12, mode='lines', type='scatter', text= rownames(temp))
data2 %>%
  ggplot(aes(x=d_col,y=deathsRoll12, color='smoker')) +
  geom_line() +
  ggtitle("                   Rolling mean plot - New Deaths") +
  xlab("Days Jan 2020 - Nov 2021") +
  ylab("New Deaths")

data2 %>%
  ggplot(aes(x=d_col, y=casesRoll12, color = 'smoker')) +
  geom_line() +
  ggtitle("                   Rolling mean plot - New Cases") +
  xlab("Days Jan 2020 - Nov 2021") +
  ylab("New Cases")

######################### Prepare data for forecasting new deaths #####################
new_data <- data2 %>%
  select(new_deaths)

row.names(new_data) = data2$d_col
inds = seq(as.Date("2020-01-22"), as.Date("2021-11-28"), by="day")

set.seed(123)
new_data_ts = ts(new_data, frequency = 365,
                 start = c(2020, as.numeric(format(inds[1],"%j"))),
                 end = c(2021, as.numeric(format(inds[677],"%j"))))


ts_ggplot(new_data_ts, xlab = "Timeline", ylab = "Deaths")


################################# Making plot stationary ###########################

nsdiffs(new_data_ts)
data_diff <- diff(new_data_ts, lag=frequency(new_data_ts), differences=1)
plot(data_diff, type="l", main="Seasonally Differenced")

ndiffs(data_diff)
stationaryTS <- diff(data_diff, differences= 1)
plot(stationaryTS, type="l", main="Differenced and Stationary")

################################# Training and testing set #############################

training_set = window(new_data_ts, start = c(2020, 22), end = c(2021, 302))
#train_set_transf = BoxCox(training_set, lambda = BoxCox.lambda(training_set))
validation_set = window(new_data_ts, start = c(2021, 302), end = c(2021, 332))
#validation_set_transf = BoxCox(validation_set, lambda = BoxCox.lambda(validation_set))

acf(new_data_ts)
kpss.test(new_data_ts)
adf.test(training_set)


############################## Arima model #####################################

AR = arima(training_set, order = c(5,1,7)) # Tune the order.
print(AR)
predict_AR = predict(AR, n.ahead = length(validation_set))

library(Metrics)
print(paste("MAE:",mae(validation_set, predict_AR$pred)))
print(paste("MSE:",mse(validation_set, predict_AR$pred)))
print(paste("RMSE:",rmse(validation_set, predict_AR$pred)))





# Plot - Training set with forecast.
ts_ggplot(Training=ts(training_set[1:646], frequency = 365,
                    start = c(2020, as.numeric(format(inds[1],"%j"))),
                    end = c(2021, as.numeric(format(inds[646],"%j")))),
          Forecast=predict_AR$pred,
          title = "                                                          Time Series Forecasting - ARIMA",
          ylab = "Deaths")

# Closer look at forecast (validation vs forecast)
ts_ggplot(Validation=validation_set,
          Forecast=predict_AR$pred,
          title = "                                                    Time Series Forecasting - November - Validation",
          ylab = "Deaths")



############################## Auto-Arima Model #######################################

arima_1 = auto.arima(training_set)
print(arima_1)
forecast_1 = forecast(arima_1, h=30)
acc_arima_1 = accuracy(f = forecast_1,x = validation_set)
summary(acc_arima_1)
autoplot(forecast_1,main = 'Forecast Test-Set',xlab = 'Deaths',ylab = 'Timeline')

########################### Facebook Prophet model ##################################

fb_frame = data2 %>%
  select(d_col, new_deaths)

fb_frame = rename(fb_frame, c(ds=d_col, y=new_deaths))

fb_train = fb_frame[1:647]
fb_test = fb_frame[647:677]

m <- prophet(fb_train, seasonality.prior.scale = 0.1,
             yearly.seasonality=TRUE,
             changepoint.prior.scale = 0.01)

future = make_future_dataframe(m, periods = 30)
fb_forecast = predict(m, future)
fb_forecast1 = fb_forecast[c('ds','yhat')]#,'yhat_lower','yhat_upper')]

plot(m, fb_forecast1, xlab = "Timeline",ylab = "Deaths")

# Plot forecast and test set.
ts_ggplot(Forecast=ts(fb_forecast1$yhat[647:677]), Testset=ts(fb_test$y))
prophet_plot_components(m, fb_forecast)

print(paste("MAE:",mae(fb_test$y, fb_forecast1$yhat[647:677])))
print(paste("MSE:",mse(fb_test$y, fb_forecast1$yhat[647:677])))
print(paste("RMSE:",rmse(fb_test$y, fb_forecast1$yhat[647:677])))
