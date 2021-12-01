library(data.table)
library(dplyr)
library(plotly)
library(zoo)
library(ggplot2)
library(tsbox)
library(ggfortify)

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

ts.plot(new_data_ts, xlab = "Timeline", ylab = "Deaths", color="red")

training_set = window(new_data_ts, start = c(2020, 22), end = c(2021, 302))
validation_set = window(new_data_ts, start = c(2021, 303), end = c(2021, 332))
acf(new_data_ts)

############################## Auto regression model ##############################

AR = arima(training_set, order = c(10,0,0)) # Tune the order.
print(AR)
predict_AR = predict(AR, n.ahead = length(validation_set))
#ts.plot(validation_set, xlab = "Timeline", ylab = "Deaths")
#points(predict_AR$pred, type = "l", col=2, lty=2)
#autoplot(validation_set)
#autoplot(predict_AR$pred, ts.colour = "red")
ts_plot(Validation=validation_set, Forecast=predict_AR$pred, title = "Forecasting on Test Set")

