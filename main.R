library(data.table)
library(dplyr)
library(plotly)

dataset <- fread("owid-covid-data-USA.csv")
columns <- c("date","total_deaths","new_deaths","total_cases","new_cases")
data2 <- select(dataset, colnames<-columns)

d_col <- as.Date(data2$date, format = "%m/%d/%Y")
day <- as.numeric(format(d_col, "%d"))
month <- as.numeric(format(d_col, "%m"))
year <- as.numeric(format(d_col, "%Y"))

data2 <- cbind(data2, day, month, year)

############################# EDA #########################################
temp = data2 %>%
        select(day, total_deaths, new_deaths, month) %>%
        filter(year==2021)

plot_ly(temp, y = ~total_deaths, x = ~month, mode = 'lines', type = 'scatter')
####################### Missing values plot #################################

new_columns = c("date","total_deaths","new_deaths","total_cases","new_cases", "day","month","year")
sum_na = colSums(is.na(data2))
temp = data.frame(sum_na)
plot_ly(temp, x=new_columns, y=sum_na, type='bar')

###########################################################################
