library(data.table)
library(dplyr)

dataset <- fread("owid-covid-data-USA.csv")
columns <- c("date","total_deaths","new_deaths","total_cases","new_cases")
data <- select(dataset, colnames<-columns)
