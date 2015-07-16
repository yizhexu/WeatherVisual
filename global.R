# for api
library(jsonlite)
library(httr)
library(lubridate)
# for shiny
library(shiny)
library(markdown)
library(DT)
library(data.table)
library(xts)
library(dygraphs)
library(leaflet)

# url address
url <- "https://api.awhere.com/v1/weather"

date_range <- function(day_start, day_end) {
  
  day_start <- ymd(day_start, tz = "UCT")
  day_end <- ymd(day_end, tz = "UCT")
  
  loops <- year(day_end) - year(day_start)
  
  lapply(0:loops,function(i){
    c(as.Date(if(i == 0) {day_start} else {day_start + days(364) * (i) + days(1)}), 
      as.Date(if(i == loops) {day_end} else {day_start + days(364) * (i+1)})
    )
  })
  
}

calculate_daily <- function(data, att_number) {
  Reduce(rbind, lapply(1:dim(data)[1], function(i){
    current <- data[i, c(3+(1:att_number))]
    past <- if(dim(data[i-1, c(3+(1:att_number))])[1] <= 0) {0} else {data[i-1, c(3+(1:att_number))]}
    data.frame(date = data[i, 1, drop = TRUE], current - past)
  }))
}


aWhereIcon <- makeIcon(
  iconUrl = "http://www.awhere.com/aWhereSite/media/aWhereLibrary/style/images/icon2.png",
  iconWidth = 26.5, iconHeight = 32
)

load("./data/example.RData")

