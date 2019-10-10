
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(RSocrata)
library(httr)
library(jsonlite)

url <- 'https://data.cityofnewyork.us/resource/uip8-fykc.csv'
url_week <- "https://data.cityofnewyork.us/resource/8h9b-rp9u.csv$where=arrest_date between '2019-01-01T12:00:00' and '2019-01-07T14:00:00"
arrests <- read.socrata(url, app_token = Sys.getenv('token'))
