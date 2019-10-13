
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(RSocrata)
library(httr)
library(jsonlite)
library(sp)
library(data.table)

# Loading in the data
url <- 'https://data.cityofnewyork.us/resource/uip8-fykc.csv'
url_week <- URLencode(paste0(url, '?$where=arrest_date%20between%20%27',input$start,'T12:00:00%27%20and%20%27',input$end,'T14:00:00%27'))


#All datasets brought to you by NYCOpenData ---------------------------------------------

arrests <- read.socrata(url, app_token = Sys.getenv('token'))

#neighborhood tabulation areas
n_tabs <- readOGR("Neighborhood Tabulation Areas.geojson")

header <- dashboardHeader(title = "NYPD Arrests July 2018 - June 2019",
                          titleWidth = 400)

sidebar <- dashboardSidebar(
  
  #A very nice menu of the different tabs ------------------------------------
  sidebarMenu(
    id = "tabs",
    
    menuItem("Raw Data", icon = icon("table"), tabName = "table"),
    
    
  selectInput("boro",
              "Borough",
              choices = c("Brooklyn" = "K",
                "Queens" = "Q",
                "Bronx" = "B",
                "Staten Island" = "S",
                "Manhattan" = "M"))
))


body <- dashboardBody(
  tabItem("table",
          fluidRow(box(title = h3("The Data"), DT::dataTableOutput("table")))
          )
)
# Putting everything together
ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(input,output){
  
  # Creating a subset based on boroughs ----------------------------------------
  data_sub <- reactive({
    subset(arrests, arrest_boro==input$boro)
  })
  
  # Creating a data table based on subset
  
  output$table <- DT::renderDataTable({
    data.table(data_sub())
  })
  
  
# base <-leaflet() %>%
#   setView(lat = 40.75, lng = -74, zoom = 11.3) 
# 
# base <- base %>%
#   addProviderTiles(providers$Stamen.TonerBackground)
# 
# 
# leaflet(data = n_tabs) %>%
#   setView(lat = 40.75, lng = -73.9, zoom = 11.3) %>%
#   addProviderTiles(providers$Stamen.TonerBackground) %>%
#   addPolygons(color = '#2ca25f', weight = 2)


#point.in.polygon
}

# Running the application
shinyApp(ui, server)