
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(rgeos)
library(RSocrata)
library(sp)
library(data.table)
library(shinyWidgets)
library(plotly)


# Geographical datasets brought to you by NYC OpenData ------------------------------------

#neighborhood tabulation areas
n_tabs <- readOGR("Neighborhood Tabulation Areas.geojson")
nta_df <- data.frame(n_tabs$ntacode)



header <- dashboardHeader(title = "NYPD Arrests for 2019",
                          titleWidth = 300)

sidebar <- dashboardSidebar(
  
  #A very nice menu of the different tabs ------------------------------------
  sidebarMenu(
    id = "tabs",
    menuItem("The Map", icon = icon("map"), tabName = "mep"),
    menuItem("Raw Data", icon = icon("table"), tabName = "table"),
    menuItem("Charts and Graphs", icon = icon("chart-bar"), tabName = "charts"),
    
    dateRangeInput("dates",
                   "Select Dates",
                   start = '2019-06-01',
                   end = '2019-06-30'),
    
  pickerInput("boro",
              "Borough",
              choices = c("Brooklyn" = "K",
                "Queens" = "Q",
                "Bronx" = "B",
                "Staten Island" = "S",
                "Manhattan" = "M"),
              selected = "M",
              options = list(`actions-box` = TRUE),
              multiple = T),
  radioButtons("offense",
               "Type of Offense",
               choices = c("Drugs" = "DANGEROUS DRUGS",
                           "Assault" = "ASSAULT 3 & RELATED OFFENSES",
                            "Larceny" = "PETIT LARCENY",
                           "Traffic Violations" = "VEHICLE AND TRAFFIC LAWS",
                           "Weapons Charge" = "DANGEROUS WEAPONS",
                           "Robbery" = "ROBBERY"))
))


body <- dashboardBody(tabItems(
  tabItem("mep",
          fluidPage(box(title = h3("Map of Arrests"), leafletOutput("map", 
                                                                    width = "200%",
                                                                    height = 500)))),
  tabItem("table",
          fluidPage(box(title = h3("The Data"), DT::dataTableOutput("table")))
          ),
  tabItem("charts",
          fluidPage(
            fluidRow(plotlyOutput("balloon")),
            fluidRow(plotlyOutput("graph"))
          ))

))
# Putting everything together
ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(input,output){
  
  # Creating the API with data filters--------------------------------------------------
  url <- 'https://data.cityofnewyork.us/resource/uip8-fykc.csv'
  
  arrests_url <- reactive({
    paste0(url, "?$where=arrest_date%20between%20%27",
                   input$dates[1],"T12:00:00%27%20and%20%27",
                   input$dates[2],"T14:00:00%27")
  })
  # Loading the data filtered by date---------------------------------------------
  
    arrest_data <- reactive({
      read.socrata(arrests_url(), app_token = Sys.getenv('token'))
    
  })
  
  # Creating a subset based on boroughs ----------------------------------------
  data_sub <- reactive({
    arrests <- arrest_data()
    req(input$boro)
    req(input$offense)
    d <- subset(arrests, arrest_boro==input$boro)
    subset(d, ofns_desc == input$offense)
    
  })
  
  # Creating a data table based on subset -----------------------------------
  
  output$table <- DT::renderDataTable({
    data.table(data_sub())
  })
  
  # Creating the base map in Leaflet ----------------------------------------
  output$map <- renderLeaflet({

  leaflet() %>%
    setView(lat = 40.78, lng = -73.95, zoom = 11.3) %>%
    addProviderTiles(providers$Esri.WorldStreetMap) 
  })
  
  # Adding arrests by neighorhood tabulation area in Leaflet ---------------------------
  observe({
    data_sub <- data_sub()
  
  # Making x,y coordinates from arrests into spatial points ---------------- 
    xy <- data_sub[,c("longitude", "latitude")]
  
    spdf <- SpatialPointsDataFrame(coords = xy, data = data_sub,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  # Assigning arrests to neighborhood tabulation areas based on location --------------
    idk <- over(n_tabs, spdf, returnList = TRUE)
  
    idk_new <- list()
    for (i in 1:195) {
      idk_new[i] <- nrow(idk[[i]])
  }
  
  # counting the number of arrests in each NTA -----------------------------------
    arrests_nta <- data.frame(matrix(unlist(idk_new), nrow=length(idk_new), byrow=T))
    names(arrests_nta) <- c("counts")
    row.names(arrests_nta) <- n_tabs$ntacode
  
  # Combining the arrest counts with the spatial dataset for NTAs ----------------
    merge_things2 <- merge(x = n_tabs, y = arrests_nta, 
                         by.x = "ntacode", by.y = 0)
  
  
  # Mapping arrest counts by NTA ----------------------------------------------------
    bin <- nrow(data_sub)/195
    bins <- c(0, bin, bin*2, bin*3, bin*4, bin*5, Inf)
    pal <- colorBin("OrRd", domain = merge_things2$counts, bins = bins)
  
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = merge_things2, color = '#7d2218', fillColor = ~pal(counts), 
                 fillOpacity = 1, weight = 2, group = "NTA") %>%
      addMarkers(lng = data_sub$longitude, lat = data_sub$latitude,
                 group = "arrests", clusterOptions = markerClusterOptions()) %>%
      clearControls() %>%
      addLegend(pal = pal, values = merge_things2$counts,
              opacity = 1.0, title = 'Arrest Totals',
              position = "bottomleft") %>%
      addLayersControl(overlayGroups = c("NTA", "arrests"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("arrests")
  })
  
   output$balloon <- renderPlotly({
     
    
      # ggplotly(ggplot(data_sub(), aes(x = age_group, y = perp_race)) +
      #          geom_point(stat = "sum") +
      # theme(panel.background=element_blank(),
      #       panel.border =element_rect(color = "blue",
      #                                  fill=NA, size=2)) +
      # ggtitle("Matching Perpetrator Race and Age Group") +
      # xlab("Age Groups") +
      # ylab("Perpetrator Race")
      # )
     
     plot_ly(
               x = data_sub()$age_group, y = data_sub()$perp_race, type = 'scatter',
               mode = 'markers')
   })
   
     output$graph <- renderPlotly({
       ofns <- data_sub()
       df <- dplyr::count(ofns, arrest_date)
       plot_ly(x = df$arrest_date, y = df$n,
               type = 'scatter',
               mode = 'lines') 
                         
     })


     
  

}   

# Running the application
shinyApp(ui, server)