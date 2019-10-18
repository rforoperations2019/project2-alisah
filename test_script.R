library(sp)
arrests <- read.csv('uip8-fykc.csv')
xy <- arrests[,c("longitude", "latitude")]

spdf <- SpatialPointsDataFrame(coords = xy, data = arrests,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))




idk <- over(n_tabs, spdf, returnList = TRUE)

idk_new <- list()
for (i in 1:195) {
  idk_new[i] <- nrow(idk[[i]])
}

nta_df <- data.frame(n_tabs$ntacode)
arrests_nta <- data.frame(matrix(unlist(idk_new), nrow=length(idk_new), byrow=T))
names(arrests_nta) <- c("counts")
row.names(arrests_nta) <- n_tabs$ntacode


merge_things2 <- merge(x = n_tabs, y = arrests_nta, 
                      by.x = "ntacode", by.y = 0)

bins <- c(0, 2, 4, 8, 10, 20, Inf)
pal <- colorBin("OrRd", domain = merge_things2$counts, bins = bins)

base <-leaflet() %>%
  setView(lat = 40.78, lng = -73.95, zoom = 11.3)

base <- base %>%
  addProviderTiles(providers$Stamen.TonerBackground)

base %>%
  addPolygons(data = merge_things2, color = '#7d2218', fillColor = ~pal(counts), 
              fillOpacity = 1, weight = 2) %>%
  addLegend(pal = pal, values = merge_things2$counts,
            opacity = 1.0, title = 'Arrest Totals',
            position = "bottomleft")
