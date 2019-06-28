devtools::install_github("tlorusso/traveltime")

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, purrr, sf, mapview, traveltime)

## Test basic plotting
traveltime30 <- traveltime_map(appId="appid",
                               apiKey="appkey",
                               location=c(38.842669, -77.270272),
                               traveltime=1800,
                               type="public_transport",
                               departure="2019-6-26T08:00:00Z")
mapview(traveltime_smt,  col.regions = c("grey")) + traveltime_smt


#===========THIS IS THE PART FOR FETCHING SUPERMARKETS TRAVEL ISOCHRONES==============

## OSM Supermarket
library(osmdata)
library(sf)
library(leaflet)

supermarket<-opq('Fairfax County')%>%
  add_osm_feature(key='shop',value=c('supermarket'))%>%
  osmdata_sf()

fairfax.boundary <- getbb("fairfax county", format_out = "sf_polygon") 

inside<-supermarket$osm_points[fairfax.boundary,]

df<-as.data.frame(st_coordinates(inside$geometry))

traveltime_bus<-data.frame()
traveltime_car<-data.frame()

for (i in 1:nrow(df)){
  bus <- traveltime_map(appId="appid",
                                 apiKey="appkey",
                                 location=c(df$Y[i], df$X[i]),
                                 traveltime=1200,
                                 type="public_transport",
                                 departure="2019-6-27T08:00:00Z")

  traveltime_bus<-rbind.data.frame(traveltime_bus,bus)
  Sys.sleep(6)
}

for (i in 1:nrow(df)){
  car <- traveltime_map(appId="3c54476f",
                        apiKey="049992edda691331bc5bbe90dd7c6952",
                        location=c(df$Y[i], df$X[i]),
                        traveltime=1200,
                        type="driving",
                        departure="2019-6-27T08:00:00Z")
  
  traveltime_car<-rbind.data.frame(traveltime_car,car)
  Sys.sleep(6)
}

mapview(traveltime_smt,  col.regions = c("grey")) + traveltime_smt
#========================================================================


outputs <- leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = traveltime_car,
              fillOpacity = 0.2,weight = 1,color='orange')%>%
  addPolygons(data = traveltime_bus,
              fillOpacity = 0.2,weight = 1,color='blue')

outputs

htmlwidgets::saveWidget(widget=outputs, file="output.html", selfcontained=TRUE, libdir = "js")


