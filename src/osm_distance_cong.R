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
supermarket<-opq('Fairfax County')%>%
  add_osm_feature(key='shop',value=c('supermarket'))%>%
  osmdata_sf()

df<-as.data.frame(st_coordinates(supermarket$osm_points))

traveltime_smt<-data.frame()

for (i in 1:nrow(df)){
  this.one <- traveltime_map(appId="appid",
                                 apiKey="appkey",
                                 location=c(df$Y[i], df$X[i]),
                                 traveltime=1200,
                                 type="public_transport",
                                 departure="2019-6-26T08:00:00Z")
  traveltime_smt<-rbind.data.frame(traveltime_smt,this.one)
  Sys.sleep(5)
}

#========================================================================


