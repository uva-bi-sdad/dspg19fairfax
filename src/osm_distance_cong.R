devtools::install_github("tlorusso/traveltime")

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, purrr, sf, mapview, traveltime)

## Basic plotting
traveltime30 <- traveltime_map(appId="3c54476f",
                               apiKey="3902eeb364e6db623d0b4f11fc25379c",
                               location=c(38.842669,-77.270272),
                               traveltime=1800,
                               type="public_transport",
                               departure="2019-6-26T08:00:00Z")
mapview(traveltime30,  col.regions = c("grey")) + traveltime30


## Try OSM
library(osmdata)
supermarket<-opq('Fairfax County')%>%
  add_osm_feature(key='shop',value=c('supermarket'))%>%
  osmdata_sf()


df<-as.data.frame(st_coordinates(supermarket$osm_points))

df%>%mutate(sf=map2(.x=X,.y=Y,~traveltime_map(appId="3c54476f",
                                              apiKey="3902eeb364e6db623d0b4f11fc25379c",
                                              location=c(.y,.x),
                                              traveltime=1200,
                                              type="public_transport",
                                              departure="2019-6-26T08:00:00Z")))


