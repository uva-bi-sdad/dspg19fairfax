install.packages("osmdata")
library(osmdata)
library(ggmap)

# References:
# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# https://wiki.openstreetmap.org/wiki/Map_Features

highway<-opq(bbox = 'fairfax county')%>%
  add_osm_feature(key = 'boundary', value = 'administrative')%>%
  osmdata_sp ()
sp::plot(highway$osm_lines)


hospital<-opq(bbox = 'fairfax county')%>%
  add_osm_feature(key = 'amenity', value = 'hospital')%>%
  osmdata_sp ()
sp::plot(hospital$osm_points,add=TRUE,pch=18,col="red")
