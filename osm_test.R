library(osmdata)
library(rgdal)
library(sf)
library(sp)
library(tidycensus)
library(ggplot2)
library(dplyr)


# Get Median Household Income from ACS and plot it
census_api_key("7a7317f23e9ade546eb19fc64649727eccc17b24",overwrite=TRUE,install = TRUE)
readRenviron("~/.Renviron")

medincome<-get_acs(geography = "tract", county = "Fairfax County",
                   variables = "B19013_001", 
                   state='VA', geometry = TRUE)
#head(medincome)
medincome %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") 

# OSM healthcare data
hospital<-opq('Fairfax County')%>%
  add_osm_feature(key='amenity',value=c('hospital','baby_hatch','clinic','dentist','doctors',
                                        'pharmacy','social_facility','veterinary'))%>%
  osmdata_sp()

# Specify the projection
wgs84<-proj4string(hospital$osm_points)


# Download Census tract shapefile 
library(tigris)
lookup_code("Virginia", "Fairfax County")
tracts.nad83<-tracts(state = '51', county = c('059'))

# Change it to the osm projection
tracts<-spTransform(tracts.nad83, wgs84)

#plot(tracts)
#plot(hospital$osm_points,add=TRUE,col="red",pch=19)

osm_output<-data.frame()
for (i in 1:2) {
  poly <- tracts$GEOID10[i]
  cell <- hospital$osm_points[tracts[i, ], ] 
  if(dim(cell)[1]>0){
    tol_health<-dim(cell)[1]
  }else{tol_health<-0.0}

record<-cbind.data.frame(poly, tol_health)
colnames(record)<-c("GEOID10","TotalHealth")
osm_output<-rbind(osm.output, record)
} 
  

the.cell <- osm.resid.pts[blocks[i, ], ] 
if (dim(the.cell)[1] > 0) {
acreage <- sum(the.cell@data$ACRES)
totbldgs <- dim(the.cell)[1]
}
else {
acreage <- 0.0
totbldgs <- 0.0
}
this.one <- cbind.data.frame(the.poly, totbldgs, acreage)
colnames(this.one) <- c("GEOID10", "TotalBldgs", "BldgAcres")
osm.output <- rbind(osm.output, this.one)
}