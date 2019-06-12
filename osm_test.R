#library(osmdata)
library(rgdal)
#library(sf)
library(tidycensus)
library(ggplot2)


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

# OSM hospital data
hospital<-opq('Fairfax County')%>%
  add_osm_feature(key='amenity',value=c('hospital','baby_hatch','clinic','dentist','doctors',
                                        'pharmacy','social_facility','veterinary'))%>%
  osmdata_sf()

proj4string(hospital$osm_points$geometry)
