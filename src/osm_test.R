library(osmdata)
library(rgdal)
library(sf)
library(sp)
library(tidycensus)
library(ggplot2)
library(dplyr)
library(ggthemes)


# Get Median Household Income from ACS and plot it
census_api_key("7a7317f23e9ade546eb19fc64649727eccc17b24",overwrite=TRUE,install = TRUE)
readRenviron("~/.Renviron")

medincome<-get_acs(geography = "tract", county = "Fairfax County",
                   variables = "B19013_001", 
                   state='VA', geometry = TRUE)
#head(medincome)
#save(medincome,file="~/dspg19fairfax/data/original/medincome.RData")

st_transform(medincome,wgs84)



medincome %>%
  ggplot(aes(fill = estimate)) + 
  ggtitle(label = "Median Household Income by Census Tracts")+
  geom_sf(color = NA) + 
  #coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") 

# OSM healthcare data
hospital<-opq('Fairfax County')%>%
  add_osm_feature(key='amenity',value=c('hospital','baby_hatch','clinic','dentist','doctors',
                                        'pharmacy','social_facility','veterinary'))%>%
  osmdata_sp()

wgs84<-proj4string(hospital$osm_points)

library(tigris)
tracts.nad83<-tracts(state = '51', county = c('059'))
#plot(tracts.nad83)
# Change it to the osm projection
tracts<-spTransform(tracts.nad83, wgs84)

osm_output<-data.frame()

for (i in 1:dim(tracts)[1]) {
  poly <- tracts$GEOID[i]
  cell <- hospital$osm_points[tracts[i, ], ]
  if(dim(cell)[1]>0){
    tol_health<-dim(cell)[1]
  }else{tol_health<-0.0}
  record<-cbind.data.frame(poly, tol_health)
  colnames(record)<-c("GEOID10","TotalHealth")
  osm_output<-rbind(osm_output, record)
}

# Join the number of health care facilities back to the shapefile
tracts_health<-merge(tracts, osm_output,  by.x="GEOID", by.y="GEOID10",all.x=TRUE)

colors <- heat.colors(dim(tracts))
colors <- sort(colors, decreasing = TRUE)
plot(tracts, col=colors)

breaks <- c(0,30,60,90,120)
bin <- findInterval(tracts_health$TotalHealth, breaks, all.inside=TRUE)
bin
tracts_health_sf<-st_as_sf(tracts_health)


ggplot(data = tracts_health_sf) +
  geom_polygon(data = tracts_health, aes(x=long, y=lat, group=group),
               fill="white", color="grey50", size=0.25) +
  geom_sf(aes(fill=TotalHealth)) +
  coord_sf(datum = NA) +
  theme_map() +
  theme(legend.position="bottom") +
  #theme(legend.key.width=unit(2, "cm")) +
  ggtitle(label = "Number of Healthcare facilities by Census Tract", subtitle = "Fairfax County, VA") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  #labs(caption = "Note: Polygons in white are states with a single Congressional district.") +
  scale_fill_gradientn(colours = c("grey", "lightblue", "green", "yellow", "red"),
                       limits=c(min(tracts_health_sf$TotalHealth), max(tracts_health_sf$TotalHealth)))




