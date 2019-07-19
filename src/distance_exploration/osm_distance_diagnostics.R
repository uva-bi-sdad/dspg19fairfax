library(sf)
library(dplyr)
library(maps)


# "I think the polys are wrong" file


#
# Prepare data --------------------------------------------------------------------------------------------------------
#

# Read in Fairfax housing data
ffx.df  <- read_csv("./data/working/Fairfax_Housing_2018/fairfax_housing_2018_geo.csv") %>% janitor::clean_names()

# Covert housing data to SF
test <- st_as_sf(ffx.df, coords = c("longitude", "latitude"))

# Read in OSM multipolygons
osm <- read_rds("./data/working/Time_distance_files/time_dist_sf.RDS")

# Extract and convert test example (alcohol stores)
test <- osm$data[[1]]


#
# Plot --------------------------------------------------------------------------------------------------------
#

# Get travel time polygons
test1 <- st_sf(test)

# Get original points
test2 <- st_as_sf(test, coords = c("lng", "lat"))

# Plot the first 10 travel time polygons with the corresponding first 10 original points
plot(st_geometry(test1[1:10, ]))
plot(st_geometry(test2[1:10, ]), add = TRUE, col = "red", lwd = 20)

# Get Virginia and Fairfax County maps to plot border
va <- map_data("county", "virginia")
va <- st_as_sf(va, coords = c("long", "lat"))

unique(va$subregion)
fairfax <- va %>% filter(subregion == "fairfax")
ffxmap <- st_as_sf(fairfax, coords = c("long", "lat"))

# Plot
plot(st_geometry(ffxmap))
plot(st_geometry(test1[1:10, ]), add = TRUE)
plot(st_geometry(test2[1:10, ]), add = TRUE, col = "red", lwd = 10)
title("10 minute drive time polygons from the first 10 alcohol shops in Fairfax County\n(supposedly)")


