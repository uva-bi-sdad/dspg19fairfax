OSM Final Data Join
================
DSPG Business Innovation Team
7/10/2019

1. Build OSM Data Frame for Mapping API Calls
---------------------------------------------

Here we are generating a tibble with an environment (Food vs. Physical), variable, and nested list/tibble with columns key, value, type.

``` r
#Generate Food Environment Tibble (variable, key, value, type)
food.key.df <- tibble(
  variable = c("Fast Food", "Supermarket", rep("Convenience", times = 2), "Restaurant", "Gas Station", rep("Alcohol", times= 3)),
  key      = c("amenity", "shop", "shop", "shop", "amenity", "amenity", "shop", "amenity", "amenity"),
  value    = c("fast_food", "supermarket", "convenience", "greengrocer", "restaurant", "fuel", "alcohol", "bar", "pub"),
  type     = rep("point", times = length(value))
) %>%
  nest(-variable)

#Generate Physical Environment Tibble (variable, key, value, type)
phys.key.df <- tibble(
  variable = c("Playground", rep("Park", times = 3), "Swimming Pool", "Sports Center", "Track", rep("Team Sport", times = 4)),
  key      = c(rep("leisure", times = 3), "boundary", rep("leisure", times = 3), rep("sport", times = 4)),
  value    = c("playground", "park", "nature_reserve", "national_park", "swimming_pool", "sports_centre", "track", "baseball", "basketball", "soccer", "tennis"),
  type    = c("point", rep("polygon", times = 3), rep("point", times = 7))
) %>%
  nest(-variable)

#Join for total osm obesegenic tibble (now with environment indicator)
obese.df <- bind_rows(food.key.df,
                      phys.key.df) %>%
            mutate(
              environment = c(
                rep("Food",     times = nrow(food.key.df)),
                rep("Physical", times = nrow(phys.key.df))
              )
            ) %>%
            dplyr::select(environment, variable, everything())
#Display
head(obese.df) %>% knitr::kable()
```

2. Map OSM API Call
-------------------

Here, we are going to map over the nested key, value, type lists/tibbles *per variable*, to generate a tibble of the object\_id, longitude, and latitude per each specific aspect of each variable (e.g. nature reserve vs. park, within the variable park). The resulting unnested frame will have 6 variables: environment, variable, object\_id (specific aspect + number), longitude, and latitude.

Here we retain all the factor identifiers so we can pass this to the distance-time API to generate our final index variables.

#### Disregard Points Outside Fairfax Boundary (for Computation)

Here we get the polygon defined by the FF boundary, and apply the `point.in.polygon` function from the `sp` package to keep only those inside Fairfax. This is because the data we obtained from OSM describes geolocations for objects in a square around fairfax, and all the housing data is inside fairfax, so looking at points outside is computationally inefficient. Actually, it also biases the proportional results. Thus restrict the scope of our OSM features to those within FF County.

Example for Playgrounds

``` r
#Define the Boundary
fairfax.boundary <- getbb("fairfax county", format_out = "polygon") %>%
  as_tibble() %>%
  rename(longitude = `V1`, latitude = `V2`)

#Example for Playgrounds
##Original with all points in sqaure grid around FF
nrow(osm_to_df())

##Trimmed set within FF
osm_to_df() %>% 
  mutate(in_fairfax = ifelse(
point.in.polygon(longitude, latitude, fairfax.boundary$longitude, fairfax.boundary$latitude) %in% 1, TRUE, FALSE
)) %>%
  filter(in_fairfax == TRUE) %>%
  select(-in_fairfax) %>%
  nrow()
```

#### Build the Map function on the First OSM Observation

``` r
import_osm <- function(tibble) {
  
  #Map import osm over rows of key, value, type tibble; rbind into cohesive output frame
  pmap_df(
    tibble,
    ~osm_to_df(key = ..1, value = ..2, type = ..3) %>%
          mutate(object_id = paste0(..2, object_id))
              ) %>% mutate(
                     in_fairfax = ifelse(point.in.polygon(longitude, latitude,
                                         fairfax.boundary$longitude,
                                         fairfax.boundary$latitude) %in% 1,
                                         TRUE,
                                         FALSE)
                     ) %>%
                      filter(in_fairfax == TRUE) %>%
                      select(-in_fairfax)
  #Output is a tibble with columns object_id (attribute + id), longitude, and latitude
  ###Where observations have been trimmed to those within the Fairfax County Boundary
}

#Now that we've built the necessary functions, time to map through all key, val pairs, nest into obese.df
obese.df <- obese.df %>%
  mutate(
    osm_df = map(data, import_osm)
  )

#View the head
head(obese.df) %>% knitr::kable()
```

#### Unnest and Write out csv

``` r
obese.df <- obese.df %>%
            unnest(osm_df)

dim(obese.df)
names(obese.df) %>% paste0(., collapse = ", ")

write_csv(obese.df, path = "./data/working/OSM_joined/7_10_2019_osm_joined.csv")
```
