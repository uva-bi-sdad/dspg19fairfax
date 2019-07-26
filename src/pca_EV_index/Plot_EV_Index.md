Economic Vulerability Index Map
================
Cong Cong
7/23/2019

``` r
#Base Map Function
fairfax.gg <- function() {
  fairfax.box <- getbb("fairfax county")
  fairfax.boundary <- getbb("fairfax county", format_out = "polygon") %>%
    as.tibble() %>%
    rename(longitude = `V1`, latitude = `V2`)
  
  #Grab the map info (many varieties)
  fairfax.map <- get_map(location = fairfax.box, source="stamen", maptype="watercolor", crop = TRUE)
  
  #ggmap and ggplot map and boundary
  ff.map <- ggmap(fairfax.map) +
    geom_polygon(data = fairfax.boundary, aes(x = longitude, y = latitude), colour = "black", size = 1, alpha = 0.1) +
    labs(
      x = "Longitude",
      y = "Latitude"
    )
  return(ff.map)
}
```

Create functions
----------------

### Functions for calculating Economic Vulnerability Index in three scenarios

``` r
# Scenario 1: weight by model loadings
# geo_type: "census_tract", "highschool_district", "supervisor_district"
create_index_modelweight <- function(data = data, geo_type = "census_tract", loadings = loadings){
  # Initialize dataframe
  df <- data %>% filter(id_type == geo_type) %>% .[,1] %>% data.frame()
  # Select by id_type then normalize data (max&min)
  slice_std <- data %>% filter(id_type == geo_type) %>%
    select (-c(Geography, id_type)) %>% apply(., 2, function(x) (x - min(x))/(max(x)-min(x))) %>% data.frame()
  # Variables in the first dimension
  factor1 <- slice_std %>% select(no_insurance, no_highschool, hispanic, limited_english, poverty, single_parent, no_vehicle)
  # Variables in the second dimension
  factor2 <- slice_std %>% select(median_house_value, no_sewer, no_water)
  # Here the index is created by sum(variable value * loading * proportion explained)/sum(loading * proportion explained)
  df$numerator <- proportion["Proportion Explained","PA1"] * (factor1$no_insurance * loadings["no_insurance","PA1"] +
                                                                factor1$no_highschool * loadings["no_highschool","PA1"] +
                                                                factor1$hispanic * loadings["hispanic","PA1"] +
                                                                factor1$limited_english * loadings["limited_english","PA1"] +
                                                                factor1$poverty * loadings["poverty","PA1"] +
                                                                factor1$single_parent * loadings["single_parent","PA1"] +
                                                                factor1$no_vehicle * loadings["no_vehicle","PA1"]) +
                  proportion["Proportion Explained","PA2"] * (factor2$median_house_value * loadings["median_house_value","PA2"] +
                                                                factor2$no_sewer * loadings["no_sewer","PA2"] +
                                                                factor2$no_water * loadings["no_water","PA2"])
    
  df$denominator <- proportion["Proportion Explained","PA1"] * (loadings["no_insurance","PA1"] +
                                                                  loadings["no_highschool","PA1"] +
                                                                  loadings["hispanic","PA1"] +
                                                                  loadings["limited_english","PA1"] +
                                                                  loadings["poverty","PA1"] +
                                                                  loadings["single_parent","PA1"] +
                                                                  loadings["no_vehicle","PA1"])+
                    proportion["Proportion Explained","PA2"] * (loadings["median_house_value","PA2"] +
                                                                  loadings["no_sewer","PA2"] +
                                                                  loadings["no_water","PA2"])
  df$EV_INDEX <- df$numerator / df$denominator
  colnames(df)[1] <- "Geography"
  return(df)
}

# Scenario 2: no weight
create_index_noweight <- function(data = data, geo_type = "census_tract", loadings = loadings){
  # Initialize dataframe
  df <- data %>% filter(id_type == geo_type) %>% .[,1] %>% data.frame()
  # Select by id_type then normalize data (max&min)
  slice_std <- data %>% filter(id_type == geo_type) %>%
    select (-c(Geography, id_type)) %>% apply(., 2, function(x) (x - min(x))/(max(x)-min(x))) %>% data.frame()
  # Variables in the first dimension
  factor1 <- slice_std %>% select(no_insurance, no_highschool, hispanic, limited_english, poverty, single_parent, no_vehicle)
  # Variables in the second dimension
  factor2 <- slice_std %>% select(median_house_value, no_sewer, no_water)
  # Here the index is created by sum(variable value * loading * proportion explained)/sum(loading * proportion explained)
  df$PA1 <- factor1$no_insurance + factor1$no_highschool + factor1$hispanic +
    factor1$limited_english + factor1$poverty + factor1$single_parent + factor1$no_vehicle
  df$PA2 <- factor2$median_house_value + factor2$no_sewer + factor2$no_water
  df$EV_INDEX <- df$PA1 + df$PA2
  colnames(df)[1] <- "Geography"
  return(df)
}

# Scenario 3: suggested weight
```

### Plotting function

``` r
# Function: Plot
# geo_shp: census_tract, highschool_district, supervisor_district
# geo_name: "Census Tract", "Highschool District", "Supervisor District"
plot_by_geography <- function(geo_shp = highschool_district, geo_name = "Highschool District"){
  geo_shp@data <- geo_shp@data %>% mutate(id = row.names(.))
  shp_df <- broom::tidy(geo_shp,region = "id")
  shp_df <- shp_df %>% left_join(geo_shp@data, by = c("id"="id"))
  fairfax.gg() + 
    geom_polygon(data=shp_df, aes(x = long, y = lat, fill = EV_INDEX, group = group))+
    ggtitle(label = "Economic Vulnerability Index by ${geo_name}" %>% str_interp()) +
    scale_fill_viridis_c(option = "plasma",direction = -1) +
    labs(caption = "Note: Darker color shows higher economic vulnerability.")
}
```

Generate factor analysis results
--------------------------------

``` r
# Read data
data <- read.csv("./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv")  %>% na.omit() 
# Create loadings from the result of factor analysis
datastd <- data %>% select (-c(Geography, id_type)) %>% scale(.,center = TRUE, scale = TRUE) %>% data.frame()
final <- datastd %>% select(no_insurance, no_highschool, hispanic, limited_english, poverty, single_parent, no_vehicle,
                            median_house_value, no_sewer, no_water)
finalcormat <- cor(final)
finalfact <- fa(r = finalcormat, nfactors = 2, rotate = "varimax", fm = "pa")
# Extract loadings and explained proportation
loadings <- data.frame(unclass(finalfact$loadings)) 
proportion <- print(finalfact) %>% .$Vaccounted %>% data.frame()
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = finalcormat, nfactors = 2, rotate = "varimax", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                      PA1   PA2   h2   u2 com
    ## no_insurance        0.93 -0.14 0.89 0.11 1.0
    ## no_highschool       0.91 -0.10 0.84 0.16 1.0
    ## hispanic            0.91 -0.14 0.85 0.15 1.0
    ## limited_english     0.82 -0.14 0.69 0.31 1.1
    ## poverty             0.79 -0.07 0.64 0.36 1.0
    ## single_parent       0.71 -0.19 0.54 0.46 1.1
    ## no_vehicle          0.63 -0.06 0.41 0.59 1.0
    ## median_house_value -0.38  0.50 0.40 0.60 1.9
    ## no_sewer           -0.06  0.89 0.80 0.20 1.0
    ## no_water           -0.04  0.90 0.81 0.19 1.0
    ## 
    ##                        PA1  PA2
    ## SS loadings           4.88 1.97
    ## Proportion Var        0.49 0.20
    ## Cumulative Var        0.49 0.69
    ## Proportion Explained  0.71 0.29
    ## Cumulative Proportion 0.71 1.00
    ## 
    ## Mean item complexity =  1.1
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  8.33
    ## The degrees of freedom for the model are 26  and the objective function was  0.61 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.04 
    ## The df corrected root mean square of the residuals is  0.05 
    ## 
    ## Fit based upon off diagonal values = 0.99
    ## Measures of factor score adequacy             
    ##                                                    PA1  PA2
    ## Correlation of (regression) scores with factors   0.98 0.95
    ## Multiple R square of scores with factors          0.96 0.90
    ## Minimum correlation of possible factor scores     0.92 0.79

Plot by census tract
--------------------

``` r
df <- create_index_modelweight(data, "census_tract", loadings)
#df <- create_index_noweight(data, "census_tract", loadings)
tracts <- tracts(state = '51', county = c('059'))
id <- read_csv("./data/working/ACS_final_index/index.csv")
join <- merge(df,id,by.x = "Geography",by.y = "Geography")
census_tract <- merge(tracts, join, by.x="GEOID",by.y="Id2",all.x=TRUE)
plot_by_geography(census_tract, "Census Tract")
```

<img src="Plot_EV_Index_files/figure-markdown_github/unnamed-chunk-5-1.png" width="90%" />

Plot by school district
-----------------------

``` r
df <- create_index_modelweight(data, "highschool_district", loadings)
#df <- create_index_noweight(data, "highschool_district", loadings)
school_shp <- readOGR("./data/original/Fairfax_Geographies/High_School_Attendance_Areas/High_School_Attendance_Areas.shp")
highschool_district<-merge(school_shp,df,by.x='SCHOOL_NAM',by.y='Geography',all.x=TRUE)
plot_by_geography(highschool_district, "Highschool District")
```

<img src="Plot_EV_Index_files/figure-markdown_github/unnamed-chunk-6-1.png" width="90%" />

Plot by supervisor district
---------------------------

``` r
df <- create_index_modelweight(data, "supervisor_district", loadings)
#df <- create_index_noweight(data, "supervisor_district", loadings)
svd_shp <- readOGR("./data/original/Fairfax_Geographies/Supervisor_Districts/Supervisor_Districts.shp")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/home/sdad/project_data/ffx/dspg2019fairfax/original/Fairfax_Geographies/Supervisor_Districts/Supervisor_Districts.shp", layer: "Supervisor_Districts"
    ## with 9 features
    ## It has 12 fields
    ## Integer64 fields read as strings:  OBJECTID

``` r
supervisor_district<-merge(svd_shp,df,by.x='DISTRICT',by.y='Geography',all.x=TRUE)
plot_by_geography(supervisor_district, "Supervisor District")
```

<img src="Plot_EV_Index_files/figure-markdown_github/unnamed-chunk-7-1.png" width="90%" />
