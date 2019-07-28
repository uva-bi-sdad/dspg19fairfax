Visualizing Obesogenic Index Factor Analysis
================
Quinton Neville
7/26/2019

1. Data Read and Factor Model
-----------------------------

Here, we read in the necessary data for our specific index, scale, fit the desired *k*-factor model, extract the loadings (variable weights), extract the proportion of total variance explained by each factor/dimension (factor weights), and define which factors load where for our composite index construction.

``` r
#Read data original data for labels
data.orig <- read_csv("./data/working/Obesogenic_final_data/2019_7_28_obesogenic_final.csv") %>%
  mutate(
    geography = ifelse(id_type %in% "census_tract", str_split(geography, ",") %>% map_chr(1), geography)
  )

#Read Data for index construction
obesity.df <- read_rds("./data/working/Obesogenic_final_data/2019_7_28_obesogenic_final_nested.RDS") %>%
  mutate(
    data = map(data,
               ~.x %>%
                 dplyr::select(-c(geography, minority, unmarried, single_parent, limited_english,
                                  low_income, not_enrolled, no_vehicle, long_commute)))
  ) %>% unnest() %>% 
  na.omit() %>% 
  mutate(no_supermarket = 1 - no_supermarket) %>% 
  rename(supermarket = no_supermarket)

#Standardize
obesity.std <- read_rds("./data/working/Obesogenic_final_data/2019_7_28_obesogenic_final_nested.RDS") %>%
  mutate(
    data = map(data,
               ~.x %>%
                 dplyr::select(-c(minority, unmarried, single_parent, limited_english,
                                  low_income, not_enrolled, no_vehicle, long_commute))),
    data = data %>% map(.x = ., ~na.omit(.x) %>%
                          dplyr::select(-geography) %>%
                          mutate_if(is.numeric, function(x) {
                            (x - mean(x))/sd(x)}))
  ) %>% unnest() %>%
  mutate(no_supermarket = 1 - no_supermarket) %>% 
  rename(supermarket = no_supermarket)

#Final Set
final.df <- obesity.std %>%
  dplyr::select(id_type, no_insurance, no_highschool,
                hispanic, poverty, restaurant,
                fast_food, gas_station, supermarket, 
                alcohol, convenience, no_swimming_pool,
                no_team_sport, no_playground)

#Correlation Matrix and FA model
cor.mat  <- final.df %>%   
            filter(id_type %in% "census_tract") %>%
            dplyr::select(-id_type) %>%
            cor()

fact.mod <- fa(r = cor.mat, nfactors = 3, rotate = "varimax", fm = "pa")
# Extract loadings and explained proportation
fa.load     <- fact.mod$loadings %>% 
               unclass() %>%
               as_tibble() %>% 
                  mutate(variable = names(final.df %>%
                                          dplyr::select(-id_type))) %>%
               dplyr::select(variable, everything()) %>%
               rename(
                 food     = PA1,
                 ses      = PA2,     #Here you declare names of your factors
                 physical = PA3
               )

fa.prop.var <- fact.mod$Vaccounted %>% unclass() %>% as_tibble() %>% slice(2)

#Declare Which Variables load on which Factors (your input
var.load <- tibble(
  variable = colnames(final.df %>%
                       dplyr::select_if(is.numeric)) %>%
                       str_c(),
  factor  = c(rep("ses", 4), rep("food", 6), rep("physical", 3))
)
```

#### a. Composite Index Construction

Here we build an index construction function, which takes the following inputs: `data` (*final scaled data frame for FA*), `factor.data` (variable~factor data frame with two columns: variable name and factor name), `geo.type` (one of `census_tract, highschool_district, supervisor_district`), `var.weights` (weights for individual variables, default is factor loadings), and `f.weights` (weights for factor aggregation, default is proportion variance explained). The output is a data frame (tibble) with columns `geography`, describing geographic observations (i.e. census tract name, school district name, etc.)), and `factor_scores`, describing the normalized (0 - 1) final index scores for each observation.

``` r
index_construct <- function(data = final.df, factor.data = var.load, geo.type = "census_tract", var.weights = fa.load, f.weights = fa.prop.var) {
  
###Housekeeping####  
  #Check geography
  if( !(geo.type %in% c("census_tract", "highschool_district", "supervisor_district"))) stop("geography not in: c('census_tract', 'highschool_district', 'supervisor_district')")
  #Check weights
  if(nrow(var.weights) != (ncol(data) - 1) | length(f.weights) != length(unique(factor.data$factor))) stop("Variable or Factor weights are incompatible length given the data")  
  #Check data  
  if( any(data %>%
      dplyr::select_if(is.numeric) %>%
      apply(., 2, function(x) {is.na(x) %>% mean()}) != 0)
      ) stop("Missing values in the input data, take care of that please.")  
  #Check Variable Loadings (location)
  if (nrow(var.load) != (ncol(data) - 1)) stop("Variable Loading declaration incompatible length with data")
  
#Filter and Standardize Data
  data <- data %>%
    filter(id_type == geo.type)
  
  
####Create Index####
score.df  <- factor.data %>%
    nest(-factor) %>%
    rename(fa_score = data) %>%
    mutate(
      fa_score = map(.x = fa_score, ~data %>% select(.x$variable) %>% as.matrix()),
      weight.vec = map2(.x = fa_score,
                        .y = factor,
                        ~var.weights %>%
                        filter(variable %in% colnames(.x)) %>%
                        dplyr::select(.y) %>%
                        as.matrix()
                        ),
      fa_score = map2(.x = fa_score,
                          .y = weight.vec,
                          ~.x %*% .y) 
    )

#Add to original data
data.orig %>%
      filter(id_type == geo.type) %>%
      na.omit() %>%
      dplyr::select(geography) %>%
      mutate(
       factor_scores = do.call(cbind, score.df$fa_score) %*% t(as.matrix(f.weights)),
       factor_scores = as.vector(factor_scores),
       factor_scores = (factor_scores - min(factor_scores)) /
                       (max(factor_scores) - min(factor_scores))
       ) %>%
       select(geography, factor_scores) %>%
  return()
}
```

2. Visualization
----------------

#### a. Shape File for Plotting

Here, the function takes the inputs `data` (an data frame object generated by the `index_construction` function above, see documentation for description of output) and `geo.type`, again, one of `census_tract, highschool_district, supervisor_district`), `var.weights`. The output is a joined shapefile of the desired geographic region joined to the data frame of final normalized factor scores for each geagraphic region's observations, type specific.

``` r
#Collect Objects for plotting

index_shp <- function(data = census.df, geo.type = "census_tract") {
  
  ####Housekeeping
  if (names(data) != c("geography", "factor_scores")) stop("Input data wrong format: require data frame with columns geography, factor_scores")
  if ( !(geo.type %in% c("census_tract", "highschool_district", "supervisor_district"))) stop("geo.type not one of 'census_tract', 'highschool_district', 'supervisor_district'")
  
  if (geo.type %in% "census_tract") {
    #Join data by blockgroup identifier in shapefile
    data <- left_join(data, 
                      read_csv("./data/working/ACS_final_index/index.csv") %>% 
                      mutate(Geography = str_split(Geography, ",") %>% 
                         map_chr(1)) %>% 
                      rename(geography = Geography, geoid = Id2),
                      by = "geography")
    #Merge with census tract shapefile
    merge(tracts(state = "51", county = c("059")),
          data,
          by.x = "GEOID",
          by.y = "geoid") %>%
      return()
  } else if (geo.type %in% "highschool_district") {
    
    merge(readOGR("./data/original/Fairfax_Geographies/High_School_Attendance_Areas/High_School_Attendance_Areas.shp"),
          data,
          by.x = "SCHOOL_NAM",
          by.y = "geography") %>%
      return()
    
  } else {
    
    merge(readOGR("./data/original/Fairfax_Geographies/Supervisor_Districts/Supervisor_Districts.shp"),
          data,
          by.x = "DISTRICT",
          by.y = "geography") %>%
      return()
    
  }
  
}
```

#### b. ggplot Construction

First we recall the function for generating the base fairfax map `ggplot2` object.

``` r
#Base Map Function
fairfax.gg <- function() {
  fairfax.box <- getbb("fairfax county")
  fairfax.boundary <- getbb("fairfax county", format_out = "polygon") %>%
    as_tibble() %>%
    rename(longitude = `V1`, latitude = `V2`)
  
  #Grab the map info (many varieties)
  fairfax.map <- get_map(location = fairfax.box, source="stamen", maptype="watercolor", crop = TRUE)
  
  #ggmap and ggplot map and boundary
  ff.map <- ggmap(fairfax.map) +
    geom_polygon(data = fairfax.boundary, aes(x = longitude, y = latitude), colour = "black", size = 1, alpha = 0.1) +
    theme(axis.text = element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(ff.map)
}

ffx.gg <- fairfax.gg()
```

Now, we create a function to plot the final index output onto a map of fairfax county, coloured by risk level (dark - high), by specified geographic unit. Inputs to the function consist of a `geo.shp` spatial polygon generated from the `index_construction` and then `index_shp` functions above, basically an object outputted by `index_shp` which requires the output of `index_construction` (see above); `geo.type` a string defining the geographic unit (i.e. "Census Tract"); and `index.name` a character string defining the Index that this plot describes.

``` r
plot_index <- function(geo.shp    = census.df, 
                       geo.type   = "Census Tract",
                       index.name = "Obesogenic Environment") {
  
  geo.shp@data <- geo.shp@data %>% mutate(id = row.names(.))
  shp.df <- broom::tidy(geo.shp, region = "id")
  shp.df <- shp.df %>% left_join(geo.shp@data, by = c("id"="id"))
    #GG object to return
    ffx.gg +
    geom_polygon(data = shp.df, aes(x = long, y = lat, fill = factor_scores, group = group)) +
    labs(
      title = sprintf("%s Index by %s", index.name, geo.type)
    ) +
    scale_fill_viridis_c("Index Score", option = "magma", direction = -1)
    
}
```

3. Results
----------

Here, we generate the final results and subsequent visualizations. The user must define the weights (default loadings/prop. variance), where variables load, and to what unit of geography index construction is to occur.

    ## [[1]]

<img src="plot_oe_index_files/figure-markdown_github/unnamed-chunk-10-1.png" width="90%" />

    ## 
    ## [[2]]

<img src="plot_oe_index_files/figure-markdown_github/unnamed-chunk-10-2.png" width="90%" />

    ## 
    ## [[3]]

<img src="plot_oe_index_files/figure-markdown_github/unnamed-chunk-10-3.png" width="90%" />

Save figures for use later.