OSM Distance-Time Calculation
================
DSPG Business Innovation Team
7/11/2019

1. Read OSM Final Data
----------------------

#### a. Introduction

``` r
osm.df    <- read_csv("./data/working/OSM_joined/7_10_2019_osm_joined.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   environment = col_character(),
    ##   variable = col_character(),
    ##   object_id = col_character(),
    ##   longitude = col_double(),
    ##   latitude = col_double()
    ## )

``` r
osm.dim   <- dim(osm.df)
osm.names <- names(osm.df) %>% paste0(., collapse = ", ")
```

The dimensions of the raw data are 48029, , 5, and the variables contained are named environment, variable, object\_id, longitude, latitude.

#### b. Sample Park Boundary

Reading in the final osm data. Need to reduce the number of points to look through for the park polygons. Using centroids is definitely going to bias the distance-time coverage downward. Further, we are interested in whether they are close to the boundary. However, we don't need to look through every point. Instead we take a random sample of boundary points for each polygon and use those instead.

This is a simplification for computational efficiency, but with random sampling, the only error should really be random. From a uniform distribution, we should get a relatively good sample of the boundary points to calculate distance\_time.

``` r
#Investigating the Number of Points in each Park Boundary
park.df <- read_csv("./data/working/OSM_joined/7_10_2019_osm_joined.csv") %>%
  nest(-c(environment, variable)) %>%
  filter(variable == "Park") %>%
  dplyr::select(data) %>%
  unnest() %>%
  nest(-object_id) %>%
  mutate(
    length = map_dbl(data, nrow)
  ) 
#Check out length of Parks
park.df$length %>% summary()
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.00   11.00   18.00   29.92   33.00  385.00

``` r
#Distribution
park.df %>%
  ggplot(aes(x = length)) +
  geom_histogram(fill = "purple", colour = "black") +
  labs(
    x = "Length of Polygon Boundary",
    y = "Count",
    title = "Distribution of Park Boundary Length"
  )
```

<img src="osm_distance_time_calc_files/figure-markdown_github/unnamed-chunk-2-1.png" width="90%" />

As the variability in length (analogous to the size of park) is quite high, quite a few small parks and quite a few extremely large parks. As such we will take a random sample that is proportionate to the size of the park. To do say we say we will sample 25% of the boundary points at random.

``` r
#Mutating/sampling the spatial data by park object_id
park.df <- park.df %>%
  mutate(
    data = map(.x = data, 
               ~ slice(.x, sample(1:nrow(.x), (nrow(.x)/2) %>%ceiling(), replace = FALSE))
               ),
    length_new = map_dbl(data, nrow)
  )

#Check out new length of Parks
park.df$length_new %>% summary()
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.00    6.00    9.00   15.23   17.00  193.00

``` r
#Distribution
park.df %>%
  ggplot(aes(x = length_new)) +
  geom_histogram(fill = "purple", colour = "black") +
  labs(
    x = "Length of Polygon Boundary",
    y = "Count",
    title = "Distribution of Park Boundary Length"
  )
```

<img src="osm_distance_time_calc_files/figure-markdown_github/unnamed-chunk-3-1.png" width="90%" />

#### c. Replace PArk Data in OSM Final Data

``` r
osm.df <- osm.df %>%
  nest(-c(environment, variable)) %>%
  mutate(
    data = ifelse(variable == "Park", 
                  park.df %>%
                  dplyr::select(object_id, data) %>%
                  unnest(),
                  data),
    data = map(data, as_tibble)
  )



osm.df <- osm.df %>%
  unnest()
```
