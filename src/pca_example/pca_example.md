Dimensional Analysis - PCA
================
Quinton Neville
7/1/2019

A. Synthetic Example
--------------------

#### 1. Generate some synthetic continuous data

``` r
#Number of observations
N <- 100
#Number of variables
K <- 10

#Mean of MVN distribution
mu <- 5

#Generate random data from a true multivariate normal distribution with random mean vector & covariance matrix
set.seed(4)
cont.df <- mvrnorm(n = N, mu = rep(c(mu, mu + 10), each = K/2),
                   Sigma = diag(sample(seq(0.1, 1, by = 0.01), K, replace = TRUE))) %>%
           as_tibble() %>%
    mutate(
    pois_1 = rpois(N, lambda = 5),
    pois_2 = rpois(N, lambda = 5)
  ) %>%
           map_df(.x = ., ~scale(.x))
#Standardize the continuous variables
```

#### 2. Dimensional Analysis

Below we produce two principal component objects, one for the raw data and one for the correlation matrix of the raw data. Then we use a parallel analysis to look at the magnitude of the eigen values (the principal components squared), where a value greater than 1 implies we should count it as a valid underlying dimension (if it is above 1 for both PC and FA analysis).

``` r
#Raw Principle Components
pca.df <- cont.df %>% princomp()

#Correlation Matrix Principal Components
pca.corr.df <- cont.df %>% cor() %>% princomp()

#Dimensional analysis from the psych package
pearson.parallel <- cont.df %>% fa.parallel()
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-2-1.png" width="90%" />

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

Here, this plots the eigenvalues of the correlation (or raw data, can't remember), and then also does a parallel simulated analysis (below). Using a cutoff of *λ* ≥ 1, the Principal Components imply that there are around 5 dimensions, however the parallel Factor Analysis describes zero dimensions. As this is simulated data, nothing much can be gained from this.

Next we look at a Scree plot, which is just a plot of the percentage of variance explained by each principal component dimension, ordered from largest to smallest. Here, we are looking for an "elbow" in the plot, i.e. where there is a sharp drop in the percentage of variability explained between two dimensions.

``` r
#Visualization of predictors projected onto Prinipal Component Dimensions

#Scree plot raw data
fviz_eig(pca.df)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-3-1.png" width="90%" />

Here there is no obvious elbow, suggesting no underlying structure

Considering a scree plot of the correlation matrix principal components, we see that perhaps 2 or 4 dimensions is explaining a sufficient amount of variability in the correlation matrix, although there is not really strong evidence for an elbow.

``` r
#Scree Plot for correlation components
fviz_eig(pca.corr.df)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-4-1.png" width="90%" />

Lastly, we will project the features of the data (variables), onto the space spanned by the first two principal components. This two dimensional space is a transformation of the data into a space that explains the most variability in the *K* dimensional data, while also showing the direction, relationship, and magnitude of effect that each feature/variable has.

Here we look at the raw principal component projection first, and then the PCA from the correlation matrix.

``` r
fviz_pca_var(pca.df,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-5-1.png" width="90%" />

``` r
fviz_pca_var(pca.corr.df,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-5-2.png" width="90%" />

As you can see, there is basically no underlying structure to the data, most variables are relatively orthogonal (not pointing in the same direction); all of which corroborate the findings from the parallel analysis and scree plot above, that there is no underlying factor or dimension evident in these data (which makes sense because there were randomly generated from the same MVN distribution).

#### 3. Keys to Analysis

So in general, the rule of thumb is to do the parallel eigenvalue analysis (with pearson correlation for continuous variables), count the \# greater than or equal to 1; in both PC and FA. Cross reference that with the amount of variance explained, look for an elbow, and come to a consesus about how many you think there are. Lastly, project the features/variables of the data into the PCA space and look at how they're related and if the structure you expect to exist is evident in the plot. Lastly, we also employ literature review and expert opinion (domain knowledge) to inform the final selection of dimensions for further Factor Analysis.

B. ACS Dimensional Analysis
---------------------------

Read in the data.

``` r
#Read in the ACS wide and ACS long (tidy) data
acs.df <- read_csv("./data/working/ACS_joined_estimates/2019_06_24_acs_all_geography.csv")
acs.tidy.df <- read_csv("./data/working/ACS_joined_estimates/2019_06_24_acs_all_geography_tidy.csv")
```

#### Highschool District

``` r
#Read and filter
highschool.df <- acs.df %>%
  filter(id_type %in% "highschool_district") %>%
  dplyr::select(-id_type) %>%
  dplyr::select(-contains('moe'))

#Scale
highschool.df <- bind_cols(highschool.df %>%
                             dplyr::select(id), 
                           highschool.df %>%
                             dplyr::select_if(is.numeric) %>%
                             map_df(scale))

#Grab Principal Components
#pca.hs <- highschool.df %>% 
#  dplyr::select_if(is.numeric) %>%
#  princomp()

#Grabe PC's from Correlation matrix
pca.cor.hs <- highschool.df %>% 
  dplyr::select_if(is.numeric) %>%
  cor() %>%
  princomp()

#Parallel analysis
#highschool.df %>% select_if(is.numeric) %>% fa.parallel()

#Scree Plot for correlation components
fviz_eig(pca.cor.hs)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-7-1.png" width="90%" />

``` r
#Feature map
fviz_pca_var(pca.cor.hs,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-7-2.png" width="90%" />

``` r
#Code for mapping median across list of df's, need for Housing Stock
#median.highschool.df$median_var[2] %>% unlist() %>% names()
#list(a = tibble(x = 1:10, y = 11:20),
#     b = tibble(x = 1:5, y = 6:10)) %>% map(., ~map_dbl(., median))
```

#### Supervisor District

``` r
#Read and filter
supervisor.df <- acs.df %>%
  filter(id_type %in% "supervisor_district") %>%
  dplyr::select(-id_type) %>%
  dplyr::select(-contains('moe'))

#Scale
highschool.df <- bind_cols(highschool.df %>%
                             dplyr::select(id), 
                           highschool.df %>%
                             dplyr::select_if(is.numeric) %>%
                             map_df(scale))

#Grab Principal Components
#pca.hs <- highschool.df %>% 
#  dplyr::select_if(is.numeric) %>%
#  princomp()

#Grabe PC's from Correlation matrix
pca.cor.sv <- supervisor.df %>% 
  dplyr::select_if(is.numeric) %>%
  cor() %>%
  princomp()

#Parallel analysis
#highschool.df %>% select_if(is.numeric) %>% fa.parallel()

#Scree Plot for correlation components
fviz_eig(pca.cor.sv)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-8-1.png" width="90%" />

``` r
#Feature map
fviz_pca_var(pca.cor.sv,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-8-2.png" width="90%" />

#### ACS + Housing Stock

``` r
ffh.highschool.df <- read_csv("./data/working/Fairfax_Housing_2018/fairfax_housing_2018_geo.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-c(geoid, parcel_id, district)) %>%
  rename(id = highschool) %>%
    nest(-id) %>%
    mutate(
      data   = map(data, ~dplyr::select_if(., is.numeric)) %>%
               map(na.omit),
      median = data %>% map(.x = ., ~map_dbl(., median)),
      median = median %>% map(.x = ., ~as_tibble(as.list(.x)))
    ) %>%
    dplyr::select(-c(data)) %>%
  unnest()

ffh.district.df<- read_csv("./data/working/Fairfax_Housing_2018/fairfax_housing_2018_geo.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-c(geoid, parcel_id, highschool)) %>%
  rename(id = district) %>%
    nest(-id) %>%
    mutate(
      data   = map(data, ~dplyr::select_if(., is.numeric)) %>%
               map(na.omit),
      median = data %>% map(.x = ., ~map_dbl(., median)),
      median = median %>% map(.x = ., ~as_tibble(as.list(.x)))
    ) %>%
    dplyr::select(-c(data)) %>%
  unnest() 
```

Here, looking at the median number of housing units, bedrooms, and bathrooms; the results are almost consistent across all units of geography meaning that median number of housing units is not a good measure (perhaps another quantile %?), and bedrooms and bathrooms should just be categorical to begin with. So for now, we will remove those and continue dimensional analysis with only the true numeric variables (year\_built, long, lat, value(s), and living area). To do so, we join the acs and housing stock estimates like so:

``` r
total.highschool.df  <- left_join(ffh.highschool.df,
                                 acs.df %>%
                                 filter(id_type %in% "highschool_district") %>%
                                 dplyr::select(-id_type) %>%
                                 dplyr::select(-contains('moe')),
                                 by = "id") %>%
  dplyr::select(-c(num_units, bedrooms, bathrooms))

total.supervisor.df <- left_join(ffh.district.df,
                                 acs.df %>%
                                 filter(id_type %in% "supervisor_district") %>%
                                 dplyr::select(-id_type) %>%
                                 dplyr::select(-contains('moe')),
                                 by = "id") %>%
  dplyr::select(-c(num_units, bedrooms, bathrooms))
```

##### ACS + Housing; Highschool

``` r
#Grabe PC's from Correlation matrix
pca.cor.hs <- total.highschool.df %>% 
  dplyr::select_if(is.numeric) %>%
  cor() %>%
  princomp()

#Parallel analysis
#highschool.df %>% select_if(is.numeric) %>% fa.parallel()

#Scree Plot for correlation components
fviz_eig(pca.cor.hs)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-11-1.png" width="90%" />

``` r
#Feature map
fviz_pca_var(pca.cor.hs,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-11-2.png" width="90%" />

``` r
#Plot of Singular Values
tibble(
  `Singular Values` = pca.cor.hs$sdev %>% sort(., decreasing = TRUE),
  order = 1:length(`Singular Values`) 
) %>%
  ggplot(aes(x = order, y = `Singular Values`)) +
  geom_line(colour = "black", alpha = 0.72, size = 1) +
  geom_point(aes(size = `Singular Values`, colour = `Singular Values`)) +
  scale_colour_viridis_c("Singular Values") +
  scale_size_continuous("Singular Values") + 
  labs(
    x = "Order",
    y = "Singular Values",
    title = "Highschool Correlation Ordered Singular Values"
  )
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-11-3.png" width="90%" />

Overall, we see that there is not necessarily convincing evidence that more than two dimensions exist within the combined ACS + Housing stock data by Highschool district. The plot of the features in PCA space does show that the Median agregated FF Housing Stock Variables (found in green in the lower right), are clearly explaining variance in a very different direction from the other variables; however the lack of magnitude and measuring just the same dimension in the oposite direction (potentially), may not be enought for them to capture a new dimension of the data vs the acs variables.

Overall, I would still investigate a 1-4 Factor model, but most likely 2 or 3 factor will be sufficient for a good model.

##### ACS + Housing; Supervisor

``` r
#Grabe PC's from Correlation matrix
pca.cor.sv <- total.supervisor.df %>% 
  dplyr::select_if(is.numeric) %>%
  cor() %>%
  princomp()

#Parallel analysis
#highschool.df %>% select_if(is.numeric) %>% fa.parallel()

#Scree Plot for correlation components
fviz_eig(pca.cor.sv)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-12-1.png" width="90%" />

``` r
#Feature map
fviz_pca_var(pca.cor.sv,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-12-2.png" width="90%" />

``` r
#Plot of Singular Values
tibble(
  `Singular Values` = pca.cor.sv$sdev %>% sort(., decreasing = TRUE),
  order = 1:length(`Singular Values`) 
) %>%
  ggplot(aes(x = order, y = `Singular Values`)) +
  geom_line(colour = "black", alpha = 0.72, size = 1) +
  geom_point(aes(size = `Singular Values`, colour = `Singular Values`)) +
  scale_colour_viridis_c("Singular Values") +
  scale_size_continuous("Singular Values") + 
  labs(
    x = "Order",
    y = "Singular Values",
    title = "Supervisor Correlation Ordered Singular Values"
  )
```

<img src="pca_example_files/figure-markdown_github/unnamed-chunk-12-3.png" width="90%" />

Considering the ACS + FF Housing stock data at the Supervisor district level, there is actual sufficient evidence to suggest that at least 3 distinct dimensions exist within these data (by *λ* ≥ 1 and % variance explanation). Further, this is corroborated by the features in pca space plot, as the spread and orthogonality of the FF Housing stock variables (at median, found in green primarily at the top) compared to the rest of the higher magnitude ACS variables suggests it is actually picking up another significant dimension seperate of the ACS data.

Overall, I would reccomend investigating 2-5 Factor models, which a suggestion that the 3 Factor model will fit the best.
