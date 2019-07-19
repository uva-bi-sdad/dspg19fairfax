-   [PCA on ACS data based on Census Tract](#pca-on-acs-data-based-on-census-tract)
    -   [Identify Dimensions](#identify-dimensions)
        -   [Number of principle components](#number-of-principle-components)
        -   [Variances in the principal components](#variances-in-the-principal-components)
    -   [Dimension description](#dimension-description)
        -   [Contributions of the variables to the principal components](#contributions-of-the-variables-to-the-principal-components)
        -   [Correlations/Loadings of variables with the principal components](#correlationsloadings-of-variables-with-the-principal-components)
        -   [The most correlated variables in a given principal component.](#the-most-correlated-variables-in-a-given-principal-component.)
-   [PCA on ACS data based on High School District](#pca-on-acs-data-based-on-high-school-district)

PCA on ACS data based on Census Tract
=====================================

``` r
data <- read.csv("./data/working/ACS_final_index_2/07_11_2019_joined_acs_final.csv") %>% select (-c(X, X.x, id, id_type, X.y)) %>% na.omit()

#PCA with standadization
pr.data <- PCA(data, scale.unit = TRUE, graph = FALSE)
```

The output of the function PCA() is a list including :

``` r
print(pr.data)
```

    ## **Results for the Principal Component Analysis (PCA)**
    ## The analysis was performed on 288 individuals, described by 13 variables
    ## *The results are available in the following objects:
    ## 
    ##    name               description                          
    ## 1  "$eig"             "eigenvalues"                        
    ## 2  "$var"             "results for the variables"          
    ## 3  "$var$coord"       "coord. for the variables"           
    ## 4  "$var$cor"         "correlations variables - dimensions"
    ## 5  "$var$cos2"        "cos2 for the variables"             
    ## 6  "$var$contrib"     "contributions of the variables"     
    ## 7  "$ind"             "results for the individuals"        
    ## 8  "$ind$coord"       "coord. for the individuals"         
    ## 9  "$ind$cos2"        "cos2 for the individuals"           
    ## 10 "$ind$contrib"     "contributions of the individuals"   
    ## 11 "$call"            "summary statistics"                 
    ## 12 "$call$centre"     "mean of the variables"              
    ## 13 "$call$ecart.type" "standard error of the variables"    
    ## 14 "$call$row.w"      "weights for the individuals"        
    ## 15 "$call$col.w"      "weights for the variables"

Identify Dimensions
-------------------

### Number of principle components

One way to determine the number of factors or components in a data matrix is to examine the scree plot of the successive eigenvalues. Sharp breaks in the plot suggest the appropriate number of components or factors to extract. The scree plot below shows ~45% of the variances contained in the data are retained by the first principal component. The elbow is between the first and the second dimensions.

``` r
fviz_screeplot(pr.data, ncp=10)
```

<img src="pca_files/figure-markdown_github/unnamed-chunk-3-1.png" width="90%" />

Parallel analyis compares the scree of factors of the observed data with that of a random data matrix of the same size as the original. The parallel analysis for this dataset indicates that **two components** should be retained. There are two ways to tell this; (1) two of the eigenvalues in the actual data are greater than the simulated/resampled data, and (2) the dashed line for parallel analysis in the graph crosses the blue line before reaching the third component.

``` r
psych::fa.parallel(data,fa="pc")
```

<img src="pca_files/figure-markdown_github/unnamed-chunk-4-1.png" width="90%" />

    ## Parallel analysis suggests that the number of factors =  NA  and the number of components =  2

### Variances in the principal components

The proportion of variances retained by the principal components can be extracted as follow. ~55% of variance is explain by the first two pricipal components.

``` r
eigenvalues <- pr.data$eig
head(eigenvalues)
```

    ##        eigenvalue percentage of variance cumulative percentage of variance
    ## comp 1  5.8010306              44.623312                          44.62331
    ## comp 2  1.3684755              10.526735                          55.15005
    ## comp 3  1.1072711               8.517470                          63.66752
    ## comp 4  0.9439232               7.260948                          70.92846
    ## comp 5  0.7863043               6.048495                          76.97696
    ## comp 6  0.7202168               5.540129                          82.51709

Dimension description
---------------------

### Contributions of the variables to the principal components

Variables that are correlated with PC1 and PC2 are the most important in explaining the variability in the data set. The contribution of variables can be extracted as follow:

``` r
pr.data$var$contrib
```

    ##                               Dim.1        Dim.2        Dim.3
    ## pai_pct                  2.92765177  0.270636926  9.698221063
    ## ssi_pct                  3.39775291  3.622227496 12.687116511
    ## unenrolled_pct           0.03478742 53.784796197  0.828637312
    ## poverty_pct             11.05639564  0.713123279  0.203981536
    ## no_hicov_pct            14.91238777  0.152032162  0.002904081
    ## over_30min_commute_pct   0.09793316  5.066942377 53.657033366
    ## unemployed_pct           1.97784190 31.302603474 16.130187471
    ## hispanic_pct            13.61302828  0.181859522  0.006469156
    ## minority_pct             7.27148640  0.004716716  0.035467578
    ## unmarried_pct            7.23922192  2.654609940  6.010002036
    ## family_singleparent_pct 10.93930399  1.416955610  0.087463337
    ## limited_ability         12.72843650  0.768376642  0.146778927
    ## less_highschool_pct     13.80377235  0.061119659  0.505737625
    ##                                 Dim.4       Dim.5
    ## pai_pct                 50.5709267052 15.17406310
    ## ssi_pct                 13.0743854214 42.15471916
    ## unenrolled_pct           0.0001634107 12.25808390
    ## poverty_pct              1.5359792455  0.08421579
    ## no_hicov_pct             2.1097347875  1.26649994
    ## over_30min_commute_pct  20.6973393719  1.25946417
    ## unemployed_pct           0.1426576847 10.39360194
    ## hispanic_pct             4.7746546030  2.92788888
    ## minority_pct             3.1612244668  0.05408768
    ## unmarried_pct            0.8297827919 11.35540160
    ## family_singleparent_pct  0.0619197515  1.83261617
    ## limited_ability          0.4395065439  0.79114922
    ## less_highschool_pct      2.6017252158  0.44820844

If the contribution of the variables were uniform, the expected value would be 1/length(variables) = 1/10 = 10%.The red dashed line on the graph below indicates the expected average contribution on the first two components. A variable with a contribution larger than this cutoff could be considered as important in contributing to the component.

``` r
fviz_pca_contrib(pr.data, choice = "var", axes = 1)
```

    ## Warning in fviz_pca_contrib(pr.data, choice = "var", axes = 1): The
    ## function fviz_pca_contrib() is deprecated. Please use the function
    ## fviz_contrib() which can handle outputs of PCA, CA and MCA functions.

<img src="pca_files/figure-markdown_github/unnamed-chunk-7-1.png" width="90%" />

``` r
fviz_pca_contrib(pr.data, choice = "var", axes = 2)
```

    ## Warning in fviz_pca_contrib(pr.data, choice = "var", axes = 2): The
    ## function fviz_pca_contrib() is deprecated. Please use the function
    ## fviz_contrib() which can handle outputs of PCA, CA and MCA functions.

<img src="pca_files/figure-markdown_github/unnamed-chunk-7-2.png" width="90%" />

### Correlations/Loadings of variables with the principal components

The variables can be plotted as points in the component space using their loadings (correlation between a variable and a PC) as coordinates.

``` r
head(pr.data$var$coord)
```

    ##                              Dim.1       Dim.2       Dim.3        Dim.4
    ## pai_pct                 0.41210918 -0.06085721  0.32769742  0.690905717
    ## ssi_pct                 0.44396473 -0.22264163  0.37480765  0.351300665
    ## unenrolled_pct          0.04492248  0.85792294  0.09578759  0.001241963
    ## poverty_pct             0.80086509  0.09878723 -0.04752503 -0.120409570
    ## no_hicov_pct            0.93009256  0.04561275 -0.00567063 -0.141117951
    ## over_30min_commute_pct -0.07537329 -0.26332464  0.77079753 -0.442003381
    ##                              Dim.5
    ## pai_pct                -0.34541904
    ## ssi_pct                 0.57572944
    ## unenrolled_pct          0.31046070
    ## poverty_pct             0.02573310
    ## no_hicov_pct           -0.09979250
    ## over_30min_commute_pct  0.09951493

Correlation circle projects the features/variables of the data into the space of the first two dimensions. The color shows the value of variables' contributions. Variables that group together are the most correlated.

``` r
fviz_pca_var(pr.data, col.var="contrib",repel = TRUE)+
  scale_color_gradient2(low="white", mid="blue", 
                      high="red")
```

<img src="pca_files/figure-markdown_github/unnamed-chunk-9-1.png" width="90%" />

### The most correlated variables in a given principal component.

The top significant variables for the Dimension 1 are : The percentage of households with no health insurance coverage (no\_hicov\_pct) and the percentage of population with less than highschool education (less\_highschool\_pct).

``` r
pr.desc <- dimdesc(pr.data, axes = c(1,2))
pr.desc$Dim.1
```

    ## $quanti
    ##                         correlation       p.value
    ## no_hicov_pct              0.9300926 2.037674e-126
    ## less_highschool_pct       0.8948525 3.402260e-102
    ## hispanic_pct              0.8886484  7.788687e-99
    ## limited_ability           0.8592907  2.921006e-85
    ## poverty_pct               0.8008651  1.204665e-65
    ## family_singleparent_pct   0.7966131  1.772004e-64
    ## minority_pct              0.6494776  6.818126e-36
    ## unmarried_pct             0.6480351  1.084758e-35
    ## ssi_pct                   0.4439647  2.433965e-15
    ## pai_pct                   0.4121092  3.103631e-13
    ## unemployed_pct            0.3387259  3.664778e-09

The top significant variables for the Dimension 2 are : The percentage of population that are 3 year old and above not enrolled in school (unenrolled\_pct) and the percentage of population that are 15 years old and above that are unmarried (unmarried\_pct).

``` r
pr.desc$Dim.2
```

    ## $quanti
    ##                         correlation      p.value
    ## unenrolled_pct            0.8579229 1.050214e-84
    ## unmarried_pct             0.1905982 1.152674e-03
    ## family_singleparent_pct  -0.1392505 1.805871e-02
    ## ssi_pct                  -0.2226416 1.390511e-04
    ## over_30min_commute_pct   -0.2633246 5.911642e-06
    ## unemployed_pct           -0.6544986 1.327678e-36

PCA on ACS data based on High School District
=============================================
