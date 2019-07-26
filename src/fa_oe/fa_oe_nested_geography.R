library(psych)
library(tidyverse)

#
# Prepare data ----------------------------------------------------------------------------------------
#

# Read in data
#data <- read_csv("./data/working/Obesogenic_final_data/2019_7_25_obesogenic_final.csv") %>% select(-id_type, -geography) %>% na.omit()
#any(is.na(data))

# Center and standardize
#datastd <- data.frame(scale(data, center = TRUE, scale = TRUE))
#describe(datastd)

# Prepare correlation matrix
#cormat <- cor(datastd, use = "na.or.complete")

#Read the data
obesity.df <- read_rds("./data/working/Obesogenic_final_data/2019_7_25_obesogenic_final_nested.RDS") %>%
  mutate(
    data = map(data,
               ~.x %>%
                 dplyr::select(-c(minority, unmarried, single_parent, limited_english,
                                  low_income, not_enrolled, no_vehicle, long_commute)))
  )


#Scale the data
obesity.scale.df <- obesity.df %>%
  mutate(
    data = data %>% map(.x = ., ~na.omit(.x) %>%
                          dplyr::select(-geography) %>%
                          mutate_if(is.numeric, function(x) {
                            (x - mean(x))/sd(x)})),
    cor_data = data %>% map(.x = ., ~.x %>% cor(., method = "pearson")),
    pca_data = cor_data %>% map(princomp)
  ) 


#
# FA exploratory ----------------------------------------------------------------------------------------
#

# Good model: INTERPRETABLE!
# The root mean square of residuals (RMSR) should be close to 0.
# Root mean square error of approximation (RMSEA) index should be below 0.05. 
# Tucker-Lewis Index (TLI) should be over 0.9.

# Play with rotations and algorithms 

# Each factor captures a certain amount of the overall variance in the observed variables.
# Eigenvalue = much of the variance of the observed variables a factor explains. A factor with eigenvalue ≥1 explains more variance than a single observed variable.
# A factor loading represents the strength of association between each variable and the latent factor.


#
# FA tentative model ----------------------------------------------------------------------------------------
#

# Select indicators
final <- obesity.df %>%
  mutate(
    data = data %>% map(.x = ., ~na.omit(.x) %>%
                          dplyr::select(no_insurance, no_highschool, hispanic, poverty,
                                        restaurant, fast_food, gas_station, no_supermarket, 
                                        alcohol, convenience, no_swimming_pool, no_team_sport, no_playground) %>%
                          mutate_if(is.numeric, function(x) {
                            (x - mean(x))/sd(x)})),
    cor_data        = data %>% map(.x = ., ~.x %>% cor(., method = "pearson")),
    pca_data        = cor_data %>% map(princomp), 
    factor_analysis = map(.x = cor_data, ~fa(.x, nfactors = 3, rotate = "varimax", fm = "pa"))
  )

# FA
finalfact <- fa(r = finalcormat, nfactors = 3, rotate = "varimax", fm = "pa")
finalfact
print(finalfact$loadings, cutoff = 0.3)
fa.diagram(finalfact, cut = 0.3)


#
# Confirm ----------------------------------------------------------------------------------------
#

# Calculate internal consistency
factor1 <- final %>% select(no_insurance, no_highschool, hispanic, poverty)
factor2 <- final %>% select(restaurant, fast_food, gas_station, supermarket, alcohol, convenience)
factor3 <- final %>% select(team_sport, swimming_pool, playground)
  
psych::alpha(factor1) # alpha = 0.94
psych::alpha(factor2) # alpha = 0.92
psych::alpha(factor3) # alpha = 0.81

# First dimension: no_insurance, no_highschool, hispanic, poverty
# Second dimension: restaurant, fast_food, gas_station, supermarket, alcohol, convenience
# Third dimension: team_sport, swimming_pool, playground