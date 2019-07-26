library(psych)
library(dplyr)

#
# Prepare data ----------------------------------------------------------------------------------------
#

# Read in data
#Read the data
data <- read_rds("./data/working/Obesogenic_final_data/2019_7_25_obesogenic_final_nested.RDS") %>%
  mutate(
    data = map(data,
               ~.x %>%
                 dplyr::select(-c(geography, minority, unmarried, single_parent, limited_english,
                                  low_income, not_enrolled, no_vehicle, long_commute)))
  ) %>% unnest() %>% na.omit() %>% dplyr::select(-id_type)

datastd <- read_rds("./data/working/Obesogenic_final_data/2019_7_25_obesogenic_final_nested.RDS") %>%
  mutate(
    data = map(data,
               ~.x %>%
                 dplyr::select(-c(minority, unmarried, single_parent, limited_english,
                                  low_income, not_enrolled, no_vehicle, long_commute))),
    data = data %>% map(.x = ., ~na.omit(.x) %>%
                          dplyr::select(-geography) %>%
                          mutate_if(is.numeric, function(x) {
                            (x - mean(x))/sd(x)}))
  ) %>% unnest() %>% dplyr::select(-id_type)

# Center and standardize
datastd <- data.frame(scale(data, center = TRUE, scale = TRUE))
describe(datastd)

# Prepare correlation matrix
cormat <- cor(datastd, use = "na.or.complete")


#
# FA exploratory ----------------------------------------------------------------------------------------
#

# Good model: INTERPRETABLE!
# The root mean square of residuals (RMSR) should be close to 0.
# Root mean square error of approximation (RMSEA) index should be below 0.05. 
# Tucker-Lewis Index (TLI) should be over 0.9.

# Play with rotations and algorithms 

# Run FA / exploratory
fact1 <- fa(r = cormat, nfactors = 4, rotate = "oblimin", fm = "ml")
fact1
print(fact1$loadings, cutoff = 0.3)
fa.diagram(fact1, cut = 0.3)

fact2 <- fa(r = cormat, nfactors = 3, rotate = "varimax", fm = "pa")
fact2
print(fact2$loadings, cutoff = 0.3)
fa.diagram(fact2, cut = 0.5)

fact3 <- fa(r = cormat, nfactors = 2, rotate = "varimax", fm = "pa")
fact3
print(fact3$loadings, cutoff = 0.3)
fa.diagram(fact3, cut = 0.3)

fact4 <- fa(r = cormat, nfactors = 3, rotate = "varimax", fm = "pa")
fact4
print(fact4$loadings, cutoff = 0.3)
fa.diagram(fact4, cut = 0.3)

# Each factor captures a certain amount of the overall variance in the observed variables.
# Eigenvalue = much of the variance of the observed variables a factor explains. A factor with eigenvalue ≥1 explains more variance than a single observed variable.
# A factor loading represents the strength of association between each variable and the latent factor.


#
# FA tentative model ----------------------------------------------------------------------------------------
#

# Select indicators
final <- datastd %>%
  dplyr::select(no_insurance, no_highschool, hispanic, poverty,
                restaurant, fast_food, gas_station, no_supermarket, 
                alcohol, convenience, no_swimming_pool, no_team_sport, no_playground)

finalcormat <- cor(final)

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
factor2 <- final %>% select(restaurant, fast_food, gas_station, no_supermarket, alcohol, convenience)
factor3 <- final %>% select(no_team_sport, no_swimming_pool, no_playground)
  
psych::alpha(factor1) # alpha = 0.94
psych::alpha(factor2) # alpha = 0.92
psych::alpha(factor3) # alpha = 0.81

# First dimension: no_insurance, no_highschool, hispanic, poverty
# Second dimension: restaurant, fast_food, gas_station, supermarket, alcohol, convenience
# Third dimension: team_sport, swimming_pool, playground