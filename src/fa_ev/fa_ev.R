library(psych)
library(dplyr)

#
# Prepare data ----------------------------------------------------------------------------------------
#

# Read in data
data <- read.csv("./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv") %>% select (-c(Geography, id_type)) %>% na.omit()
any(is.na(data))

# Center and standardize
datastd <- data.frame(scale(data, center = TRUE, scale = TRUE))
describe(datastd)

# Prepare correlation matrix
cormat <- cor(datastd)


#
# FA exploratory ----------------------------------------------------------------------------------------
#

# Good model: INTERPRETABLE!
# The root mean square of residuals (RMSR) should be close to 0.
# Root mean square error of approximation (RMSEA) index should be below 0.05. 
# Tucker-Lewis Index (TLI) should be over 0.9.

# Tried nearly every sensible combination of rotation and factoring method. Always the same result. We have 2 dimensions.

# Run FA / exploratory
fact1 <- fa(r = cormat, nfactors = 4, rotate = "oblimin", fm = "ml")
fact1
print(fact1$loadings, cutoff = 0.3)
fa.diagram(fact1, cut = 0.3)

fact2 <- fa(r = cormat, nfactors = 3, rotate = "varimax", fm = "pa")
fact2
print(fact2$loadings, cutoff = 0.3)
fa.diagram(fact2, cut = 0.3)

fact3 <- fa(r = cormat, nfactors = 2, rotate = "varimax", fm = "pa")
fact3
print(fact3$loadings, cutoff = 0.3)
fa.diagram(fact3, cut = 0.3)

# Each factor captures a certain amount of the overall variance in the observed variables.
# Eigenvalue = much of the variance of the observed variables a factor explains. A factor with eigenvalue ≥1 explains more variance than a single observed variable.
# A factor loading represents the strength of association between each variable and the latent factor.


#
# FA tentative model ----------------------------------------------------------------------------------------
#

# Select indicators
final <- datastd %>% select(no_insurance, no_highschool, hispanic, limited_english, poverty, single_parent, no_vehicle, minority, 
                            median_house_value, no_sewer, no_water)
finalcormat <- cor(final)

# FA
finalfact <- fa(r = finalcormat, nfactors = 2, rotate = "varimax", fm = "pa")
finalfact
print(finalfact$loadings, cutoff = 0.3)
fa.diagram(finalfact, cut = 0.3)


#
# Confirm ----------------------------------------------------------------------------------------
#

# Calculate internal consistency
factor1 <- final %>% select(no_insurance, no_highschool, hispanic, limited_english, poverty, single_parent, no_vehicle, minority)
factor2 <- final %>% select(median_house_value, no_sewer, no_water)

psych::alpha(factor1) # alpha = 0.93
psych::alpha(factor2) # alpha = 0.81

# First dimension: no_insurance, no_highschool, hispanic, limited_english, poverty, single_parent, no_vehicle
# Second dimension: median_house_value, no_sewer, no_water