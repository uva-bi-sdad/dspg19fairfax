library(ggplot2)
library(dplyr)
library(readr)
library(raster)
library(maps)
library(sp)
library(sf)
library(ggthemes)
library(gridExtra)
library(grid)
library(ggpubr)
library(stringr)
library(purrr)
library(rgdal)

#
# Prepare --------------------------------------------------------------------------------------------------------
#

# Get data
data.orig <- read_csv("./data/working/Obesogenic_final_data/2019_7_28_obesogenic_final.csv") %>%
  mutate(geography = ifelse(id_type %in% "census_tract", str_split(geography, ",") %>% map_chr(1), geography))

# Join for plotting
data <- left_join(data.orig, 
                  read_csv("./data/working/ACS_final_index/index.csv") %>% 
                    mutate(Geography = str_split(Geography, ",") %>% 
                             map_chr(1)) %>% 
                    rename(geography = Geography, geoid = Id2),
                  by = "geography")

# Version 1: Tracts
test <- merge(tracts(state = "51", county = c("059")),
              data,
              by.x = "GEOID",
              by.y = "geoid")

# Version 2: Another geography
data <- data %>% filter(id_type == "supervisor_district")

test <- merge(readOGR("./data/original/Fairfax_Geographies/Supervisor_Districts/Supervisor_Districts.shp"),
              data,
              by.x = "DISTRICT",
              by.y = "geography")


#
# Plotting --------------------------------------------------------------------------------------------------------
#

# Convert to sf
test <- st_as_sf(test)

# Create variables Sallie wants
test$supermarket <- (1 - test$no_supermarket)
test$swimming_pool <- (1 - test$no_swimming_pool)

# Set some parameters
rng <- range(c(.20, .75)) # Specify range to have the same min and max for both plots

# Plot 
p1 <- ggplot() + geom_sf(data = test, aes(fill = fast_food)) +
  labs(title = "Fast Food Restaurant",
       fill = "Proportion with access") +
  theme_map() +
  theme(title = element_text(size = 12)) +
  scale_fill_gradient2(low = "#f7fbff", mid = "#6baed6", high = "#08306b", #colors in the scale
                       midpoint = mean(rng),    #same midpoint for plots (mean of the range)
                       breaks = seq(.20,.75,0.1), #breaks in the scale bar
                       limits = c(rng[1], rng[2])) #same limits for plots

p2 <- ggplot() + geom_sf(data = test, aes(fill = supermarket)) +
  labs(title = "Supermarket",
       fill = "Proportion with access") +
  theme_map() +
  theme(title = element_text(size = 12)) +
  scale_fill_gradient2(low="#f7fbff", mid="#6baed6", high="#08306b", #colors in the scale
                       midpoint=mean(rng),    #same midpoint for plots (mean of the range)
                       breaks=seq(0.20,.75,0.1), #breaks in the scale bar
                       limits=c(rng[1], rng[2])) #same limits for plots

p3 <- ggplot() + geom_sf(data = test, aes(fill = swimming_pool)) +
  labs(title = "Swimming pool",
       fill = "Proportion with access") +
  theme_map() +
  theme(title = element_text(size = 12)) +
  scale_fill_gradient2(low="#f7fbff", mid="#6baed6", high="#08306b", #colors in the scale
                       midpoint=mean(rng),    #same midpoint for plots (mean of the range)
                       breaks=seq(0.20,.75,0.1), #breaks in the scale bar
                       limits=c(rng[1], rng[2])) #same limits for plots

# Display plots together
grid.arrange(p1, p2, p3, ncol=3, top = textGrob("Proportion of Fairfax County Households with 20 Minute Bus Ride Access to Amenities, \nby Supervisor District\n", gp = gpar(fontsize=16,font=2)))

# Bar plot
ggplot(test, aes(x = reorder(DISTRICT, -no_swimming_pool), y = no_swimming_pool)) + geom_col(fill = "#2b8cbe")  +
  labs(title = "Proportion of Fairfax County households without 20 minute bus ride access to a public swimming pool\nby supervisor district", y = "Proportion without access",
       x = "Supervisor district") +
  theme_classic() +
  theme(title = element_text(size = 14, face="bold"),
        axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        axis.ticks.length = unit(3, "pt")
  )



