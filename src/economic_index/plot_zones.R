library(tidycensus)
library(viridis)
library(ggthemes)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


# Run obesogenic_index and economic_index to get data

# Economic vulnerability
testfiles <- economic.result.df$shp_files[1]
testfiles <- testfiles[[1]]

head(testfiles@data)
testfiles@data$oppzone <- ifelse((testfiles@data$NAMELSAD == "Census Tract 4154.01" |
                                  testfiles@data$NAMELSAD == "Census Tract 4215" |
                                  testfiles@data$NAMELSAD == "Census Tract 4218" |
                                  testfiles@data$NAMELSAD == "Census Tract 4216" |
                                  testfiles@data$NAMELSAD == "Census Tract 4514" |
                                  testfiles@data$NAMELSAD == "Census Tract 4515.02" |
                                  testfiles@data$NAMELSAD == "Census Tract 4528.01" |
                                  testfiles@data$NAMELSAD == "Census Tract 4810" |
                                  testfiles@data$NAMELSAD == "Census Tract 4821"), 1, 0)
head(testfiles@data)

data <- testfiles@data
shapes <- get_acs(geography = "tract", state = 51, county = 059, variables = "B00001_001", year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
econdata <- merge(shapes, data, by = c("GEOID", "STATEFP", "COUNTYFP", "TRACTCE", "ALAND", "AWATER"))

oppzonedata <- econdata[econdata$oppzone == 1, ]

ggplot() +
  geom_sf(data = econdata, size = 0.2, aes(fill = factor_scores)) + 
  geom_sf(data = oppzonedata, size = 2, aes(color = as.factor(oppzone), fill = factor_scores)) + 
  labs(title = "Fairfax County Opportunity Zones: Economic Vulnerability Index") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_viridis_c("Index Score", option = "magma", direction = -1) +
  scale_color_manual("Opportunity Zone", values = c("1" = "green", "0" = NA))

summary(econdata[econdata$oppzone == 1, ])
summary(econdata[econdata$oppzone == 0, ])

# Obesogenic
testfiles <- obesogenic.result.df$shp_files[1]
testfiles <- testfiles[[1]]

head(testfiles@data)
testfiles@data$oppzone <- ifelse((testfiles@data$NAMELSAD == "Census Tract 4154.01" |
                                    testfiles@data$NAMELSAD == "Census Tract 4215" |
                                    testfiles@data$NAMELSAD == "Census Tract 4218" |
                                    testfiles@data$NAMELSAD == "Census Tract 4216" |
                                    testfiles@data$NAMELSAD == "Census Tract 4514" |
                                    testfiles@data$NAMELSAD == "Census Tract 4515.02" |
                                    testfiles@data$NAMELSAD == "Census Tract 4528.01" |
                                    testfiles@data$NAMELSAD == "Census Tract 4810" |
                                    testfiles@data$NAMELSAD == "Census Tract 4821"), 1, 0)
head(testfiles@data)

data <- testfiles@data
shapes <- get_acs(geography = "tract", state = 51, county = 059, variables = "B00001_001", year = 2017, survey = "acs5", cache_table = TRUE, 
                  output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
obesdata <- merge(shapes, data, by = c("GEOID", "STATEFP", "COUNTYFP", "TRACTCE", "ALAND", "AWATER"))

oppzonedata <- obesdata[obesdata$oppzone == 1, ]

ggplot() +
  geom_sf(data = obesdata, size = 0.2, aes(fill = factor_scores)) + 
  geom_sf(data = oppzonedata, size = 2, aes(color = as.factor(oppzone), fill = factor_scores)) + 
  labs(title = "Fairfax County Opportunity Zones: Obesogenic Environment Exposure Index") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_viridis_c("Index Score", option = "magma", direction = -1) +
  scale_color_manual("Opportunity Zone", values = c("1" = "green", "0" = NA))

summary(obesdata[obesdata$oppzone == 1, ])
summary(obesdata[obesdata$oppzone == 0, ])

