
#Libraries
library(tidyverse)
library(janitor) #Also needs library(snakecase) as dependencies
library(viridis)
library(purrr)
library(stringr)
library(rgdal)
library(tigris)
#Setting root directory
knitr::opts_knit$set(echo = TRUE,
                     root.dir = rprojroot::find_rstudio_root_file())


# Census tracts -----------------------------------------------------------

acs.df <- read_csv("./data/working/ACS_joined_estimates/2019_07_16_acs_all_geography.csv")
#acs.tidy.df <- read_csv("./data/working/ACS_joined_estimates/2019_06_24_acs_all_geography_tidy.csv")

#1.Age
age <- acs.df %>% select("id_type","id",starts_with("age")) %>% filter(id_type=="census_tract")
age <- age[, -grep("moe",colnames(age))]
age$SUM = rowSums( age[ sapply(age, is.numeric)] )
age$age_below_18 <- (age$age0_5_est+age$age5_9_est+age$age10_14_est+age$age15_17_est)/age$SUM
age$age_above_65 <- age$age65up_est/age$SUM

#2.Education
education <- acs.df %>% select("id_type","id","less_highschool_est","highschool_ged_est",
                               "bachelors_est","masters_est","professional_school_est",
                               "doctorate_est") %>% filter(id_type=="census_tract")
education$SUM <- rowSums( education[ sapply(education, is.numeric)] )
education$no_highschool <- education$less_highschool_est/education$SUM

#3.Race
race <- acs.df %>% select("id_type","id","white_est","black_est",
                          "asian_est","other_est_y") %>% filter(id_type=="census_tract")
race$SUM <- rowSums( race[ sapply(race, is.numeric)] )
race$minority <- (race$black_est+race$asian_est+race$other_est_y)/race$SUM

#4.Ethnicity
ethnicity <- acs.df %>% select("id_type","id","hispanic_est","nonhispanic_est") %>% filter(id_type=="census_tract")
ethnicity$SUM <- rowSums( ethnicity[ sapply(ethnicity, is.numeric)] )
ethnicity$hispanic <- ethnicity$hispanic_est/ethnicity$SUM

#5.Marital status
maritalstatus <- acs.df %>% select("id_type","id","married_est","unmarried_est") %>% filter(id_type=="census_tract")
maritalstatus$SUM <- rowSums( maritalstatus[ sapply(maritalstatus, is.numeric)] )
maritalstatus$unmarried <- maritalstatus$unmarried_est/maritalstatus$SUM

#6.Single parent household
family <- acs.df %>% select("id_type","id","family_married_est","family_singleparent_est",
                            "nonfamily_est") %>% filter(id_type=="census_tract")
family$SUM <- rowSums( family[ sapply(family, is.numeric)] )
family$single_parent <- family$family_singleparent_est/family$SUM

#7.Language
language <- acs.df %>% select("id_type","id","very_well_est","well_est",
                              "not_well_est","not_at_all_est") %>% filter(id_type=="census_tract")
language$SUM <- rowSums(language[ sapply(language, is.numeric)] )
language$limited_english <- (language$not_well_est+language$not_at_all_est)/language$SUM

#8.Household income
income <- acs.df %>% select("id_type","id","income_under50_est","income_50to100_est",
                            "income_100to150_est","income_150to200_est","income_over200_est") %>%
  filter(id_type=="census_tract")
income$SUM <- rowSums(income[ sapply(income, is.numeric)] )
income$low_income <- (income$income_under50_est+income$income_50to100_est)/income$SUM

#9.Poverty
poverty <- acs.df %>% select("id_type","id","poverty_est","nonpoverty_est") %>% 
  filter(id_type=="census_tract")
poverty$SUM <- poverty$poverty_est+poverty$nonpoverty_est
poverty$poverty <- poverty$poverty_est/poverty$SUM

#10.Supplemental security income
ssi <- acs.df %>% select("id_type","id","ssi_est","no_ssi_est") %>% 
  filter(id_type=="census_tract")
ssi$ssi <- ssi$ssi_est/(ssi$ssi_est+ssi$no_ssi_est)

#11.Public assistance income
pai <- acs.df %>% select("id_type","id","pai_est","no_pai_est") %>% 
  filter(id_type=="census_tract")
pai$pai <- pai$pai_est/(pai$pai_est+pai$no_pai_est)

#12.Health insurance coverage
insurance <- acs.df %>% select("id_type","id","hicov_est","no_hicov_est") %>% 
  filter(id_type=="census_tract")
insurance$no_insurance <- insurance$no_hicov_est/(insurance$hicov_est+insurance$no_hicov_est)

#13.Unemployment rate
employment <- acs.df %>% select("id_type","id","employed_est","unemployed_est",
                                "armed_forces_est","not_in_labor_force_est") %>% 
  filter(id_type=="census_tract")
employment$SUM <- employment$employed_est+employment$unemployed_est+employment$armed_forces_est
employment$unemployed <- employment$unemployed_est/employment$SUM

#14.Unenrolled in school
enrollment <- acs.df %>% select("id_type","id","enrolled_est","unenrolled_est") %>% 
  filter(id_type=="census_tract")
enrollment$not_enrolled <- enrollment$unenrolled_est/(enrollment$enrolled_est+enrollment$unenrolled_est)

#15.Car ownership
vehicle <- acs.df %>% select("id_type","id","vehicle0_est","vehicle1_est","vehicle2_est","vehicle3up_est") %>% 
  filter(id_type=="census_tract")
vehicle$SUM <- rowSums(vehicle[ sapply(vehicle, is.numeric)] )
vehicle$no_vehicle <- vehicle$vehicle0_est/vehicle$SUM

#16.Commute time
commute <-acs.df %>% select("id_type","id","min_under30_est","min_30to60_est",
                            "min_60to90_est","min_over90_est")%>% 
  filter(id_type=="census_tract")
commute$SUM <- rowSums(commute[ sapply(commute, is.numeric)] )
commute$longer_commute <- (commute$min_30to60_est+commute$min_60to90_est+commute$min_over90_est)/commute$SUM

#17.Homeownership
ownership <- acs.df %>% select("id_type","id","owned_est","rented_est") %>% filter(id_type=="census_tract")
ownership$renters <- ownership$rented_est/(ownership$owned_est+ownership$rented_est)

#18.Housing cost burden for renters
costburden <- acs.df %>% select("id_type","id","grpi0_15_est","grpi15_30_est","grpi30_50_est","grpi50up_est") %>%
  filter(id_type=="census_tract")
costburden$SUM <- rowSums(costburden[ sapply(costburden, is.numeric)] )
costburden_burdened <- (costburden$grpi30_50_est+costburden$grpi50up_est)/costburden$SUM
  

acsdf <- cbind.data.frame(age$id,age$age_below_18,age$age_above_65,education$no_highschool,race$minority,
                          ethnicity$hispanic,maritalstatus$unmarried,family$single_parent,
                          language$limited_english,income$low_income,poverty$poverty,ssi$ssi,pai$pai,
                          insurance$no_insurance,employment$unemployed,enrollment$not_enrolled,
                          vehicle$no_vehicle,commute$longer_commute)

#Housing -
# Read in the housing stock data
housing <- read.csv("./data/working/Fairfax_Housing_2018/fairfax_housing_2018_geo.csv")
housing$tract_id <- substr(housing$GEOID, 1, nchar(housing$GEOID)-1) 
index <- read_csv("./data/working/ACS_final_index_2/index.csv")
housing <- merge(housing,index,by.x="tract_id",by.y="Id2",all.x=TRUE)

#1.Water
water <- housing %>% subset(select=c("Geography", "WATER")) %>% group_by(Geography) %>%
  table() %>% as.data.frame() 
water <- spread(water,key = WATER,value = Freq)
water$not_available <- water$`Water not available`/(water$`Water available`+water$`Water connected`+water$`Water not available`)

#2.Sewer
sewer <- housing %>% subset(select=c("Geography", "SEWER")) %>% group_by(Geography) %>%
  table() %>% as.data.frame() 
sewer <- spread(sewer,key = SEWER,value = Freq)
sewer$not_available <- sewer$`Sewer not available`/(sewer$`Sewer available`+sewer$`Sewer connected`+sewer$`Sewer not available`)

#3.Gas
gas <- housing %>% subset(select=c("Geography", "GAS")) %>% group_by(Geography) %>%
  table() %>% as.data.frame() 
gas <- spread(gas,key = GAS,value = Freq)
gas$not_available <- gas$`Gas not available`/(gas$`Gas available`+gas$`Gas connected`+gas$`Gas not available`)

#4.Housing value
value <- housing %>% subset(select=c("Geography", "VALUE_TOTAL","NUM_UNITS")) %>% group_by(Geography) %>% na.omit()
value$weighted_value <- value$VALUE_TOTAL/value$NUM_UNITS
medvalue <- value %>% group_by(Geography)  %>%
  summarise(Median = median(weighted_value), Count = n()) 
#medvalue$norm <- (medvalue$Median-min(medvalue$Median))/(max(medvalue$Median)-min(medvalue$Median))

#5.Building age
year <- housing %>% subset(select=c("Geography", "YEAR_BUILT")) %>%
  group_by(Geography) %>% na.omit() %>%
  summarise(Median = median(YEAR_BUILT), Count = n())
#year$norm <- (year$Median-min(year$Median))/(max(year$Median)-min(year$Median))

housingdf <- Reduce(function(x,y) merge(x,y,by="Geography",all=TRUE) ,list(water,sewer,gas,medvalue,year))
housingdf <- housingdf %>% select("Geography","not_available.x","not_available.y","not_available","Median.x","Median.y")

#Join the master dataset
finaldf <- merge(acsdf,housingdf,by.x="age$id",by.y="Geography",all.x=TRUE)
colnames(finaldf) <-c("Geography","age_below_18","age_above_65","no_highschool","minority","hispanic","unmarried","single_parent","limited_english","low_income","poverty","ssi","pai","no_insurance","unemployed","not_enrolled","no_vehicle","long_commute","no_water","no_sewer","no_gas","median_house_value","year_built")
finaldf$id_type <- "census_tract"
#write.csv(finaldf,"./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv",row.names = FALSE)


# School districts --------------------------------------------------------
acs.df <- read_csv("./data/working/ACS_joined_estimates/2019_07_16_acs_all_geography.csv")

#1.Age
age <- acs.df %>% select("id_type","id",starts_with("age")) %>% filter(id_type=="highschool_district")
age <- age[, -grep("moe",colnames(age))]
age$SUM = rowSums( age[ sapply(age, is.numeric)] )
age$age_below_18 <- (age$age0_5_est+age$age5_9_est+age$age10_14_est+age$age15_17_est)/age$SUM
age$age_above_65 <- age$age65up_est/age$SUM

#2.Education
education <- acs.df %>% select("id_type","id","less_highschool_est","highschool_ged_est",
                               "bachelors_est","masters_est","professional_school_est",
                               "doctorate_est") %>% filter(id_type=="highschool_district")
education$SUM <- rowSums( education[ sapply(education, is.numeric)] )
education$no_highschool <- education$less_highschool_est/education$SUM

#3.Race
race <- acs.df %>% select("id_type","id","white_est","black_est",
                          "asian_est","other_est_y") %>% filter(id_type=="highschool_district")
race$SUM <- rowSums( race[ sapply(race, is.numeric)] )
race$minority <- (race$black_est+race$asian_est+race$other_est_y)/race$SUM

#4.Ethnicity
ethnicity <- acs.df %>% select("id_type","id","hispanic_est","nonhispanic_est") %>% filter(id_type=="highschool_district")
ethnicity$SUM <- rowSums( ethnicity[ sapply(ethnicity, is.numeric)] )
ethnicity$hispanic <- ethnicity$hispanic_est/ethnicity$SUM

#5.Marital status
maritalstatus <- acs.df %>% select("id_type","id","married_est","unmarried_est") %>% filter(id_type=="highschool_district")
maritalstatus$SUM <- rowSums( maritalstatus[ sapply(maritalstatus, is.numeric)] )
maritalstatus$unmarried <- maritalstatus$unmarried_est/maritalstatus$SUM

#6.Single parent household
family <- acs.df %>% select("id_type","id","family_married_est","family_singleparent_est",
                            "nonfamily_est") %>% filter(id_type=="highschool_district")
family$SUM <- rowSums( family[ sapply(family, is.numeric)] )
family$single_parent <- family$family_singleparent_est/family$SUM

#7.Language
language <- acs.df %>% select("id_type","id","very_well_est","well_est",
                              "not_well_est","not_at_all_est") %>% filter(id_type=="highschool_district")
language$SUM <- rowSums(language[ sapply(language, is.numeric)] )
language$limited_english <- (language$not_well_est+language$not_at_all_est)/language$SUM

#8.Household income
income <- acs.df %>% select("id_type","id","income_under50_est","income_50to100_est",
                            "income_100to150_est","income_150to200_est","income_over200_est") %>%
  filter(id_type=="highschool_district")
income$SUM <- rowSums(income[ sapply(income, is.numeric)] )
income$low_income <- (income$income_under50_est+income$income_50to100_est)/income$SUM

#9.Poverty
poverty <- acs.df %>% select("id_type","id","poverty_est","nonpoverty_est") %>% 
  filter(id_type=="highschool_district")
poverty$SUM <- poverty$poverty_est+poverty$nonpoverty_est
poverty$poverty <- poverty$poverty_est/poverty$SUM

#10.Supplemental security income
ssi <- acs.df %>% select("id_type","id","ssi_est","no_ssi_est") %>% 
  filter(id_type=="highschool_district")
ssi$ssi <- ssi$ssi_est/(ssi$ssi_est+ssi$no_ssi_est)

#11.Public assistance income
pai <- acs.df %>% select("id_type","id","pai_est","no_pai_est") %>% 
  filter(id_type=="highschool_district")
pai$pai <- pai$pai_est/(pai$pai_est+pai$no_pai_est)

#12.Health insurance coverage
insurance <- acs.df %>% select("id_type","id","hicov_est","no_hicov_est") %>% 
  filter(id_type=="highschool_district")
insurance$no_insurance <- insurance$no_hicov_est/(insurance$hicov_est+insurance$no_hicov_est)

#13.Unemployment rate
employment <- acs.df %>% select("id_type","id","employed_est","unemployed_est",
                                "armed_forces_est","not_in_labor_force_est") %>% 
  filter(id_type=="highschool_district")
employment$SUM <- employment$employed_est+employment$unemployed_est+employment$armed_forces_est
employment$unemployed <- employment$unemployed_est/employment$SUM

#14.Unenrolled in school
enrollment <- acs.df %>% select("id_type","id","enrolled_est","unenrolled_est") %>% 
  filter(id_type=="highschool_district")
enrollment$not_enrolled <- enrollment$unenrolled_est/(enrollment$enrolled_est+enrollment$unenrolled_est)

#15.Car ownership
vehicle <- acs.df %>% select("id_type","id","vehicle0_est","vehicle1_est","vehicle2_est","vehicle3up_est") %>% 
  filter(id_type=="highschool_district")
vehicle$SUM <- rowSums(vehicle[ sapply(vehicle, is.numeric)] )
vehicle$no_vehicle <- vehicle$vehicle0_est/vehicle$SUM

#16.Commute time
commute <-acs.df %>% select("id_type","id","min_under30_est","min_30to60_est",
                            "min_60to90_est","min_over90_est")%>% 
  filter(id_type=="highschool_district")
commute$SUM <- rowSums(commute[ sapply(commute, is.numeric)] )
commute$longer_commute <- (commute$min_30to60_est+commute$min_60to90_est+commute$min_over90_est)/commute$SUM

#17.Homeownership
ownership <- acs.df %>% select("id_type","id","owned_est","rented_est") %>% filter(id_type=="highschool_district")
ownership$renters <- ownership$rented_est/(ownership$owned_est+ownership$rented_est)

#18.Housing cost burden for renters
costburden <- acs.df %>% select("id_type","id","grpi0_15_est","grpi15_30_est","grpi30_50_est","grpi50up_est") %>%
  filter(id_type=="highschool_district")
costburden$SUM <- rowSums(costburden[ sapply(costburden, is.numeric)] )
costburden_burdened <- (costburden$grpi30_50_est+costburden$grpi50up_est)/costburden$SUM


acsdf <- cbind.data.frame(rep("highschool_district",nrow(age)),age$id,age$age_below_18,age$age_above_65,education$no_highschool,race$minority,
                          ethnicity$hispanic,maritalstatus$unmarried,family$single_parent,
                          language$limited_english,income$low_income,poverty$poverty,ssi$ssi,pai$pai,
                          insurance$no_insurance,employment$unemployed,enrollment$not_enrolled,
                          vehicle$no_vehicle,commute$longer_commute)

#Housing -
# Read in the housing stock data
housing <- read.csv("./data/working/Fairfax_Housing_2018/fairfax_housing_2018_geo.csv")

#1.Water
water <- housing %>% subset(select=c("HIGHSCHOOL", "WATER")) %>% group_by(HIGHSCHOOL) %>%
  table() %>% as.data.frame() 
water <- spread(water,key = WATER,value = Freq)
water$not_available <- water$`Water not available`/(water$`Water available`+water$`Water connected`+water$`Water not available`)

#2.Sewer
sewer <- housing %>% subset(select=c("HIGHSCHOOL", "SEWER")) %>% group_by(HIGHSCHOOL) %>%
  table() %>% as.data.frame() 
sewer <- spread(sewer,key = SEWER,value = Freq)
sewer$not_available <- sewer$`Sewer not available`/(sewer$`Sewer available`+sewer$`Sewer connected`+sewer$`Sewer not available`)

#3.Gas
gas <- housing %>% subset(select=c("HIGHSCHOOL", "GAS")) %>% group_by(HIGHSCHOOL) %>%
  table() %>% as.data.frame() 
gas <- spread(gas,key = GAS,value = Freq)
gas$not_available <- gas$`Gas not available`/(gas$`Gas available`+gas$`Gas connected`+gas$`Gas not available`)

#4.Housing value
value <- housing %>% subset(select=c("HIGHSCHOOL", "VALUE_TOTAL","NUM_UNITS")) %>% group_by(HIGHSCHOOL) %>% na.omit()
value$weighted_value <- value$VALUE_TOTAL/value$NUM_UNITS
medvalue <- value %>% group_by(HIGHSCHOOL)  %>%
  summarise(Median = median(weighted_value), Count = n()) 
#medvalue$norm <- (medvalue$Median-min(medvalue$Median))/(max(medvalue$Median)-min(medvalue$Median))

#5.Building age
year <- housing %>% subset(select=c("HIGHSCHOOL", "YEAR_BUILT")) %>%
  group_by(HIGHSCHOOL) %>% na.omit() %>%
  summarise(Median = median(YEAR_BUILT), Count = n())
#year$norm <- (year$Median-min(year$Median))/(max(year$Median)-min(year$Median))


housingdf <- cbind.data.frame(water$not_available,sewer$not_available,gas$not_available,medvalue$Median,year$Median)

#Join the master dataset
df <- cbind.data.frame(acsdf,housingdf)
colnames(df) <-c("id_type","Geography","age_below_18","age_above_65","no_highschool","minority","hispanic","unmarried","single_parent","limited_english","low_income","poverty","ssi","pai","no_insurance","unemployed","not_enrolled","no_vehicle","long_commute","no_water","no_sewer","no_gas","median_house_value","year_built")

#df <- read.csv("./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv") 
finaldf <- rbind.data.frame(finaldf,df)
#write.csv(finaldf,"./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv")


# Supervisor districts ----------------------------------------------------

acs.df <- read_csv("./data/working/ACS_joined_estimates/2019_07_16_acs_all_geography.csv")

#1.Age
age <- acs.df %>% select("id_type","id",starts_with("age")) %>% filter(id_type=="supervisor_district")
age <- age[, -grep("moe",colnames(age))]
age$SUM = rowSums( age[ sapply(age, is.numeric)] )
age$age_below_18 <- (age$age0_5_est+age$age5_9_est+age$age10_14_est+age$age15_17_est)/age$SUM
age$age_above_65 <- age$age65up_est/age$SUM

#2.Education
education <- acs.df %>% select("id_type","id","less_highschool_est","highschool_ged_est",
                               "bachelors_est","masters_est","professional_school_est",
                               "doctorate_est") %>% filter(id_type=="supervisor_district")
education$SUM <- rowSums( education[ sapply(education, is.numeric)] )
education$no_highschool <- education$less_highschool_est/education$SUM

#3.Race
race <- acs.df %>% select("id_type","id","white_est","black_est",
                          "asian_est","other_est_y") %>% filter(id_type=="supervisor_district")
race$SUM <- rowSums( race[ sapply(race, is.numeric)] )
race$minority <- (race$black_est+race$asian_est+race$other_est_y)/race$SUM

#4.Ethnicity
ethnicity <- acs.df %>% select("id_type","id","hispanic_est","nonhispanic_est") %>% filter(id_type=="supervisor_district")
ethnicity$SUM <- rowSums( ethnicity[ sapply(ethnicity, is.numeric)] )
ethnicity$hispanic <- ethnicity$hispanic_est/ethnicity$SUM

#5.Marital status
maritalstatus <- acs.df %>% select("id_type","id","married_est","unmarried_est") %>% filter(id_type=="supervisor_district")
maritalstatus$SUM <- rowSums( maritalstatus[ sapply(maritalstatus, is.numeric)] )
maritalstatus$unmarried <- maritalstatus$unmarried_est/maritalstatus$SUM

#6.Single parent household
family <- acs.df %>% select("id_type","id","family_married_est","family_singleparent_est",
                            "nonfamily_est") %>% filter(id_type=="supervisor_district")
family$SUM <- rowSums( family[ sapply(family, is.numeric)] )
family$single_parent <- family$family_singleparent_est/family$SUM

#7.Language
language <- acs.df %>% select("id_type","id","very_well_est","well_est",
                              "not_well_est","not_at_all_est") %>% filter(id_type=="supervisor_district")
language$SUM <- rowSums(language[ sapply(language, is.numeric)] )
language$limited_english <- (language$not_well_est+language$not_at_all_est)/language$SUM

#8.Household income
income <- acs.df %>% select("id_type","id","income_under50_est","income_50to100_est",
                            "income_100to150_est","income_150to200_est","income_over200_est") %>%
  filter(id_type=="supervisor_district")
income$SUM <- rowSums(income[ sapply(income, is.numeric)] )
income$low_income <- (income$income_under50_est+income$income_50to100_est)/income$SUM

#9.Poverty
poverty <- acs.df %>% select("id_type","id","poverty_est","nonpoverty_est") %>% 
  filter(id_type=="supervisor_district")
poverty$SUM <- poverty$poverty_est+poverty$nonpoverty_est
poverty$poverty <- poverty$poverty_est/poverty$SUM

#10.Supplemental security income
ssi <- acs.df %>% select("id_type","id","ssi_est","no_ssi_est") %>% 
  filter(id_type=="supervisor_district")
ssi$ssi <- ssi$ssi_est/(ssi$ssi_est+ssi$no_ssi_est)

#11.Public assistance income
pai <- acs.df %>% select("id_type","id","pai_est","no_pai_est") %>% 
  filter(id_type=="supervisor_district")
pai$pai <- pai$pai_est/(pai$pai_est+pai$no_pai_est)

#12.Health insurance coverage
insurance <- acs.df %>% select("id_type","id","hicov_est","no_hicov_est") %>% 
  filter(id_type=="supervisor_district")
insurance$no_insurance <- insurance$no_hicov_est/(insurance$hicov_est+insurance$no_hicov_est)

#13.Unemployment rate
employment <- acs.df %>% select("id_type","id","employed_est","unemployed_est",
                                "armed_forces_est","not_in_labor_force_est") %>% 
  filter(id_type=="supervisor_district")
employment$SUM <- employment$employed_est+employment$unemployed_est+employment$armed_forces_est
employment$unemployed <- employment$unemployed_est/employment$SUM

#14.Unenrolled in school
enrollment <- acs.df %>% select("id_type","id","enrolled_est","unenrolled_est") %>% 
  filter(id_type=="supervisor_district")
enrollment$not_enrolled <- enrollment$unenrolled_est/(enrollment$enrolled_est+enrollment$unenrolled_est)

#15.Car ownership
vehicle <- acs.df %>% select("id_type","id","vehicle0_est","vehicle1_est","vehicle2_est","vehicle3up_est") %>% 
  filter(id_type=="supervisor_district")
vehicle$SUM <- rowSums(vehicle[ sapply(vehicle, is.numeric)] )
vehicle$no_vehicle <- vehicle$vehicle0_est/vehicle$SUM

#16.Commute time
commute <-acs.df %>% select("id_type","id","min_under30_est","min_30to60_est",
                            "min_60to90_est","min_over90_est")%>% 
  filter(id_type=="supervisor_district")
commute$SUM <- rowSums(commute[ sapply(commute, is.numeric)] )
commute$longer_commute <- (commute$min_30to60_est+commute$min_60to90_est+commute$min_over90_est)/commute$SUM

#17.Homeownership
ownership <- acs.df %>% select("id_type","id","owned_est","rented_est") %>% filter(id_type=="supervisor_district")
ownership$renters <- ownership$rented_est/(ownership$owned_est+ownership$rented_est)

#18.Housing cost burden for renters
costburden <- acs.df %>% select("id_type","id","grpi0_15_est","grpi15_30_est","grpi30_50_est","grpi50up_est") %>%
  filter(id_type=="supervisor_district")
costburden$SUM <- rowSums(costburden[ sapply(costburden, is.numeric)] )
costburden_burdened <- (costburden$grpi30_50_est+costburden$grpi50up_est)/costburden$SUM


acsdf <- cbind.data.frame(rep("supervisor_district",nrow(age)),age$id,age$age_below_18,age$age_above_65,education$no_highschool,race$minority,
                          ethnicity$hispanic,maritalstatus$unmarried,family$single_parent,
                          language$limited_english,income$low_income,poverty$poverty,ssi$ssi,pai$pai,
                          insurance$no_insurance,employment$unemployed,enrollment$not_enrolled,
                          vehicle$no_vehicle,commute$longer_commute)

#Housing -
# Read in the housing stock data
housing <- read.csv("./data/working/Fairfax_Housing_2018/fairfax_housing_2018_geo.csv")

#1.Water
water <- housing %>% subset(select=c("DISTRICT", "WATER")) %>% group_by(DISTRICT) %>%
  table() %>% as.data.frame() 
water <- spread(water,key = WATER,value = Freq)
water$not_available <- water$`Water not available`/(water$`Water available`+water$`Water connected`+water$`Water not available`)

#2.Sewer
sewer <- housing %>% subset(select=c("DISTRICT", "SEWER")) %>% group_by(DISTRICT) %>%
  table() %>% as.data.frame() 
sewer <- spread(sewer,key = SEWER,value = Freq)
sewer$not_available <- sewer$`Sewer not available`/(sewer$`Sewer available`+sewer$`Sewer connected`+sewer$`Sewer not available`)

#3.Gas
gas <- housing %>% subset(select=c("DISTRICT", "GAS")) %>% group_by(DISTRICT) %>%
  table() %>% as.data.frame() 
gas <- spread(gas,key = GAS,value = Freq)
gas$not_available <- gas$`Gas not available`/(gas$`Gas available`+gas$`Gas connected`+gas$`Gas not available`)

#4.Housing value
value <- housing %>% subset(select=c("DISTRICT", "VALUE_TOTAL","NUM_UNITS")) %>% group_by(DISTRICT) %>% na.omit()
value$weighted_value <- value$VALUE_TOTAL/value$NUM_UNITS
medvalue <- value %>% group_by(DISTRICT)  %>%
  summarise(Median = median(weighted_value), Count = n()) 
#medvalue$norm <- (medvalue$Median-min(medvalue$Median))/(max(medvalue$Median)-min(medvalue$Median))

#5.Building age
year <- housing %>% subset(select=c("DISTRICT", "YEAR_BUILT")) %>%
  group_by(DISTRICT) %>% na.omit() %>%
  summarise(Median = median(YEAR_BUILT), Count = n())
#year$norm <- (year$Median-min(year$Median))/(max(year$Median)-min(year$Median))


housingdf <- cbind.data.frame(water$not_available,sewer$not_available,gas$not_available,medvalue$Median,year$Median)

#Join the master dataset
df <- cbind.data.frame(acsdf,housingdf)
colnames(df) <-c("id_type","Geography","age_below_18","age_above_65","no_highschool","minority","hispanic","unmarried","single_parent","limited_english","low_income","poverty","ssi","pai","no_insurance","unemployed","not_enrolled","no_vehicle","long_commute","no_water","no_sewer","no_gas","median_house_value","year_built")

#data <- read.csv("./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv") 
finaldf <- rbind.data.frame(finaldf,df)
write.csv(finaldf,"./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv",row.names = FALSE)
