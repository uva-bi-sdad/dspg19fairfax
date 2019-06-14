# read in tables from 2017 5-year ACS estimates (downloaded from American FactFinder)
# save estimates by Fairfax county, Census tract, and Census block group

library(dplyr)

setwd("~/git/DSPG Fairfax/")

# list filenames
filenames <- list.files("~/git/DSPG Fairfax/data/original/ACS2017_5year")
# read in (non-metadata) .csv files
filenames <- filenames[which(unlist( sapply(filenames,function(x){length(grep('with_ann',x)>0)}))==1)]
acs_tables <- list()
for(i in 1:length(filenames)){
  acs_tables[[i]] <- read.csv(paste0('~/git/DSPG Fairfax/data/original/ACS2017_5year/',filenames[i]),stringsAsFactors = F)
}

table( sapply(acs_tables,nrow) )
# 260 = Census tracts
# 909 = Census block groups
# Table B19066 has 486 rows; only some block groups are observed
# (supplemental social security income)

# 14 of the tables do not have block group estimates??
filenames_blockgroup <- filenames[sapply(acs_tables,nrow)==909]
# checked a few of these on AFF -- not available at the block group level
# for now, stick with the 22 tables that are available by blockgroup
acsb <- acs_tables[sapply(acs_tables,nrow)==909]
for(i in 1:length(acsb)){ # use descriptive column names
  names(acsb[[i]]) <- sapply(acsb[[i]][1,],as.character)
  acsb[[i]] <- acsb[[i]][-1,]
  acsb[[i]][,4:ncol(acsb[[i]])] <- sapply(acsb[[i]][,4:ncol(acsb[[i]])], as.numeric)
}

# formatted acs tables
acsf <- list()

# ACS reference for deriving proportion margins-of-error:
# https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2016StatisticalTesting5year.pdf
# p = a/b
# se(p) = 1/b*sqrt(se(a)^2 - p^2*se(b)^2)
# where se(a) = MOE(a)/1.645 for 90% MOEs
# if the value under the square root sign is negative, instead use
# se(p) = 1/b*sqrt(se(a)^2 + p^2*se(b)^2)
# if p=1, se(p) = se(a)/b

# for sums: se(a+b) = sqrt(se(a)^2 + se(b)^2)

# function to compute acs margins of error for proportions
# vector inputs = a, b, se(a), se(b)
acs_moe <- function(a,b,moe_a,moe_b){
  se_a <- moe_a/1.645; se_b <- moe_b/1.645
  se_prop <- rep(NA,length(a))
  for(i in 1:length(se_prop)){
    if(b[i]==0 | is.na(se_a[i]) | is.na(se_b[i]) | is.na(a[i]) | is.na(b[i])){
      se_prop[i] <- NA
    } else{
      tmp <- se_a[i]^2 - (a[i]/b[i])^2*se_b[i]^2
      if(tmp > 0){
        se_prop[i] <- 1/b[i]*sqrt(tmp)
      } else{
        se_prop[i] <- 1/b[i]*sqrt(se_a[i]^2 + (a[i]/b[i])^2*se_b[i]^2)
      }
    }
  }
  moe_prop <- se_prop*1.645
  moe_prop
}

acs_sum_moe <- function(varmat){ 1.645*sqrt(rowSums(varmat/1.645)^2) }

# format estimates to match PUMS definitions
# (go variable-at-a-time, reference PUMS to ACS excel table - 25 variables total)
filenames_blockgroup[1] # B01001; population, sex (SEX), age (AGEP)
acsf[[1]] <- data.frame(geo=acsb[[1]]$Geography,
                        pop_est=acsb[[1]]$'Estimate; Total:',
                        pop_moe=acsb[[1]]$'Margin of Error; Total:')
names(acsf)[[1]] <- "population"

# -----------------------------------------------

acsf[[2]] <- data.frame(geo=acsb[[1]]$Geography,
                        pop_est=acsb[[1]]$'Estimate; Total:',
                        pop_moe=acsb[[1]]$'Margin of Error; Total:',
                        male_est=acsb[[1]]$'Estimate; Male:',
                        male_moe=acsb[[1]]$'Margin of Error; Male:',
                        female_est=acsb[[1]]$'Estimate; Female:',
                        female_moe=acsb[[1]]$'Margin of Error; Female:')
acsf[[2]] <- acsf[[2]] %>% mutate(
  male_pct = male_est/pop_est,
  male_pct_moe = acs_moe(male_est,pop_est,male_moe,pop_moe),
  female_pct = female_est/pop_est,
  female_pct_moe = acs_moe(female_est,pop_est,female_moe,pop_moe)
)
names(acsf)[[2]] <- "sex"

# -----------------------------------------------
filenames_blockgroup[2] # B02001; race (RAC1P)
# RAC1P PUMS definitions: 1=white, 2=black, 6=asian, 3+4+5+7+8+9=other

acsf[[3]] <- data.frame(geo=acsb[[2]]$Geography,
                        pop_est=acsb[[2]]$'Estimate; Total:',
                        pop_moe=acsb[[2]]$'Margin of Error; Total:',
                        white_est=acsb[[2]]$'Estimate; Total: - White alone',
                        white_moe=acsb[[2]]$'Margin of Error; Total: - White alone',
                        black_est=acsb[[2]]$'Estimate; Total: - Black or African American alone',
                        black_moe=acsb[[2]]$'Margin of Error; Total: - Black or African American alone',
                        asian_est=acsb[[2]]$'Estimate; Total: - Asian alone',
                        asian_moe=acsb[[2]]$'Margin of Error; Total: - Asian alone',
                        other_est=acsb[[2]]$'Estimate; Total: - American Indian and Alaska Native alone'+
                          acsb[[2]]$'Estimate; Total: - Native Hawaiian and Other Pacific Islander alone'+
                          acsb[[2]]$'Estimate; Total: - Some other race alone'+
                          acsb[[2]]$'Estimate; Total: - Two or more races:',
                        other_moe=1.645*sqrt( (acsb[[2]]$'Margin of Error; Total: - American Indian and Alaska Native alone'/1.645)^2 +
                                                (acsb[[2]]$'Margin of Error; Total: - Native Hawaiian and Other Pacific Islander alone'/1.645)^2 +
                                                (acsb[[2]]$'Margin of Error; Total: - Some other race alone'/1.645)^2 +
                                                (acsb[[2]]$'Margin of Error; Total: - Two or more races:'/1.645)^2 )
)
acsf[[3]] <- acsf[[3]] %>% mutate(
  white_pct = white_est/pop_est,
  white_pct_moe = acs_moe(white_est,pop_est,white_moe,pop_moe),
  black_pct = black_est/pop_est,
  black_pct_moe = acs_moe(black_est,pop_est,black_moe,pop_moe),
  asian_pct = asian_est/pop_est,
  asian_pct_moe = acs_moe(asian_est,pop_est,asian_moe,pop_moe),
  other_pct = other_est/pop_est,
  other_pct_moe = acs_moe(other_est,pop_est,other_moe,pop_moe)
)
names(acsf)[[3]] <- "race"

# -----------------------------------------------
filenames_blockgroup[3] # B03003; hispanic/latino (HISP)
# HISP PUMS definitions: 1=not spanish/hispanic/latino, 2-20=spanish/hispanic/latino

acsf[[4]] <- data.frame(geo=acsb[[3]]$Geography,
                        pop_est=acsb[[3]]$'Estimate; Total:',
                        pop_moe=acsb[[3]]$'Margin of Error; Total:',
                        hispanic_est=acsb[[3]]$'Estimate; Total: - Hispanic or Latino',
                        hispanic_moe=acsb[[3]]$'Margin of Error; Total: - Hispanic or Latino',
                        nonhispanic_est=acsb[[3]]$'Estimate; Total: - Not Hispanic or Latino',
                        nonhispanic_moe=acsb[[3]]$'Margin of Error; Total: - Not Hispanic or Latino')
acsf[[4]] <- acsf[[4]] %>% mutate(
  hispanic_pct = hispanic_est/pop_est,
  hispanic_pct_moe = acs_moe(hispanic_est,pop_est,hispanic_moe,pop_moe),
  nonhispanic_pct = nonhispanic_est/pop_est,
  nonhispanic_pct_moe = acs_moe(nonhispanic_est,pop_est,nonhispanic_moe,pop_moe)
)
names(acsf)[[4]] <- "hispanic"

# -----------------------------------------------
filenames_blockgroup[4] # B08303; travel time to work (JWMNP)
# JWMNP PUMS definitions: 1 to 200 minutes (top-coded), 'bbb'=NA
# use categories 0-30, 30-60, 60-90, 90+

acsf[[5]] <- data.frame(geo=acsb[[4]]$Geography,
                        pop_est=acsb[[4]]$'Estimate; Total:',
                        pop_moe=acsb[[4]]$'Margin of Error; Total:',
                        min_under30_est=acsb[[4]]$'Estimate; Total: - Less than 5 minutes'+
                          acsb[[4]]$'Estimate; Total: - 5 to 9 minutes'+
                          acsb[[4]]$'Estimate; Total: - 10 to 14 minutes'+
                          acsb[[4]]$'Estimate; Total: - 15 to 19 minutes'+
                          acsb[[4]]$'Estimate; Total: - 20 to 24 minutes'+
                          acsb[[4]]$'Estimate; Total: - 25 to 29 minutes',
                        min_under30_moe=1.645*sqrt( (acsb[[4]]$'Margin of Error; Total: - Less than 5 minutes'/1.645)^2 +
                                                      (acsb[[4]]$'Margin of Error; Total: - 5 to 9 minutes'/1.645)^2 +
                                                      (acsb[[4]]$'Margin of Error; Total: - 10 to 14 minutes'/1.645)^2 +
                                                      (acsb[[4]]$'Margin of Error; Total: - 15 to 19 minutes'/1.645)^2 +
                                                      (acsb[[4]]$'Margin of Error; Total: - 20 to 24 minutes'/1.645)^2 +
                                                    (acsb[[4]]$'Margin of Error; Total: - 25 to 29 minutes'/1.645)^2 ),
                        min_30to60_est=acsb[[4]]$'Estimate; Total: - 30 to 34 minutes'+
                          acsb[[4]]$'Estimate; Total: - 35 to 39 minutes'+
                          acsb[[4]]$'Estimate; Total: - 40 to 44 minutes'+
                          acsb[[4]]$'Estimate; Total: - 45 to 59 minutes',
                        min_30to60_moe=1.645*sqrt( (acsb[[4]]$'Margin of Error; Total: - 30 to 34 minutes'/1.645)^2 +
                                                      (acsb[[4]]$'Margin of Error; Total: - 35 to 39 minutes'/1.645)^2 +
                                                      (acsb[[4]]$'Margin of Error; Total: - 40 to 44 minutes'/1.645)^2 +
                                                      (acsb[[4]]$'Margin of Error; Total: - 45 to 59 minutes'/1.645)^2 ),
                        min_60to90_est=acsb[[4]]$'Estimate; Total: - 60 to 89 minutes',
                        min_60to90_moe=acsb[[4]]$'Margin of Error; Total: - 60 to 89 minutes',
                        min_over90_est=acsb[[4]]$'Estimate; Total: - 90 or more minutes',
                        min_over90_moe=acsb[[4]]$'Margin of Error; Total: - 90 or more minutes'
)
acsf[[5]] <- acsf[[5]] %>% mutate(
  min_under30_pct = min_under30_est/pop_est,
  min_under30_pct_moe = acs_moe(min_under30_est,pop_est,min_under30_moe,pop_moe),
  min_30to60_pct = min_30to60_est/pop_est,
  min_30to60_pct_moe = acs_moe(min_30to60_est,pop_est,min_30to60_moe,pop_moe),
  min_60to90_pct = min_60to90_est/pop_est,
  min_60to90_pct_moe = acs_moe(min_60to90_est,pop_est,min_60to90_moe,pop_moe),
  min_over90_pct = min_over90_est/pop_est,
  min_over90_pct_moe = acs_moe(min_over90_est,pop_est,min_over90_moe,pop_moe)
)
names(acsf)[[5]] <- "travel_time_to_work"

# -----------------------------------------------
filenames_blockgroup[5] # B11001; household type (HHT)
# HHT PUMS definitions: 1=Family, married, 2+3=Family, single parent, 4+5+6+7=Nonfamily

acsf[[6]] <- data.frame(geo=acsb[[5]]$Geography,
                        pop_est=acsb[[5]]$'Estimate; Total:',
                        pop_moe=acsb[[5]]$'Margin of Error; Total:',
                        family_married_est=acsb[[5]]$'Estimate; Family households: - Married-couple family',
                        family_married_moe=acsb[[5]]$'Margin of Error; Family households: - Married-couple family',
                        family_singleparent_est=acsb[[5]]$'Estimate; Family households: - Other family:',
                        family_singleparent_moe=acsb[[5]]$'Margin of Error; Family households: - Other family:',
                        nonfamily_est=acsb[[5]]$'Estimate; Nonfamily households:',
                        nonfamily_moe=acsb[[5]]$'Margin of Error; Nonfamily households:')
acsf[[6]] <- acsf[[6]] %>% mutate(
  family_married_pct = family_married_est/pop_est,
  family_married_pct_moe = acs_moe(family_married_est,pop_est,family_married_moe,pop_moe),
  family_singleparent_pct = family_singleparent_est/pop_est,
  family_singleparent_pct_moe = acs_moe(family_singleparent_est,pop_est,family_singleparent_moe,pop_moe),
  nonfamily_pct = nonfamily_est/pop_est,
  nonfamily_pct_moe = acs_moe(nonfamily_est,pop_est,nonfamily_moe,pop_moe)
)
names(acsf)[6] <- "household_type"

# -----------------------------------------------
filenames_blockgroup[6] # B12001; marital status (MAR)
# MAR PUMS definitions: 1=married, 2+3+4+5=unmarried

acsf[[7]] <- data.frame(geo=acsb[[6]]$Geography,
                        pop_est=acsb[[6]]$'Estimate; Total:',
                        pop_moe=acsb[[6]]$'Margin of Error; Total:',
                        married_est=acsb[[6]]$'Estimate; Male: - Now married:' +
                          acsb[[6]]$'Estimate; Female: - Now married:',
                        married_moe=1.645*sqrt((acsb[[6]]$'Margin of Error; Male: - Now married:'/1.645)^2 +
                                                 (acsb[[6]]$'Margin of Error; Female: - Now married:'/1.645)^2),
                        unmarried_est=acsb[[6]]$'Estimate; Male: - Never married' +
                          acsb[[6]]$'Estimate; Male: - Widowed' +
                          acsb[[6]]$'Estimate; Male: - Divorced' +
                          acsb[[6]]$'Estimate; Female: - Never married' +
                          acsb[[6]]$'Estimate; Female: - Widowed' +
                          acsb[[6]]$'Estimate; Female: - Divorced',
                        unmarried_moe=1.645*sqrt((acsb[[6]]$'Margin of Error; Male: - Never married'/1.645)^2 +
                                                   (acsb[[6]]$'Margin of Error; Male: - Widowed'/1.645)^2 +
                                                   (acsb[[6]]$'Margin of Error; Male: - Divorced'/1.645)^2 +
                                                   (acsb[[6]]$'Margin of Error; Female: - Never married'/1.645)^2 +
                                                   (acsb[[6]]$'Margin of Error; Female: - Widowed'/1.645)^2 +
                                                   (acsb[[6]]$'Margin of Error; Female: - Divorced'/1.645)^2 )
)
acsf[[7]] <- acsf[[7]] %>% mutate(
  married_pct = married_est/pop_est,
  married_pct_moe = acs_moe(married_est,pop_est,married_moe,pop_moe),
  unmarried_pct = unmarried_est/pop_est,
  unmarried_pct_moe = acs_moe(unmarried_est,pop_est,unmarried_moe,pop_moe)
)
names(acsf)[7] <- "marital_status"

# -----------------------------------------------
filenames_blockgroup[7] # B14007; school enrollment (SCH)
# SCH PUMS definitions: 1=not enrolled, 2+3=enrolled

acsf[[8]] <- data.frame(geo=acsb[[7]]$Geography,
                        pop_est=acsb[[7]]$'Estimate; Total:',
                        pop_moe=acsb[[7]]$'Margin of Error; Total:',
                        enrolled_est=acsb[[7]]$'Estimate; Enrolled in school:',
                        enrolled_moe=acsb[[7]]$'Margin of Error; Enrolled in school:',
                        unenrolled_est=acsb[[7]]$'Estimate; Not enrolled in school',
                        unenrolled_moe=acsb[[7]]$'Margin of Error; Not enrolled in school'
)
acsf[[8]] <- acsf[[8]] %>% mutate(
  enrolled_pct = enrolled_est/pop_est,
  enrolled_pct_moe = acs_moe(enrolled_est,pop_est,enrolled_moe,pop_moe),
  unenrolled_pct = unenrolled_est/pop_est,
  unenrolled_pct_moe = acs_moe(unenrolled_est,pop_est,unenrolled_moe,pop_moe)
)
names(acsf)[8] <- "school_enrollment"

# -----------------------------------------------
filenames_blockgroup[8] # B16004; ability to speak english (ENG)
# ENG PUMS definitions: 1=very well, 2=well, 3=not well, 4=not at all

acsf[[9]] <- data.frame(geo=acsb[[8]]$Geography,
                        pop_est=acsb[[8]]$'Estimate; Total:',
                        pop_moe=acsb[[8]]$'Margin of Error; Total:',
                        very_well_est=acsb[[8]]$'Estimate; 5 to 17 years: - Speak only English'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak Spanish: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other Indo-European languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak only English'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Spanish: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other Indo-European languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak only English'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Spanish: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other Indo-European languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "very well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other languages: - Speak English "very well"',
                        very_well_moe=acs_sum_moe(cbind(
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak only English',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Spanish: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other Indo-European languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak only English',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Spanish: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other Indo-European languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak only English',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Spanish: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other Indo-European languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "very well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other languages: - Speak English "very well"'
                        )),
                        well_est=acsb[[8]]$'Estimate; 5 to 17 years: - Speak Spanish: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other Indo-European languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Spanish: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other Indo-European languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Spanish: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other Indo-European languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other languages: - Speak English "well"',
                        well_moe=acs_sum_moe(cbind(
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Spanish: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other Indo-European languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Spanish: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other Indo-European languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Spanish: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other Indo-European languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other languages: - Speak English "well"'
                        )),
                        not_well_est=acsb[[8]]$'Estimate; 5 to 17 years: - Speak Spanish: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other Indo-European languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Spanish: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other Indo-European languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Spanish: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other Indo-European languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "not well"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other languages: - Speak English "not well"',
                        not_well_moe=acs_sum_moe(cbind(
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Spanish: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other Indo-European languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Spanish: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other Indo-European languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Spanish: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other Indo-European languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "not well"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other languages: - Speak English "not well"'
                        )),
                        not_at_all_est=acsb[[8]]$'Estimate; 5 to 17 years: - Speak Spanish: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other Indo-European languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 5 to 17 years: - Speak other languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Spanish: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other Indo-European languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 18 to 64 years: - Speak other languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Spanish: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other Indo-European languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "not at all"'+
                          acsb[[8]]$'Estimate; 65 years and over: - Speak other languages: - Speak English "not at all"',
                        not_at_all_moe=acs_sum_moe(cbind(
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Spanish: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other Indo-European languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak Asian and Pacific Island languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 5 to 17 years: - Speak other languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Spanish: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other Indo-European languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak Asian and Pacific Island languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 18 to 64 years: - Speak other languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Spanish: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other Indo-European languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak Asian and Pacific Island languages: - Speak English "not at all"',
                          acsb[[8]]$'Margin of Error; 65 years and over: - Speak other languages: - Speak English "not at all"'
                        ))
)
acsf[[9]] <- acsf[[9]] %>% mutate(
  very_well_pct = very_well_est/pop_est,
  very_well_pct_moe = acs_moe(very_well_est,pop_est,very_well_moe,pop_moe),
  well_pct = well_est/pop_est,
  well_pct_moe = acs_moe(well_est,pop_est,well_moe,pop_moe),
  not_well_pct = not_well_est/pop_est,
  not_well_pct_moe = acs_moe(not_well_est,pop_est,not_well_moe,pop_moe),
  not_at_all_pct = not_at_all_est/pop_est,
  not_at_all_pct_moe = acs_moe(not_at_all_est,pop_est,not_at_all_moe,pop_moe)
)
names(acsf)[9] <- "english_ability"

# -----------------------------------------------
filenames_blockgroup[9] # B19001; hosuehold income (HINCP)
# HINCP PUMS definition: numerical $ value
# use bins 0-50k, 50-100k, 100-150k, 150-200k, 200k+

acsf[[10]] <- data.frame(geo=acsb[[9]]$Geography,
                        pop_est=acsb[[9]]$'Estimate; Total:',
                        pop_moe=acsb[[9]]$'Margin of Error; Total:',
                        income_under50_est=acsb[[9]]$'Estimate; Total: - Less than $10,000'+
                          acsb[[9]]$'Estimate; Total: - $10,000 to $14,999'+
                          acsb[[9]]$'Estimate; Total: - $15,000 to $19,999'+
                          acsb[[9]]$'Estimate; Total: - $20,000 to $24,999'+
                          acsb[[9]]$'Estimate; Total: - $25,000 to $29,999'+
                          acsb[[9]]$'Estimate; Total: - $30,000 to $34,999'+
                          acsb[[9]]$'Estimate; Total: - $35,000 to $39,999'+
                          acsb[[9]]$'Estimate; Total: - $40,000 to $44,999'+
                          acsb[[9]]$'Estimate; Total: - $45,000 to $49,999',
                        income_under50_moe=acs_sum_moe(cbind(
                          acsb[[9]]$'Margin of Error; Total: - Less than $10,000',
                          acsb[[9]]$'Margin of Error; Total: - $10,000 to $14,999',
                          acsb[[9]]$'Margin of Error; Total: - $15,000 to $19,999',
                          acsb[[9]]$'Margin of Error; Total: - $20,000 to $24,999',
                          acsb[[9]]$'Margin of Error; Total: - $25,000 to $29,999',
                          acsb[[9]]$'Margin of Error; Total: - $30,000 to $34,999',
                          acsb[[9]]$'Margin of Error; Total: - $35,000 to $39,999',
                          acsb[[9]]$'Margin of Error; Total: - $40,000 to $44,999',
                          acsb[[9]]$'Margin of Error; Total: - $45,000 to $49,999'
                        )),
                        income_50to100_est=acsb[[9]]$'Estimate; Total: - $50,000 to $59,999'+
                          acsb[[9]]$'Estimate; Total: - $60,000 to $74,999'+
                          acsb[[9]]$'Estimate; Total: - $75,000 to $99,999',
                        income_50to100_moe=acs_sum_moe(cbind(
                          acsb[[9]]$'Margin of Error; Total: - $50,000 to $59,999',
                          acsb[[9]]$'Margin of Error; Total: - $60,000 to $74,999',
                          acsb[[9]]$'Margin of Error; Total: - $75,000 to $99,999'
                        )),
                        income_100to150_est=acsb[[9]]$'Estimate; Total: - $100,000 to $124,999'+
                          acsb[[9]]$'Estimate; Total: - $125,000 to $149,999',
                        income_100to150_moe=acs_sum_moe(cbind(
                          acsb[[9]]$'Margin of Error; Total: - $100,000 to $124,999',
                          acsb[[9]]$'Margin of Error; Total: - $150,000 to $199,999'
                        )),
                        income_150to200_est=acsb[[9]]$'Estimate; Total: - $150,000 to $199,999',
                        income_150to200_moe=acsb[[9]]$'Margin of Error; Total: - $150,000 to $199,999',
                        income_over200_est=acsb[[9]]$'Estimate; Total: - $200,000 or more',
                        income_over200_moe=acsb[[9]]$'Margin of Error; Total: - $200,000 or more'
)
acsf[[10]] <- acsf[[10]] %>% mutate(
  income_under50_pct = income_under50_est/pop_est,
  income_under50_pct_moe = acs_moe(income_under50_est,pop_est,income_under50_moe,pop_moe),
  income_50to100_pct = income_50to100_est/pop_est,
  income_50to100_pct_moe = acs_moe(income_50to100_est,pop_est,income_50to100_moe,pop_moe),
  income_100to150_pct = income_100to150_est/pop_est,
  income_100to150_pct_moe = acs_moe(income_100to150_est,pop_est,income_100to150_moe,pop_moe),
  income_150to200_pct = income_150to200_est/pop_est,
  income_150to200_pct_moe = acs_moe(income_150to200_est,pop_est,income_150to200_moe,pop_moe),
  income_over200_pct = income_over200_est/pop_est,
  income_over200_pct_moe = acs_moe(income_over200_est,pop_est,income_over200_moe,pop_moe)
)
names(acsf)[10] <- "household_income"

# -----------------------------------------------
filenames_blockgroup[10] # B19056; supplemental security income y/n (SSIP)
# SSIP PUMS definition: numerical $ value (0=none)

acsf[[11]] <- data.frame(geo=acsb[[10]]$Geography,
                        pop_est=acsb[[10]]$'Estimate; Total:',
                        pop_moe=acsb[[10]]$'Margin of Error; Total:',
                        ssi_est=acsb[[10]]$'Estimate; Total: - With Supplemental Security Income (SSI)',
                        ssi_moe=acsb[[10]]$'Margin of Error; Total: - With Supplemental Security Income (SSI)',
                        no_ssi_est=acsb[[10]]$'Estimate; Total: - No Supplemental Security Income (SSI)',
                        no_ssi_moe=acsb[[10]]$'Margin of Error; Total: - No Supplemental Security Income (SSI)'
)
acsf[[11]] <- acsf[[11]] %>% mutate(
  ssi_pct = ssi_est/pop_est,
  ssi_pct_moe = acs_moe(ssi_est,pop_est,ssi_moe,pop_moe),
  no_ssi_pct = no_ssi_est/pop_est,
  no_ssi_pct_moe = acs_moe(no_ssi_est,pop_est,no_ssi_moe,pop_moe)
)
names(acsf)[11] <- "supplemental_security_income"

# -----------------------------------------------
filenames_blockgroup[11] # B19057; public assistance income y/n (PAP)
# PAP PUMS definition: numerical $ value (0=none)

acsf[[12]] <- data.frame(geo=acsb[[11]]$Geography,
                         pop_est=acsb[[11]]$'Estimate; Total:',
                         pop_moe=acsb[[11]]$'Margin of Error; Total:',
                         pai_est=acsb[[11]]$'Estimate; Total: - With public assistance income',
                         pai_moe=acsb[[11]]$'Margin of Error; Total: - With public assistance income',
                         no_pai_est=acsb[[11]]$'Estimate; Total: - No public assistance income',
                         no_pai_moe=acsb[[11]]$'Margin of Error; Total: - No public assistance income'
)
acsf[[12]] <- acsf[[12]] %>% mutate(
  pai_pct = pai_est/pop_est,
  pai_pct_moe = acs_moe(pai_est,pop_est,pai_moe,pop_moe),
  no_pai_pct = no_pai_est/pop_est,
  no_pai_pct_moe = acs_moe(no_pai_est,pop_est,no_pai_moe,pop_moe)
)
names(acsf)[12] <- "public_assistance_income"

# -----------------------------------------------
filenames_blockgroup[12] # B23025; employment status (ESR)
# ESR PUMS definition: 1+2=employed, 3=unemployed, 4+5=armed forces, 6=not in labor force

acsf[[13]] <- data.frame(geo=acsb[[12]]$Geography,
                         pop_est=acsb[[12]]$'Estimate; Total:',
                         pop_moe=acsb[[12]]$'Margin of Error; Total:',
                         employed_est=acsb[[12]]$'Estimate; In labor force: - Civilian labor force: - Employed',
                         employed_moe=acsb[[12]]$'Margin of Error; In labor force: - Civilian labor force: - Employed',
                         unemployed_est=acsb[[12]]$'Estimate; In labor force: - Civilian labor force: - Unemployed',
                         unemployed_moe=acsb[[12]]$'Margin of Error; In labor force: - Civilian labor force: - Unemployed',
                         armed_forces_est=acsb[[12]]$'Estimate; In labor force: - Armed Forces',
                         armed_forces_moe=acsb[[12]]$'Margin of Error; In labor force: - Armed Forces',
                         not_in_labor_force_est=acsb[[12]]$'Estimate; Not in labor force',
                         not_in_labor_force_moe=acsb[[12]]$'Margin of Error; Not in labor force'
)
acsf[[13]] <- acsf[[13]] %>% mutate(
  employed_pct = employed_est/pop_est,
  employed_pct_moe = acs_moe(employed_est,pop_est,employed_moe,pop_moe),
  unemployed_pct = unemployed_est/pop_est,
  unemployed_pct_moe = acs_moe(unemployed_est,pop_est,unemployed_moe,pop_moe),
  armed_forces_pct = armed_forces_est/pop_est,
  armed_forces_pct_moe = acs_moe(armed_forces_est,pop_est,armed_forces_moe,pop_moe),
  not_in_labor_force_pct = not_in_labor_force_est/pop_est,
  not_in_labor_force_pct_moe = acs_moe(not_in_labor_force_est,pop_est,not_in_labor_force_moe,pop_moe)
)
names(acsf)[13] <- "employment_status"

# -----------------------------------------------
filenames_blockgroup[13] # B25003; households

acsf[[14]] <- data.frame(geo=acsb[[13]]$Geography,
                        households_est=acsb[[13]]$'Estimate; Total:',
                        households_moe=acsb[[13]]$'Margin of Error; Total:')
names(acsf)[[14]] <- "households"

# -----------------------------------------------
filenames_blockgroup[13] # B25003; tenure (TEN)

acsf[[15]] <- data.frame(geo=acsb[[13]]$Geography,
                         pop_est=acsb[[13]]$'Estimate; Total:',
                         pop_moe=acsb[[13]]$'Margin of Error; Total:',
                         owned_est=acsb[[13]]$'Estimate; Total: - Owner occupied',
                         owned_moe=acsb[[13]]$'Margin of Error; Total: - Owner occupied',
                         rented_est=acsb[[13]]$'Estimate; Total: - Renter occupied',
                         rented_moe=acsb[[13]]$'Margin of Error; Total: - Renter occupied'
)
acsf[[15]] <- acsf[[15]] %>% mutate(
  owned_pct = owned_est/pop_est,
  owned_pct_moe = acs_moe(owned_est,pop_est,owned_moe,pop_moe),
  rented_pct = rented_est/pop_est,
  rented_pct_moe = acs_moe(rented_est,pop_est,rented_moe,pop_moe)
)
names(acsf)[15] <- "tenure"

# -----------------------------------------------
filenames_blockgroup[14] # B25004; vehicles available (VEH)
# VEH PUMS definition: number of vehicles, topcoded at 6
# use bins 0,1,2,3+

acsf[[16]] <- data.frame(geo=acsb[[14]]$Geography,
                         pop_est=acsb[[14]]$'Estimate; Total:',
                         pop_moe=acsb[[14]]$'Margin of Error; Total:',
                         vehicle0_est=acsb[[14]]$`Estimate; Owner occupied: - No vehicle available`+
                           acsb[[14]]$`Estimate; Renter occupied: - No vehicle available`,
                         vehicle0_moe=acs_sum_moe(cbind(
                           acsb[[14]]$`Margin of Error; Owner occupied: - No vehicle available`,
                           acsb[[14]]$`Margin of Error; Renter occupied: - No vehicle available`
                           )),
                         vehicle1_est=acsb[[14]]$`Estimate; Owner occupied: - 1 vehicle available`+
                           acsb[[14]]$`Estimate; Renter occupied: - 1 vehicle available`,
                         vehicle1_moe=acs_sum_moe(cbind(
                           acsb[[14]]$`Margin of Error; Owner occupied: - 1 vehicle available`,
                           acsb[[14]]$`Margin of Error; Renter occupied: - 1 vehicle available`
                         )),
                         vehicle2_est=acsb[[14]]$`Estimate; Owner occupied: - 2 vehicles available`+
                           acsb[[14]]$`Estimate; Renter occupied: - 2 vehicles available`,
                         vehicle2_moe=acs_sum_moe(cbind(
                           acsb[[14]]$`Margin of Error; Owner occupied: - 2 vehicles available`,
                           acsb[[14]]$`Margin of Error; Renter occupied: - 2 vehicles available`
                         )),
                         vehicle3up_est=acsb[[14]]$`Estimate; Owner occupied: - 3 vehicles available`+
                           acsb[[14]]$`Estimate; Renter occupied: - 3 vehicles available`+
                           acsb[[14]]$`Estimate; Owner occupied: - 4 vehicles available`+
                           acsb[[14]]$`Estimate; Renter occupied: - 4 vehicles available`+
                           acsb[[14]]$`Estimate; Owner occupied: - 5 or more vehicles available`+
                           acsb[[14]]$`Estimate; Renter occupied: - 5 or more vehicles available`,
                         vehicle3up_moe=acs_sum_moe(cbind(
                           acsb[[14]]$`Margin of Error; Owner occupied: - 3 vehicles available`,
                           acsb[[14]]$`Margin of Error; Renter occupied: - 3 vehicles available`,
                           acsb[[14]]$`Margin of Error; Owner occupied: - 4 vehicles available`,
                           acsb[[14]]$`Margin of Error; Renter occupied: - 4 vehicles available`,
                           acsb[[14]]$`Margin of Error; Owner occupied: - 5 or more vehicles available`,
                           acsb[[14]]$`Margin of Error; Renter occupied: - 5 or more vehicles available`
                         ))
)
acsf[[16]] <- acsf[[16]] %>% mutate(
  vehicle0_pct = vehicle0_est/pop_est,
  vehicle0_pct_moe = acs_moe(vehicle0_est,pop_est,vehicle0_moe,pop_moe),
  vehicle1_pct = vehicle1_est/pop_est,
  vehicle1_pct_moe = acs_moe(vehicle1_est,pop_est,vehicle1_moe,pop_moe),
  vehicle2_pct = vehicle2_est/pop_est,
  vehicle2_pct_moe = acs_moe(vehicle2_est,pop_est,vehicle2_moe,pop_moe),
  vehicle3up_pct = vehicle3up_est/pop_est,
  vehicle3up_pct_moe = acs_moe(vehicle3up_est,pop_est,vehicle3up_moe,pop_moe)
)
names(acsf)[16] <- "vehicles"

# -----------------------------------------------
filenames_blockgroup[15] # B25063; gross rent (GRNTP)
# GRNTP PUMS definition: $ value (NA=vacant/owned/no rent payment)
# bins to use: $0-500, $500-1000, $1000-1500, $1500-2000, $2000-3000, $3000+
# set the population = renters with cash payments (can't disentangle $0 renters and owners)

acsf[[17]] <- data.frame(geo=acsb[[15]]$Geography,
                         pop_est=acsb[[15]]$'Estimate; With cash rent:',
                         pop_moe=acsb[[15]]$'Margin of Error; With cash rent:',
                         rent0_500_est=acsb[[15]]$`Estimate; With cash rent: - Less than $100`+
                           acsb[[15]]$`Estimate; With cash rent: - $100 to $149`+
                           acsb[[15]]$`Margin of Error; With cash rent: - $150 to $199`+
                           acsb[[15]]$`Estimate; With cash rent: - $200 to $249`+
                           acsb[[15]]$`Estimate; With cash rent: - $250 to $299`+
                           acsb[[15]]$`Estimate; With cash rent: - $300 to $349`+
                           acsb[[15]]$`Estimate; With cash rent: - $350 to $399`+
                           acsb[[15]]$`Estimate; With cash rent: - $400 to $449`+
                           acsb[[15]]$`Estimate; With cash rent: - $450 to $499`,
                         rent0_500_moe=acs_sum_moe(cbind(
                           acsb[[15]]$`Margin of Error; With cash rent: - Less than $100`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $100 to $149`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $150 to $199`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $200 to $249`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $250 to $299`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $300 to $349`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $350 to $399`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $400 to $449`,
                             acsb[[15]]$`Margin of Error; With cash rent: - $450 to $499`
                         )),
                         rent500_1000_est=acsb[[15]]$`Estimate; With cash rent: - $500 to $549`+
                           acsb[[15]]$`Estimate; With cash rent: - $550 to $599`+
                           acsb[[15]]$`Estimate; With cash rent: - $600 to $649`+
                           acsb[[15]]$`Estimate; With cash rent: - $650 to $699`+
                           acsb[[15]]$`Estimate; With cash rent: - $700 to $749`+
                           acsb[[15]]$`Estimate; With cash rent: - $750 to $799`+
                           acsb[[15]]$`Estimate; With cash rent: - $800 to $899`+
                           acsb[[15]]$`Estimate; With cash rent: - $900 to $999`,
                         rent500_1000_moe=acs_sum_moe(cbind(
                           acsb[[15]]$`Margin of Error; With cash rent: - $500 to $549`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $550 to $599`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $600 to $649`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $650 to $699`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $700 to $749`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $750 to $799`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $800 to $899`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $900 to $999`
                         )),
                         rent1000_1500_est=acsb[[15]]$`Estimate; With cash rent: - $1,000 to $1,249`+
                           acsb[[15]]$`Estimate; With cash rent: - $1,250 to $1,499`,
                         rent1000_1500_moe=acs_sum_moe(cbind(
                           acsb[[15]]$`Margin of Error; With cash rent: - $1,000 to $1,249`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $1,250 to $1,499`                         
                         )),
                         rent1500_2000_est=acsb[[15]]$`Estimate; With cash rent: - $1,500 to $1,999`,
                         rent1500_2000_moe=acsb[[15]]$`Margin of Error; With cash rent: - $1,500 to $1,999`,
                         rent2000_3000_est=acsb[[15]]$`Estimate; With cash rent: - $2,000 to $2,499`+
                           acsb[[15]]$`Estimate; With cash rent: - $2,500 to $2,999`,
                         rent2000_3000_moe=acs_sum_moe(cbind(
                           acsb[[15]]$`Margin of Error; With cash rent: - $2,000 to $2,499`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $2,500 to $2,999`                         
                         )),
                         rent3000up_est=acsb[[15]]$`Estimate; With cash rent: - $3,000 to $3,499`+
                           acsb[[15]]$`Estimate; With cash rent: - $3,500 or more`,
                         rent3000up_moe=acs_sum_moe(cbind(
                           acsb[[15]]$`Margin of Error; With cash rent: - $3,000 to $3,499`,
                           acsb[[15]]$`Margin of Error; With cash rent: - $3,500 or more`
                         ))
)
acsf[[17]] <- acsf[[17]] %>% mutate(
  rent0_500_pct = rent0_500_est/pop_est,
  rent0_500_pct_moe = acs_moe(rent0_500_est,pop_est,rent0_500_moe,pop_moe),
  rent500_1000_pct = rent500_1000_est/pop_est,
  rent500_1000_pct_moe = acs_moe(rent500_1000_est,pop_est,rent500_1000_moe,pop_moe),
  rent1000_1500_pct = rent1000_1500_est/pop_est,
  rent1000_1500_pct_moe = acs_moe(rent1000_1500_est,pop_est,rent1000_1500_moe,pop_moe),
  rent1500_2000_pct = rent1500_2000_est/pop_est,
  rent1500_2000_pct_moe = acs_moe(rent1500_2000_est,pop_est,rent1500_2000_moe,pop_moe),
  rent2000_3000_pct = rent2000_3000_est/pop_est,
  rent2000_3000_pct_moe = acs_moe(rent2000_3000_est,pop_est,rent2000_3000_moe,pop_moe),
  rent3000up_pct = rent3000up_est/pop_est,
  rent3000up_pct_moe = acs_moe(rent3000up_est,pop_est,rent3000up_moe,pop_moe)
)
names(acsf)[17] <- "gross_rent"

# -----------------------------------------------
filenames_blockgroup[16] # B25070; gross rent as %of HH income (GRPIP)
# GRPIP PUMS definition: numeric percent, topcoded at 101%
# bins to use: 0-15, 15-30, 30-50, 50+
# some are not computed - bins will not add to 100%

acsf[[18]] <- data.frame(geo=acsb[[16]]$Geography,
                         pop_est=acsb[[16]]$'Estimate; Total:',
                         pop_moe=acsb[[16]]$'Margin of Error; Total:',
                         grpi0_15_est=acsb[[16]]$`Estimate; Total: - Less than 10.0 percent`+
                           acsb[[16]]$`Estimate; Total: - 10.0 to 14.9 percent`,
                         grpi0_15_moe=acs_sum_moe(cbind(
                           acsb[[16]]$`Margin of Error; Total: - Less than 10.0 percent`,
                           acsb[[16]]$`Margin of Error; Total: - 10.0 to 14.9 percent`
                         )),
                         grpi15_30_est=acsb[[16]]$`Estimate; Total: - 15.0 to 19.9 percent`+
                           acsb[[16]]$`Estimate; Total: - 20.0 to 24.9 percent`+
                           acsb[[16]]$`Estimate; Total: - 25.0 to 29.9 percent`,
                         grpi15_30_moe=acs_sum_moe(cbind(
                           acsb[[16]]$`Margin of Error; Total: - 15.0 to 19.9 percent`,
                           acsb[[16]]$`Margin of Error; Total: - 20.0 to 24.9 percent`,
                           acsb[[16]]$`Margin of Error; Total: - 25.0 to 29.9 percent`
                         )),
                         grpi30_50_est=acsb[[16]]$`Estimate; Total: - 30.0 to 34.9 percent`+
                           acsb[[16]]$`Estimate; Total: - 35.0 to 39.9 percent`+
                           acsb[[16]]$`Estimate; Total: - 40.0 to 49.9 percent`,
                         grpi30_50_moe=acs_sum_moe(cbind(
                           acsb[[16]]$`Margin of Error; Total: - 30.0 to 34.9 percent`,
                           acsb[[16]]$`Margin of Error; Total: - 35.0 to 39.9 percent`,
                           acsb[[16]]$`Margin of Error; Total: - 40.0 to 49.9 percent`
                         )),
                         grpi50up_est=acsb[[16]]$`Estimate; Total: - 50.0 percent or more`,
                         grpi50up_moe=acsb[[16]]$`Margin of Error; Total: - 50.0 percent or more`
)
acsf[[18]] <- acsf[[18]] %>% mutate(
  grpi0_15_pct = grpi0_15_est/pop_est,
  grpi0_15_pct_moe = acs_moe(grpi0_15_est,pop_est,grpi0_15_moe,pop_moe),
  grpi15_30_pct = grpi15_30_est/pop_est,
  grpi15_30_pct_moe = acs_moe(grpi15_30_est,pop_est,grpi15_30_moe,pop_moe),
  grpi30_50_pct = grpi30_50_est/pop_est,
  grpi30_50_pct_moe = acs_moe(grpi30_50_est,pop_est,grpi30_50_moe,pop_moe),
  grpi50up_pct = grpi50up_est/pop_est,
  grpi50up_pct_moe = acs_moe(grpi50up_est,pop_est,grpi50up_moe,pop_moe)
)
names(acsf)[18] <- "gross_rent_percent_income"

# -----------------------------------------------
filenames_blockgroup[17] # B25081; mortgage status *not in PUMS - skip*
# -----------------------------------------------
filenames_blockgroup[18] # B25087; selected monthly owner costs (SMP)
# *doesn't agree exactly w/ PUMS definition - skip*
# SMP PUMS definition: numeric $ value
#acsf[[19]] <- data.frame(geo=acsb[[18]]$Geography,
#                         pop_est=acsb[[18]]$'Estimate; Total:',
#                         pop_moe=acsb[[18]]$'Margin of Error; Total:')
#acsf[[19]] <- acsf[[19]] %>% mutate(
#
#)
#names(acsf)[19] <- "selected_monthly_costs"
# -----------------------------------------------
filenames_blockgroup[19] # B25088; median selected monthly owner costs *median - skip*
# -----------------------------------------------
filenames_blockgroup[20] # B27010; health insurance coverage (HICOV)
# HICOV definition: 1=coverage, 2=no health insurance coverage

acsf[[19]] <- data.frame(geo=acsb[[20]]$Geography,
                         pop_est=acsb[[20]]$'Estimate; Total:',
                         pop_moe=acsb[[20]]$'Margin of Error; Total:',
                         hicov_est=acsb[[20]]$'Estimate; Under 19 years: - With one type of health insurance coverage:'+
                           acsb[[20]]$'Estimate; Under 19 years: - With two or more types of health insurance coverage:'+
                           acsb[[20]]$'Estimate; 19 to 34 years: - With one type of health insurance coverage:'+
                           acsb[[20]]$'Estimate; 19 to 34 years: - With two or more types of health insurance coverage:'+
                           acsb[[20]]$'Estimate; 35 to 64 years: - With one type of health insurance coverage:'+
                           acsb[[20]]$'Estimate; 35 to 64 years: - With two or more types of health insurance coverage:'+
                           acsb[[20]]$'Estimate; 65 years and over: - With one type of health insurance coverage:'+
                           acsb[[20]]$'Estimate; 65 years and over: - With two or more types of health insurance coverage:',
                         hicov_moe=acs_sum_moe(cbind(
                           acsb[[20]]$'Margin of Error; Under 19 years: - With one type of health insurance coverage:',
                           acsb[[20]]$'Margin of Error; Under 19 years: - With two or more types of health insurance coverage:',
                           acsb[[20]]$'Margin of Error; 19 to 34 years: - With one type of health insurance coverage:',
                           acsb[[20]]$'Margin of Error; 19 to 34 years: - With two or more types of health insurance coverage:',
                           acsb[[20]]$'Margin of Error; 35 to 64 years: - With one type of health insurance coverage:',
                           acsb[[20]]$'Margin of Error; 35 to 64 years: - With two or more types of health insurance coverage:',
                           acsb[[20]]$'Margin of Error; 65 years and over: - With one type of health insurance coverage:',
                           acsb[[20]]$'Margin of Error; 65 years and over: - With two or more types of health insurance coverage:'
                         )),
                         no_hicov_est=acsb[[20]]$`Estimate; Under 19 years: - No health insurance coverage`+
                           acsb[[20]]$`Estimate; 19 to 34 years: - No health insurance coverage`+
                           acsb[[20]]$`Estimate; 35 to 64 years: - No health insurance coverage`+
                           acsb[[20]]$`Estimate; 65 years and over: - No health insurance coverage`,
                         no_hicov_moe=acs_sum_moe(cbind(
                           acsb[[20]]$`Margin of Error; Under 19 years: - No health insurance coverage`,
                           acsb[[20]]$`Margin of Error; 19 to 34 years: - No health insurance coverage`,
                           acsb[[20]]$`Margin of Error; 35 to 64 years: - No health insurance coverage`,
                           acsb[[20]]$`Margin of Error; 65 years and over: - No health insurance coverage`                           
                         ))
)
acsf[[19]] <- acsf[[19]] %>% mutate(
  hicov_pct = hicov_est/pop_est,
  hicov_pct_moe = acs_moe(hicov_est,pop_est,hicov_moe,pop_moe),
  no_hicov_pct = no_hicov_est/pop_est,
  no_hicov_pct_moe = acs_moe(no_hicov_est,pop_est,no_hicov_moe,pop_moe)
)
names(acsf)[19] <- "health_insurance_coverage"

# -----------------------------------------------
filenames_blockgroup[21] # C16002; household language (HHL)
# HHL PUMS definition: 1=English only, 2=Spanish, 3+4+5=Other

acsf[[20]] <- data.frame(geo=acsb[[21]]$Geography,
                         pop_est=acsb[[21]]$'Estimate; Total:',
                         pop_moe=acsb[[21]]$'Margin of Error; Total:',
                         english_only_est=acsb[[21]]$`Estimate; Total: - English only`,
                         english_only_moe=acsb[[21]]$`Margin of Error; Total: - English only`,
                         spanish_est=acsb[[21]]$'Estimate; Total: - Spanish:',
                         spanish_moe=acsb[[21]]$'Margin of Error; Total: - Spanish:',
                         other_est=acsb[[21]]$'Estimate; Total: - Other Indo-European languages:'+
                           acsb[[21]]$'Estimate; Total: - Asian and Pacific Island languages:'+
                           acsb[[21]]$'Estimate; Total: - Other languages:',
                         other_moe=acs_sum_moe(cbind(
                           acsb[[21]]$'Margin of Error; Total: - Other Indo-European languages:',
                           acsb[[21]]$'Margin of Error; Total: - Asian and Pacific Island languages:',
                           acsb[[21]]$'Margin of Error; Total: - Other languages:'
                         ))
)
acsf[[20]] <- acsf[[20]] %>% mutate(
  english_only_pct = english_only_est/pop_est,
  english_only_pct_moe = acs_moe(english_only_est,pop_est,english_only_moe,pop_moe),
  spanish_pct = spanish_est/pop_est,
  spanish_pct_moe = acs_moe(spanish_est,pop_est,spanish_moe,pop_moe),
  other_pct = other_est/pop_est,
  other_pct_moe = acs_moe(other_est,pop_est,other_moe,pop_moe)
)
names(acsf)[20] <- "household_language"

# -----------------------------------------------
filenames_blockgroup[22] # C17002; income to poverty ratio (POVPIP)
# POVPIP PUMS definition: numeric percent, topcoded at 501%
# bins to use: 0-1 (poverty), 1+ (nonpoverty)

acsf[[21]] <- data.frame(geo=acsb[[22]]$Geography,
                         pop_est=acsb[[22]]$'Estimate; Total:',
                         pop_moe=acsb[[22]]$'Margin of Error; Total:',
                         poverty_est=acsb[[22]]$`Estimate; Total: - Under .50`+
                           acsb[[22]]$`Estimate; Total: - .50 to .99`,
                         poverty_moe=acs_sum_moe(cbind(
                           acsb[[22]]$`Margin of Error; Total: - Under .50`,
                           acsb[[22]]$`Margin of Error; Total: - .50 to .99`
                         )),
                         nonpoverty_est=acsb[[22]]$`Estimate; Total: - 1.00 to 1.24`+
                           acsb[[22]]$`Estimate; Total: - 1.25 to 1.49`+
                           acsb[[22]]$`Estimate; Total: - 1.50 to 1.84`+
                           acsb[[22]]$`Estimate; Total: - 1.85 to 1.99`+
                           acsb[[22]]$`Estimate; Total: - 2.00 and over`,
                         nonpoverty_moe=acs_sum_moe(cbind(
                           acsb[[22]]$`Margin of Error; Total: - 1.00 to 1.24`,
                           acsb[[22]]$`Margin of Error; Total: - 1.25 to 1.49`,
                           acsb[[22]]$`Margin of Error; Total: - 1.50 to 1.84`,
                           acsb[[22]]$`Margin of Error; Total: - 1.85 to 1.99`,
                           acsb[[22]]$`Margin of Error; Total: - 2.00 and over`
                         ))
)
acsf[[21]] <- acsf[[21]] %>% mutate(
  poverty_pct = poverty_est/pop_est,
  poverty_pct_moe = acs_moe(poverty_est,pop_est,poverty_moe,pop_moe),
  nonpoverty_pct = nonpoverty_est/pop_est,
  nonpoverty_pct_moe = acs_moe(nonpoverty_est,pop_est,nonpoverty_moe,pop_moe)
)
names(acsf)[21] <- "poverty"

# -----------------------------------------------
filenames_blockgroup[1] # B01001; age (AGEP)
# granular ACS categories (granular defintions from 0 to 24; then switch to 10 years)

acsf[[22]] <- data.frame(geo=acsb[[1]]$Geography,
                         pop_est=acsb[[1]]$'Estimate; Total:',
                         pop_moe=acsb[[1]]$'Margin of Error; Total:',
                         age0_5_est=acsb[[1]]$`Estimate; Male: - Under 5 years`+
                           acsb[[1]]$`Estimate; Female: - Under 5 years`,
                         age0_5_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - Under 5 years`,
                           acsb[[1]]$`Margin of Error; Female: - Under 5 years`
                         )),
                         age5_9_est=acsb[[1]]$`Estimate; Male: - 5 to 9 years`+
                           acsb[[1]]$`Estimate; Female: - 5 to 9 years`,
                         age5_9_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 5 to 9 years`,
                           acsb[[1]]$`Margin of Error; Female: - 5 to 9 years`
                         )),
                         age10_14_est=acsb[[1]]$`Estimate; Male: - 10 to 14 years`+
                           acsb[[1]]$`Estimate; Female: - 10 to 14 years`,
                         age10_14_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 10 to 14 years`,
                           acsb[[1]]$`Margin of Error; Female: - 10 to 14 years`
                         )),
                         age15_17_est=acsb[[1]]$`Estimate; Male: - 15 to 17 years`+
                           acsb[[1]]$`Estimate; Female: - 15 to 17 years`,
                         age15_17_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 15 to 17 years`,
                           acsb[[1]]$`Margin of Error; Female: - 15 to 17 years`
                         )),
                         age18_19_est=acsb[[1]]$`Estimate; Male: - 18 and 19 years`+
                           acsb[[1]]$`Estimate; Female: - 18 and 19 years`,
                         age18_19_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 18 and 19 years`,
                           acsb[[1]]$`Margin of Error; Female: - 18 and 19 years`
                         )),
                         age20_est=acsb[[1]]$`Estimate; Male: - 20 years`+
                           acsb[[1]]$`Estimate; Female: - 20 years`,
                         age20_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 20 years`,
                           acsb[[1]]$`Margin of Error; Female: - 20 years`
                         )),
                         age21_est=acsb[[1]]$`Estimate; Male: - 21 years`+
                           acsb[[1]]$`Estimate; Female: - 21 years`,
                         age21_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 21 years`,
                           acsb[[1]]$`Margin of Error; Female: - 21 years`
                         )),
                         age22_24_est=acsb[[1]]$`Estimate; Male: - 22 to 24 years`+
                           acsb[[1]]$`Estimate; Female: - 22 to 24 years`,
                         age22_24_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 22 to 24 years`,
                           acsb[[1]]$`Margin of Error; Female: - 22 to 24 years`
                         )),
                         age25_34_est=acsb[[1]]$`Estimate; Male: - 25 to 29 years`+
                           acsb[[1]]$`Estimate; Female: - 25 to 29 years`+
                           acsb[[1]]$`Estimate; Male: - 30 to 34 years`+
                           acsb[[1]]$`Estimate; Female: - 30 to 34 years`,
                         age25_34_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 25 to 29 years`,
                           acsb[[1]]$`Margin of Error; Female: - 25 to 29 years`,
                           acsb[[1]]$`Margin of Error; Male: - 30 to 34 years`,
                           acsb[[1]]$`Margin of Error; Female: - 30 to 34 years`
                         )),
                         age35_44_est=acsb[[1]]$`Estimate; Male: - 35 to 39 years`+
                           acsb[[1]]$`Estimate; Female: - 35 to 39 years`+
                           acsb[[1]]$`Estimate; Male: - 40 to 44 years`+
                           acsb[[1]]$`Estimate; Female: - 40 to 44 years`,
                         age35_44_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 35 to 39 years`,
                           acsb[[1]]$`Margin of Error; Female: - 35 to 39 years`,
                           acsb[[1]]$`Margin of Error; Male: - 40 to 44 years`,
                           acsb[[1]]$`Margin of Error; Female: - 40 to 44 years`
                         )),
                         age45_54_est=acsb[[1]]$`Estimate; Male: - 45 to 49 years`+
                           acsb[[1]]$`Estimate; Female: - 45 to 49 years`+
                           acsb[[1]]$`Estimate; Male: - 50 to 54 years`+
                           acsb[[1]]$`Estimate; Female: - 50 to 54 years`,
                         age45_54_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 45 to 49 years`,
                           acsb[[1]]$`Margin of Error; Female: - 45 to 49 years`,
                           acsb[[1]]$`Margin of Error; Male: - 50 to 54 years`,
                           acsb[[1]]$`Margin of Error; Female: - 50 to 54 years`
                         )),
                         age55_64_est=acsb[[1]]$`Estimate; Male: - 55 to 59 years`+
                           acsb[[1]]$`Estimate; Female: - 55 to 59 years`+                           
                           acsb[[1]]$`Estimate; Male: - 60 and 61 years`+
                           acsb[[1]]$`Estimate; Female: - 60 and 61 years`+
                           acsb[[1]]$`Estimate; Male: - 62 to 64 years`+
                           acsb[[1]]$`Estimate; Female: - 62 to 64 years`,
                         age55_64_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 55 to 59 years`,
                           acsb[[1]]$`Margin of Error; Female: - 55 to 59 years`,                           
                           acsb[[1]]$`Margin of Error; Male: - 60 and 61 years`,
                           acsb[[1]]$`Margin of Error; Female: - 60 and 61 years`,
                           acsb[[1]]$`Margin of Error; Male: - 62 to 64 years`,
                           acsb[[1]]$`Margin of Error; Female: - 62 to 64 years`
                         )),
                         age65up_est=acsb[[1]]$`Estimate; Male: - 65 and 66 years`+
                           acsb[[1]]$`Estimate; Female: - 65 and 66 years`+
                           acsb[[1]]$`Estimate; Male: - 67 to 69 years`+
                           acsb[[1]]$`Estimate; Female: - 67 to 69 years`+
                           acsb[[1]]$`Estimate; Male: - 70 to 74 years`+
                           acsb[[1]]$`Estimate; Female: - 70 to 74 years`+
                           acsb[[1]]$`Estimate; Male: - 75 to 79 years`+
                           acsb[[1]]$`Estimate; Female: - 75 to 79 years`+
                           acsb[[1]]$`Estimate; Male: - 80 to 84 years`+
                           acsb[[1]]$`Estimate; Female: - 80 to 84 years`+
                           acsb[[1]]$`Estimate; Male: - 85 years and over`+
                           acsb[[1]]$`Estimate; Female: - 85 years and over`,
                         age65up_moe=acs_sum_moe(cbind(
                           acsb[[1]]$`Margin of Error; Male: - 65 and 66 years`,
                           acsb[[1]]$`Margin of Error; Female: - 65 and 66 years`,
                           acsb[[1]]$`Margin of Error; Male: - 67 to 69 years`,
                           acsb[[1]]$`Margin of Error; Female: - 67 to 69 years`,
                           acsb[[1]]$`Margin of Error; Male: - 70 to 74 years`,
                           acsb[[1]]$`Margin of Error; Female: - 70 to 74 years`,
                           acsb[[1]]$`Margin of Error; Male: - 75 to 79 years`,
                           acsb[[1]]$`Margin of Error; Female: - 75 to 79 years`,
                           acsb[[1]]$`Margin of Error; Male: - 80 to 84 years`,
                           acsb[[1]]$`Margin of Error; Female: - 80 to 84 years`,
                           acsb[[1]]$`Margin of Error; Male: - 85 years and over`,
                           acsb[[1]]$`Margin of Error; Female: - 85 years and over`
                         ))
)
acsf[[22]] <- acsf[[22]] %>% mutate(
  age0_5_pct = age0_5_est/pop_est,
  age0_5_pct_moe = acs_moe(age0_5_est,pop_est,age0_5_moe,pop_moe),
  age5_9_pct = age5_9_est/pop_est,
  age5_9_pct_moe = acs_moe(age5_9_est,pop_est,age5_9_moe,pop_moe),
  age10_14_pct = age10_14_est/pop_est,
  age10_14_pct_moe = acs_moe(age10_14_est,pop_est,age10_14_moe,pop_moe),
  age15_17_pct = age15_17_est/pop_est,
  age15_17_pct_moe = acs_moe(age15_17_est,pop_est,age15_17_moe,pop_moe),
  age18_19_pct = age18_19_est/pop_est,
  age18_19_pct_moe = acs_moe(age18_19_est,pop_est,age18_19_moe,pop_moe),
  age20_pct = age20_est/pop_est,
  age20_pct_moe = acs_moe(age20_est,pop_est,age20_moe,pop_moe),
  age21_pct = age21_est/pop_est,
  age21_pct_moe = acs_moe(age21_est,pop_est,age21_moe,pop_moe),
  age22_24_pct = age22_24_est/pop_est,
  age22_24_pct_moe = acs_moe(age22_24_est,pop_est,age22_24_moe,pop_moe),
  age25_34_pct = age25_34_est/pop_est,
  age25_34_pct_moe = acs_moe(age25_34_est,pop_est,age25_34_moe,pop_moe),
  age35_44_pct = age35_44_est/pop_est,
  age35_44_pct_moe = acs_moe(age35_44_est,pop_est,age35_44_moe,pop_moe),
  age45_54_pct = age45_54_est/pop_est,
  age45_54_pct_moe = acs_moe(age45_54_est,pop_est,age45_54_moe,pop_moe),
  age55_64_pct = age55_64_est/pop_est,
  age55_64_pct_moe = acs_moe(age55_64_est,pop_est,age55_64_moe,pop_moe),
  age65up_pct = age65up_est/pop_est,
  age65up_pct_moe = acs_moe(age65up_est,pop_est,age65up_moe,pop_moe)
)
names(acsf)[22] <- "age"

# -----------------------------------------------
# save estimates by blockgroup and Census tract
# save estimates in a list by variable (combine list for FFX county, tract, blockgroups)
# save as .RData

save(acsf,file="~/git/DSPG Fairfax/data/working/ACS2017_5yearacs_ffx_blockgroup_tract.RData")

# to do: pick out and highlight the nine 'opportunity zones'
# unlist, combine variables, save as a single .csv?



