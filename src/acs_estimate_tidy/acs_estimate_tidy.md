ACS Estimate Read and Tidy
================
Quinton Neville
6/24/2019

1. Read ACS Estimate Data (by geography)
========================================

``` r
read_acs <- function(geo_type = "blockgroup") {

#Generate path based on geography type
relative.path <- str_c("./data/working/ACS_estimates/", geo_type)
  
#List files, cast as tibble, map -> nest each data.csv, track data source
acs.file.df <- list.files(path = relative.path) %>%
  enframe() %>%
  rename(data_path = value) %>%
  mutate(
    data_path     = str_c(relative.path, "/", data_path),
    input_files   = map(.x = data_path, ~read_csv(.x)),
    data_source   = str_split(data_path, "/") %>% 
                    map_chr(6) %>%
                    str_remove(".csv")
  ) %>%
  dplyr::select(-data_path)

#Iteratively join each df by GEOID
acs.df     <- acs.file.df$input_files[[1]]  #Pull first data frame

#Join by specific ID (given geo_type)

join.by <- ifelse(geo_type %in% "blockgroup", "GEOID",
                  ifelse(geo_type %in% "district", "DISTRICT",
                         ifelse(geo_type %in% "highschool", "HIGHSCHOOL",
                                "TRACT")))

#Loop
for(i in 2:nrow(acs.file.df)) {
  acs.df <- left_join(acs.df, acs.file.df$input_files[[i]], by = join.by) 
}

#Clean names
acs.df <- acs.df %>%
  janitor::clean_names()

#Not Ideal but ID is always first column, cast factor
#acs.df[ ,1] <- as.factor(acs.df[ ,1])


#Profile Dim, missing ACS 
dim.acs <- dim(acs.df)
missing <- map_dbl(.x = acs.df, ~is.na(.x) %>% mean())
missing <- ifelse(any(missing != 0), "Missing Data Present", "No Missing Data")

#Return List of data, tidy data, dimension, and whether or not there is missing data
##Missing is a check to make sure the read operated correctly (as there is none)

  return(list(data = acs.df, dimension = dim.acs, missing = missing))
}

#Test and view output
##test <- read_acs()
##head(test$data)
##head(test$tidy_data)
##test$dimension  
##test$missing
```

2. Write out CSV's into same files
==================================

``` r
blockgroup.list  <- read_acs("blockgroup")
district.list    <- read_acs("district")
highschool.list  <- read_acs("highschool")
opportunity.list <- read_acs("opportunity")

#Check and verify reads
blockgroup.list
district.list
highschool.list
opportunity.list

#Function to write data with good filepath
write_acs <- function(data, geo_type = "blockgroup") {
  #Generate filepath
  file.path <- str_c(
    "./data/working/ACS_estimates/",
    Sys.Date() %>%
      str_replace_all("-", "_"),
      str_c("_clean_acs_", geo_type, ".csv")
  )
  #Write data with filepath
  write.csv(data, file = file.path)
}

write_acs(blockgroup.list$data, file.path)

#Apply function
write.csv(blockgroup.list$data, file.path)
```

3. Example Read and Tidy New Cleaned Data
=========================================
