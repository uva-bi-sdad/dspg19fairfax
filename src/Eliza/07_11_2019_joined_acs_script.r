library(tidyverse)


acs.1 <- read.csv("./data/working/ACS_final_index/master_census_dataset.csv")
acs.2 <- read.csv("./data/working/ACS_final_index_2/master_dataset_eliza.csv")
acs.joined <- left_join(acs.1, acs.2, by = c("id", "id_type"))
##write.csv(acs.joined, "./data/working/ACS_final_index_2/07_16_2019_joined_acs_final.csv")


