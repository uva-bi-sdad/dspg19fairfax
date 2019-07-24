Obesogenic Dimensional Analysis
================
DSPG Business Innovation Team
7/24/2019

1. Read & Join the Obesogenic Data
----------------------------------

Here we read the Fairfax Housing/OSM Binary Isochrone data, aggregate and nest by geography; read, clean, and nest by geography type the American Community Survey data (final Economic Vulnerability); and finally join by geography and write out a final Obesegenic Environment data frame for dimensional/factor analysis (and index construction).

``` r
#Read clean census track id's by manipulating the census tract number for matching join
census.tract.df <- read_csv("./data/working/ACS_final_index_2/index.csv") %>%
                     rename(geoid = Id2, census_tract = Geography) %>%
                     mutate(
                       geoid = census_tract %>% 
                         parse_number() %>% 
                         as.character() %>% 
                         ifelse(str_length(.) == 4, str_c(., ".00"), .) %>%
                         str_split("") %>%
                         map_chr(.x = ., ~.x[-5] %>% str_c(., collapse = ""))
                     )
                     
#Read in the Fairfax/OSM data
ffx.osm.df <- read_rds("./data/working/Fairfax_Housing_OSM_Joined/fairfax_osm_final.RDS") %>%
               mutate(
                 geoid = geoid %>% as.character() %>% str_sub(., 6, nchar(.)-1)
               )

#Bind together (3 observations do not have census tract identified & 3 census tracts not represented)
ffx.osm.df <- bind_rows(left_join(ffx.osm.df %>% filter(!is.na(geoid)), census.tract.df, by = "geoid"),
            ffx.osm.df %>% filter(is.na(geoid))) %>%
            dplyr::select(-parcel_id) %>%
            rename(
              highschool_district = highschool,
              supervisor_district = district
            ) %>%
            gather(key = id_type, 
                   value = geography,
                   c(census_tract, highschool_district, supervisor_district)) %>%
            mutate(id_type = as.factor(id_type)) %>%
            nest(-c(id_type, geography))

#Read in the Economic Vulnerability ACS/Housing Stock data
acs.df     <- read_csv("./data/working/ACS_final_index_2/07_22_2019_joined_acs_final.csv") %>%
  janitor::clean_names() %>%
  mutate(
    id_type   = as.factor(id_type) 
  ) %>%
  dplyr::select(id_type, geography, everything()) %>%
  nest(-c(id_type, geography)) 

#Join together for final Obesogenic Data: ACS, Fairfax/OSM (aggregated by geography, median or mean respectively)
obesity.df <- inner_join(ffx.osm.df, acs.df, by = c("id_type", "geography")) %>%
              rename(ffx_osm_df = data.x, acs_df = data.y) %>%
              unnest(acs_df) %>%
              mutate(
                ffx_osm_df = map(ffx_osm_df,
                                 ~.x %>%
                                   dplyr::select(alcohol:track) %>%
                                   map_df(mean))
              ) %>%
              unnest()

#Check counts
obesity.df %>% 
  group_by(id_type) %>%
  summarise(
    count = n()
  ) %>% knitr::kable()
```

| id\_type             |  count|
|:---------------------|------:|
| census\_tract        |    255|
| highschool\_district |     24|
| supervisor\_district |      9|

``` r
#View Head
head(obesity.df) %>% knitr::kable()
```

| id\_type      | geography                                      |  age\_below\_18|  age\_above\_65|  no\_highschool|   minority|   hispanic|  unmarried|  single\_parent|  limited\_english|  low\_income|    poverty|        ssi|        pai|  no\_insurance|  unemployed|  not\_enrolled|  no\_vehicle|  long\_commute|  no\_water|  no\_sewer|    no\_gas|  median\_house\_value|  year\_built|    alcohol|  convenience|  fast\_food|  gas\_station|  playground|  restaurant|  sports\_center|  supermarket|  swimming\_pool|  team\_sport|      track|
|:--------------|:-----------------------------------------------|---------------:|---------------:|---------------:|----------:|----------:|----------:|---------------:|-----------------:|------------:|----------:|----------:|----------:|--------------:|-----------:|--------------:|------------:|--------------:|----------:|----------:|----------:|---------------------:|------------:|----------:|------------:|-----------:|-------------:|-----------:|-----------:|---------------:|------------:|---------------:|------------:|----------:|
| census\_tract | Census Tract 4319, Fairfax County, Virginia    |       0.2204442|       0.1789680|       0.0584053|  0.2253429|  0.0806662|  0.3016315|       0.0492126|         0.0200138|    0.1811024|  0.0084912|  0.0206693|  0.0000000|      0.0127133|   0.0276035|      0.7210682|    0.0000000|      0.7137809|  0.0056926|  0.0018975|  0.2368672|                611015|         1979|  0.0000000|    0.0000000|   0.2846300|     0.0000000|   0.1688805|   0.0000000|       0.0000000|    0.2865275|       0.0256167|    0.5275142|  0.2789374|
| census\_tract | Census Tract 4913.01, Fairfax County, Virginia |       0.2685956|       0.0713798|       0.1292689|  0.4484782|  0.2133206|  0.4267754|       0.1865385|         0.1311428|    0.5278846|  0.0860260|  0.0182692|  0.0000000|      0.2147844|   0.0295554|      0.6631609|    0.0269231|      0.5578050|  0.0476190|  0.0415991|  0.2445415|                404100|         1992|  0.0000000|    0.1927646|   0.2786177|     0.1349892|   0.5799136|   0.0982721|       0.0000000|    0.1079914|       0.6533477|    0.6376890|  0.0000000|
| census\_tract | Census Tract 4405.01, Fairfax County, Virginia |       0.2497147|       0.1926588|       0.0673870|  0.2900342|  0.1369342|  0.4062135|       0.1626168|         0.0834334|    0.2704050|  0.0434286|  0.0317757|  0.0161994|      0.0903499|   0.0540541|      0.7058014|    0.0099688|      0.5692049|  0.0054512|  0.0272232|  0.2733333|                571140|         1968|  0.1015719|    0.0000000|   0.1239420|     0.0350665|   0.5223700|   0.3923821|       0.3996372|    0.3222491|       0.3845224|    0.6958888|  0.1632406|
| census\_tract | Census Tract 4304, Fairfax County, Virginia    |       0.2626639|       0.1373361|       0.0647316|  0.3251898|  0.1456177|  0.3965821|       0.1448763|         0.0820498|    0.3370141|  0.0178054|  0.0167845|  0.0066254|      0.0460804|   0.0165572|      0.6919090|    0.0163428|      0.4712644|  0.0000000|  0.0000000|  0.0000000|                491950|         1963|  0.0000000|    0.0000000|   0.1683893|     0.1218777|   0.8010336|   0.0844100|       0.2571059|    0.3406546|       0.7041344|    0.7958656|  0.0340224|
| census\_tract | Census Tract 4803, Fairfax County, Virginia    |       0.2379027|       0.2261888|       0.0331675|  0.2754149|  0.0232882|  0.2928571|       0.0799652|         0.0412763|    0.1599305|  0.0409985|  0.0247718|  0.0000000|      0.0236001|   0.0124805|      0.7117571|    0.0165146|      0.5421191|  0.3426896|  0.5906433|  0.4310924|               1205030|         1982|  0.0166736|    0.0541892|   0.0225094|     0.0741976|   0.1383910|   0.1137974|       0.0195915|    0.0216757|       0.2146728|    0.2876198|  0.0000000|
| census\_tract | Census Tract 4804.01, Fairfax County, Virginia |       0.3010405|       0.1572254|       0.0193501|  0.2658960|  0.0386127|  0.2445718|       0.0589971|         0.0291848|    0.1430678|  0.0229379|  0.0117994|  0.0000000|      0.0209059|   0.0387409|      0.6698382|    0.0081121|      0.6194842|  0.1922276|  0.9854369|  0.4759916|                867850|         1979|  0.0988935|    0.1037344|   0.1168741|     0.2185339|   0.3852006|   0.2413555|       0.1376210|    0.1479945|       0.1694329|    0.4273859|  0.0000000|

Here we were very meticulous about making sure everything is matched, aggregated, and assigned to the right geographic unit. After joining three seperate data sets, Fairfax Housing, OSM, and ACS; we have our final working Obesogenic Environment data (stored in the `"./data/working/Obesogenic_final_data/"` folder). Note that three observations are missing census tracts but are not missing supervisor or highschool district, while three census tracts are not present at all, thus there are three fewer total observations than in the original ACS units of geography (due to the three missing census tracts, as verified in the table above).
