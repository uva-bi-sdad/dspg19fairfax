Pre-PCA
================
VH
7/3/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.0     ✔ purrr   0.2.5
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.1
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
#Setting root directory
knitr::opts_knit$set(echo = TRUE,
                     root.dir = rprojroot::find_rstudio_root_file())
```

``` r
#Read in the ACS wide and ACS long (tidy) data
acs.df <- read_csv("/home/vh5dg/dspg19fairfax/data/working/ACS_joined_estimates/2019_07_08_acs_all_geography.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   id = col_character(),
    ##   id_type = col_character(),
    ##   age0_5_moe = col_double(),
    ##   age5_9_moe = col_double(),
    ##   age10_14_moe = col_double(),
    ##   age15_17_moe = col_double(),
    ##   age18_19_moe = col_double(),
    ##   age20_moe = col_double(),
    ##   age21_moe = col_double(),
    ##   age22_24_moe = col_double(),
    ##   age25_34_moe = col_double(),
    ##   age35_44_moe = col_double(),
    ##   age45_54_moe = col_double(),
    ##   age55_64_moe = col_double(),
    ##   age65up_moe = col_double(),
    ##   less_highschool_moe = col_double(),
    ##   highschool_ged_moe = col_double(),
    ##   employed_est = col_double(),
    ##   very_well_est = col_double(),
    ##   very_well_moe = col_double()
    ##   # ... with 32 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
#acs.tidy.df <- read_csv("/home/vh5dg/dspg19fairfax/data/working/ACS_joined_estimates/2019_06_24_acs_all_geography_tidy.csv")
```

``` r
find_pct <- function(dt){
  # only one id_type
  dt <- dt[, -grep("moe",colnames(dt))]
  dt$SUM = rowSums( dt[ sapply(dt, is.numeric)] )
  dt.pct <- dt %>% 
    mutate_if(is.numeric, funs(round(.*100/SUM,2)))
  colnames(dt.pct) <- gsub("est","pct",colnames(dt.pct))
  dt.pct
}
```

PUBLIC ASSISTANCE INCOME (PAI)

``` r
#1. Data Description  
acs.PAI.df <- acs.df %>% select("id_type","id",contains('pai')) 

#Dimension
PAI.dim  <- acs.PAI.df %>% select(ends_with("est")) %>% dim()

PAI.vars <- str_extract(names(acs.PAI.df), "(?<=no_pai|pai).*(?=_est)") %>% 
  na.omit() %>% 
  unique() %>%
  gsub("_","-",.)

#Missing data
miss.df <- tibble(
  Variable = names(acs.PAI.df),
  Count = acs.PAI.df %>% map_dbl(.x = ., ~is.na(.x) %>% sum()),
  Percentage = acs.PAI.df %>% map_dbl(.x = ., ~is.na(.x) %>% mean())) %>%
  mutate(
    Percentage = as.character(Percentage * 100) %>% str_c(., "%")
  )
```

``` r
acs.PAI.tract = (acs.PAI.df %>% filter(id_type=="census_tract") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## please use list() instead
    ## 
    ##   # Before:
    ##   funs(name = f(.))
    ## 
    ##   # After: 
    ##   list(name = ~ f(.))
    ## This warning is displayed once per session.

``` r
acs.PAI.tract %>%
  knitr::kable() 
```

| id\_type      | id                                             | pai\_pct | no\_pai\_pct | SUM |
|:--------------|:-----------------------------------------------|:---------|:-------------|:----|
| census\_tract | Census Tract 4154.01, Fairfax County, Virginia | 5.05     | 94.95        | 100 |
| census\_tract | Census Tract 4215, Fairfax County, Virginia    | 0.77     | 99.23        | 100 |
| census\_tract | Census Tract 4216, Fairfax County, Virginia    | 0.36     | 99.64        | 100 |
| census\_tract | Census Tract 4218, Fairfax County, Virginia    | 2.85     | 97.15        | 100 |
| census\_tract | Census Tract 4514, Fairfax County, Virginia    | 3.73     | 96.27        | 100 |
| census\_tract | Census Tract 4515.02, Fairfax County, Virginia | 0.31     | 99.69        | 100 |
| census\_tract | Census Tract 4528.01, Fairfax County, Virginia | 1.14     | 98.86        | 100 |
| census\_tract | Census Tract 4810, Fairfax County, Virginia    | 1.27     | 98.73        | 100 |
| census\_tract | Census Tract 4821, Fairfax County, Virginia    | 0        | 100          | 100 |

``` r
PAI_tract <- acs.PAI.tract %>% select(id_type,id,pai_pct) 
```

``` r
acs.PAI.highschool = (acs.PAI.df %>% filter(id_type=="highschool_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.PAI.highschool %>%
  knitr::kable() 
```

| id\_type             | id               | pai\_pct | no\_pai\_pct | SUM |
|:---------------------|:-----------------|:---------|:-------------|:----|
| highschool\_district | ANNANDALE        | 0.94     | 99.06        | 100 |
| highschool\_district | CENTREVILLE      | 0.94     | 99.06        | 100 |
| highschool\_district | CHANTILLY        | 0.91     | 99.09        | 100 |
| highschool\_district | EDISON           | 0.79     | 99.21        | 100 |
| highschool\_district | FAIRFAX          | 1.17     | 98.83        | 100 |
| highschool\_district | FALLS CHURCH     | 1.08     | 98.92        | 100 |
| highschool\_district | HAYFIELD         | 0.88     | 99.12        | 100 |
| highschool\_district | HERNDON          | 1.42     | 98.58        | 100 |
| highschool\_district | JUSTICE          | 1.68     | 98.32        | 100 |
| highschool\_district | LAKE BRADDOCK    | 1.3      | 98.7         | 100 |
| highschool\_district | LANGLEY          | 1.7      | 98.3         | 100 |
| highschool\_district | LEE              | 1.97     | 98.03        | 100 |
| highschool\_district | MADISON          | 2.22     | 97.78        | 100 |
| highschool\_district | MARSHALL         | 0.61     | 99.39        | 100 |
| highschool\_district | MCLEAN           | 0.88     | 99.12        | 100 |
| highschool\_district | MOUNT VERNON     | 1.4      | 98.6         | 100 |
| highschool\_district | OAKTON           | 1.5      | 98.5         | 100 |
| highschool\_district | ROBINSON         | 2.06     | 97.94        | 100 |
| highschool\_district | SOUTH COUNTY     | 1.44     | 98.56        | 100 |
| highschool\_district | SOUTH LAKES      | 0.83     | 99.17        | 100 |
| highschool\_district | WEST POTOMAC     | 1.39     | 98.61        | 100 |
| highschool\_district | WEST SPRINGFIELD | 1.19     | 98.81        | 100 |
| highschool\_district | WESTFIELD        | 1.04     | 98.96        | 100 |
| highschool\_district | WOODSON          | 1.42     | 98.58        | 100 |

``` r
PAI_highschool <- acs.PAI.highschool %>% select(id_type,id,pai_pct) 
```

``` r
acs.PAI.supervisor = (acs.PAI.df %>% filter(id_type=="supervisor_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.PAI.supervisor %>%
  knitr::kable() 
```

| id\_type             | id           | pai\_pct | no\_pai\_pct | SUM |
|:---------------------|:-------------|:---------|:-------------|:----|
| supervisor\_district | BRADDOCK     | 1.27     | 98.73        | 100 |
| supervisor\_district | DRANESVILLE  | 1.23     | 98.77        | 100 |
| supervisor\_district | HUNTER MILL  | 0.97     | 99.03        | 100 |
| supervisor\_district | LEE          | 0.95     | 99.05        | 100 |
| supervisor\_district | MASON        | 1.56     | 98.44        | 100 |
| supervisor\_district | MOUNT VERNON | 1.36     | 98.64        | 100 |
| supervisor\_district | PROVIDENCE   | 1.4      | 98.6         | 100 |
| supervisor\_district | SPRINGFIELD  | 1.53     | 98.47        | 100 |
| supervisor\_district | SULLY        | 1.31     | 98.69        | 100 |

``` r
PAI_supervisor <- acs.PAI.supervisor %>% select(id_type,id,pai_pct) 
```

RECOMBINE PAI

``` r
PAI_proportions = do.call("rbind", list(
  PAI_tract,
  PAI_highschool,
  PAI_supervisor))
```

SUPPLEMENTAL SECURITY INCOME (SSI)

``` r
#1. Data Description  
acs.SSI.df <- acs.df %>% select("id_type","id",contains('ssi_')) 

#Dimension
SSI.dim  <- acs.SSI.df %>% select(ends_with("est")) %>% dim()

SSI.vars <- str_extract(names(acs.SSI.df), "(?<=no_ssi|ssi).*(?=_est)") %>% 
  na.omit() %>% 
  unique() %>%
  gsub("_","-",.)

#Missing data
miss.df <- tibble(
  Variable = names(acs.SSI.df),
  Count = acs.SSI.df %>% map_dbl(.x = ., ~is.na(.x) %>% sum()),
  Percentage = acs.SSI.df %>% map_dbl(.x = ., ~is.na(.x) %>% mean())) %>%
  mutate(
    Percentage = as.character(Percentage * 100) %>% str_c(., "%")
  )
```

``` r
acs.SSI.tract = (acs.SSI.df %>% filter(id_type=="census_tract") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.SSI.tract %>%
  knitr::kable() 
```

| id\_type      | id                                             | ssi\_pct | no\_ssi\_pct | SUM |
|:--------------|:-----------------------------------------------|:---------|:-------------|:----|
| census\_tract | Census Tract 4154.01, Fairfax County, Virginia | 3.84     | 96.16        | 100 |
| census\_tract | Census Tract 4215, Fairfax County, Virginia    | 4.24     | 95.76        | 100 |
| census\_tract | Census Tract 4216, Fairfax County, Virginia    | 3.51     | 96.49        | 100 |
| census\_tract | Census Tract 4218, Fairfax County, Virginia    | 9.64     | 90.36        | 100 |
| census\_tract | Census Tract 4514, Fairfax County, Virginia    | 5.97     | 94.03        | 100 |
| census\_tract | Census Tract 4515.02, Fairfax County, Virginia | 4.3      | 95.7         | 100 |
| census\_tract | Census Tract 4528.01, Fairfax County, Virginia | 1.63     | 98.37        | 100 |
| census\_tract | Census Tract 4810, Fairfax County, Virginia    | 0.94     | 99.06        | 100 |
| census\_tract | Census Tract 4821, Fairfax County, Virginia    | 2.88     | 97.12        | 100 |

``` r
SSI_tract <- acs.SSI.tract %>% select(id_type,id,ssi_pct) 
```

``` r
acs.SSI.highschool = (acs.SSI.df %>% filter(id_type=="highschool_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.SSI.highschool %>%
  knitr::kable() 
```

| id\_type             | id               | ssi\_pct | no\_ssi\_pct | SUM |
|:---------------------|:-----------------|:---------|:-------------|:----|
| highschool\_district | ANNANDALE        | 3.05     | 96.95        | 100 |
| highschool\_district | CENTREVILLE      | 1.89     | 98.11        | 100 |
| highschool\_district | CHANTILLY        | 3.17     | 96.83        | 100 |
| highschool\_district | EDISON           | 1.65     | 98.35        | 100 |
| highschool\_district | FAIRFAX          | 1.91     | 98.09        | 100 |
| highschool\_district | FALLS CHURCH     | 2.24     | 97.76        | 100 |
| highschool\_district | HAYFIELD         | 1.39     | 98.61        | 100 |
| highschool\_district | HERNDON          | 3.96     | 96.04        | 100 |
| highschool\_district | JUSTICE          | 2.89     | 97.11        | 100 |
| highschool\_district | LAKE BRADDOCK    | 2.26     | 97.74        | 100 |
| highschool\_district | LANGLEY          | 3.68     | 96.32        | 100 |
| highschool\_district | LEE              | 4.19     | 95.81        | 100 |
| highschool\_district | MADISON          | 3.19     | 96.81        | 100 |
| highschool\_district | MARSHALL         | 2.56     | 97.44        | 100 |
| highschool\_district | MCLEAN           | 1.94     | 98.06        | 100 |
| highschool\_district | MOUNT VERNON     | 2.42     | 97.58        | 100 |
| highschool\_district | OAKTON           | 2.01     | 97.99        | 100 |
| highschool\_district | ROBINSON         | 2.2      | 97.8         | 100 |
| highschool\_district | SOUTH COUNTY     | 2.43     | 97.57        | 100 |
| highschool\_district | SOUTH LAKES      | 2.23     | 97.77        | 100 |
| highschool\_district | WEST POTOMAC     | 2.9      | 97.1         | 100 |
| highschool\_district | WEST SPRINGFIELD | 1.72     | 98.28        | 100 |
| highschool\_district | WESTFIELD        | 1.88     | 98.12        | 100 |
| highschool\_district | WOODSON          | 2.39     | 97.61        | 100 |

``` r
SSI_highschool <- acs.SSI.highschool %>% select(id_type,id,ssi_pct) 
```

``` r
acs.SSI.supervisor = (acs.SSI.df %>% filter(id_type=="supervisor_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.SSI.supervisor %>%
  knitr::kable() 
```

| id\_type             | id           | ssi\_pct | no\_ssi\_pct | SUM |
|:---------------------|:-------------|:---------|:-------------|:----|
| supervisor\_district | BRADDOCK     | 2.98     | 97.02        | 100 |
| supervisor\_district | DRANESVILLE  | 2.22     | 97.78        | 100 |
| supervisor\_district | HUNTER MILL  | 1.67     | 98.33        | 100 |
| supervisor\_district | LEE          | 2.43     | 97.57        | 100 |
| supervisor\_district | MASON        | 2.49     | 97.51        | 100 |
| supervisor\_district | MOUNT VERNON | 2.4      | 97.6         | 100 |
| supervisor\_district | PROVIDENCE   | 2.04     | 97.96        | 100 |
| supervisor\_district | SPRINGFIELD  | 3.32     | 96.68        | 100 |
| supervisor\_district | SULLY        | 3.1      | 96.9         | 100 |

``` r
SSI_supervisor <- acs.SSI.supervisor %>% select(id_type,id,ssi_pct) 
```

RECOMBINE SSI

``` r
SSI_proportions = do.call("rbind", list(
  SSI_tract,
  SSI_highschool,
  SSI_supervisor))
```

UNENROLLED IN SCHOOL

``` r
#1. Data Description  
acs.unschool.df <- acs.df %>% select("id_type","id",contains('enrolled')) 

#Dimension
unschool.dim  <- acs.unschool.df %>% select(ends_with("est")) %>% dim()

unschool.vars <- str_extract(names(acs.unschool.df), "(?<=unenrolled|enrolled).*(?=_est)") %>% 
  na.omit() %>% 
  unique() %>%
  gsub("_","-",.)

#Missing data
miss.df <- tibble(
  Variable = names(acs.unschool.df),
  Count = acs.unschool.df %>% map_dbl(.x = ., ~is.na(.x) %>% sum()),
  Percentage = acs.unschool.df %>% map_dbl(.x = ., ~is.na(.x) %>% mean())) %>%
  mutate(
    Percentage = as.character(Percentage * 100) %>% str_c(., "%")
  )
```

``` r
acs.unschool.tract = (acs.unschool.df %>% filter(id_type=="census_tract") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.unschool.tract %>%
  knitr::kable() 
```

| id\_type      | id                                             | enrolled\_pct | unenrolled\_pct | SUM |
|:--------------|:-----------------------------------------------|:--------------|:----------------|:----|
| census\_tract | Census Tract 4154.01, Fairfax County, Virginia | 29.86         | 70.14           | 100 |
| census\_tract | Census Tract 4215, Fairfax County, Virginia    | 33.34         | 66.66           | 100 |
| census\_tract | Census Tract 4216, Fairfax County, Virginia    | 25.15         | 74.85           | 100 |
| census\_tract | Census Tract 4218, Fairfax County, Virginia    | 26.71         | 73.29           | 100 |
| census\_tract | Census Tract 4514, Fairfax County, Virginia    | 21.31         | 78.69           | 100 |
| census\_tract | Census Tract 4515.02, Fairfax County, Virginia | 22.49         | 77.51           | 100 |
| census\_tract | Census Tract 4528.01, Fairfax County, Virginia | 26.48         | 73.52           | 100 |
| census\_tract | Census Tract 4810, Fairfax County, Virginia    | 28.27         | 71.73           | 100 |
| census\_tract | Census Tract 4821, Fairfax County, Virginia    | 23.49         | 76.51           | 100 |

``` r
unschool_tract <- acs.unschool.tract %>% select(id_type,id,unenrolled_pct) 
```

``` r
acs.unschool.highschool = (acs.unschool.df %>% filter(id_type=="highschool_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.unschool.highschool %>%
  knitr::kable() 
```

| id\_type             | id               | enrolled\_pct | unenrolled\_pct | SUM |
|:---------------------|:-----------------|:--------------|:----------------|:----|
| highschool\_district | ANNANDALE        | 30.35         | 69.65           | 100 |
| highschool\_district | CENTREVILLE      | 30.25         | 69.75           | 100 |
| highschool\_district | CHANTILLY        | 35.66         | 64.34           | 100 |
| highschool\_district | EDISON           | 29.77         | 70.23           | 100 |
| highschool\_district | FAIRFAX          | 28.83         | 71.17           | 100 |
| highschool\_district | FALLS CHURCH     | 25.71         | 74.29           | 100 |
| highschool\_district | HAYFIELD         | 31.28         | 68.72           | 100 |
| highschool\_district | HERNDON          | 30.25         | 69.75           | 100 |
| highschool\_district | JUSTICE          | 27.76         | 72.24           | 100 |
| highschool\_district | LAKE BRADDOCK    | 29.38         | 70.62           | 100 |
| highschool\_district | LANGLEY          | 23.49         | 76.51           | 100 |
| highschool\_district | LEE              | 27.41         | 72.59           | 100 |
| highschool\_district | MADISON          | 27.51         | 72.49           | 100 |
| highschool\_district | MARSHALL         | 30.11         | 69.89           | 100 |
| highschool\_district | MCLEAN           | 27.4          | 72.6            | 100 |
| highschool\_district | MOUNT VERNON     | 23.96         | 76.04           | 100 |
| highschool\_district | OAKTON           | 29.11         | 70.89           | 100 |
| highschool\_district | ROBINSON         | 28.84         | 71.16           | 100 |
| highschool\_district | SOUTH COUNTY     | 23.76         | 76.24           | 100 |
| highschool\_district | SOUTH LAKES      | 23.75         | 76.25           | 100 |
| highschool\_district | WEST POTOMAC     | 28.72         | 71.28           | 100 |
| highschool\_district | WEST SPRINGFIELD | 25.92         | 74.08           | 100 |
| highschool\_district | WESTFIELD        | 32.54         | 67.46           | 100 |
| highschool\_district | WOODSON          | 28.68         | 71.32           | 100 |

``` r
unschool_highschool <- acs.unschool.highschool %>% select(id_type,id,unenrolled_pct) 
```

``` r
acs.unschool.supervisor = (acs.unschool.df %>% filter(id_type=="supervisor_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.unschool.supervisor %>%
  knitr::kable() 
```

| id\_type             | id           | enrolled\_pct | unenrolled\_pct | SUM |
|:---------------------|:-------------|:--------------|:----------------|:----|
| supervisor\_district | BRADDOCK     | 31.91         | 68.09           | 100 |
| supervisor\_district | DRANESVILLE  | 30.14         | 69.86           | 100 |
| supervisor\_district | HUNTER MILL  | 27.96         | 72.04           | 100 |
| supervisor\_district | LEE          | 28.85         | 71.15           | 100 |
| supervisor\_district | MASON        | 30.04         | 69.96           | 100 |
| supervisor\_district | MOUNT VERNON | 26.08         | 73.92           | 100 |
| supervisor\_district | PROVIDENCE   | 26.14         | 73.86           | 100 |
| supervisor\_district | SPRINGFIELD  | 24.7          | 75.3            | 100 |
| supervisor\_district | SULLY        | 26.72         | 73.28           | 100 |

``` r
unschool_supervisor <- acs.unschool.supervisor %>% select(id_type,id,unenrolled_pct) 
```

RECOMBINE UNENROLED

``` r
unenrolled_proportions = do.call("rbind", list(
  unschool_tract,
  unschool_highschool,
  unschool_supervisor))
```

POVERTY

``` r
#1. Data Description  
acs.poverty.df <- acs.df %>% select("id_type","id",contains('poverty')) 

#Dimension
poverty.dim  <- acs.poverty.df %>% select(ends_with("est")) %>% dim()

poverty.vars <- str_extract(names(acs.poverty.df), "(?<=nonpoverty|poverty).*(?=_est)") %>% 
  na.omit() %>% 
  unique() %>%
  gsub("_","-",.)

#Missing data
miss.df <- tibble(
  Variable = names(acs.poverty.df),
  Count = acs.poverty.df %>% map_dbl(.x = ., ~is.na(.x) %>% sum()),
  Percentage = acs.poverty.df %>% map_dbl(.x = ., ~is.na(.x) %>% mean())) %>%
  mutate(
    Percentage = as.character(Percentage * 100) %>% str_c(., "%")
  )
```

``` r
acs.poverty.tract = (acs.poverty.df %>% filter(id_type=="census_tract") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.poverty.tract %>%
  knitr::kable() 
```

| id\_type      | id                                             | poverty\_pct | nonpoverty\_pct | SUM |
|:--------------|:-----------------------------------------------|:-------------|:----------------|:----|
| census\_tract | Census Tract 4154.01, Fairfax County, Virginia | 7.42         | 92.58           | 100 |
| census\_tract | Census Tract 4215, Fairfax County, Virginia    | 20.24        | 79.76           | 100 |
| census\_tract | Census Tract 4216, Fairfax County, Virginia    | 19.58        | 80.42           | 100 |
| census\_tract | Census Tract 4218, Fairfax County, Virginia    | 14.21        | 85.79           | 100 |
| census\_tract | Census Tract 4514, Fairfax County, Virginia    | 27.45        | 72.55           | 100 |
| census\_tract | Census Tract 4515.02, Fairfax County, Virginia | 9.91         | 90.09           | 100 |
| census\_tract | Census Tract 4528.01, Fairfax County, Virginia | 17.2         | 82.8            | 100 |
| census\_tract | Census Tract 4810, Fairfax County, Virginia    | 9.38         | 90.62           | 100 |
| census\_tract | Census Tract 4821, Fairfax County, Virginia    | 16.6         | 83.4            | 100 |

``` r
poverty_tract <- acs.poverty.tract %>% select(id_type,id,poverty_pct) 
```

``` r
acs.poverty.highschool = (acs.poverty.df %>% filter(id_type=="highschool_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.poverty.highschool %>%
  knitr::kable() 
```

| id\_type             | id               | poverty\_pct | nonpoverty\_pct | SUM |
|:---------------------|:-----------------|:-------------|:----------------|:----|
| highschool\_district | ANNANDALE        | 2.93         | 97.07           | 100 |
| highschool\_district | CENTREVILLE      | 6.4          | 93.6            | 100 |
| highschool\_district | CHANTILLY        | 3.83         | 96.17           | 100 |
| highschool\_district | EDISON           | 3.38         | 96.62           | 100 |
| highschool\_district | FAIRFAX          | 2.72         | 97.28           | 100 |
| highschool\_district | FALLS CHURCH     | 5.11         | 94.89           | 100 |
| highschool\_district | HAYFIELD         | 5.42         | 94.58           | 100 |
| highschool\_district | HERNDON          | 11.02        | 88.98           | 100 |
| highschool\_district | JUSTICE          | 5.17         | 94.83           | 100 |
| highschool\_district | LAKE BRADDOCK    | 4.78         | 95.22           | 100 |
| highschool\_district | LANGLEY          | 15.11        | 84.89           | 100 |
| highschool\_district | LEE              | 10.11        | 89.89           | 100 |
| highschool\_district | MADISON          | 6.12         | 93.88           | 100 |
| highschool\_district | MARSHALL         | 4.7          | 95.3            | 100 |
| highschool\_district | MCLEAN           | 4.16         | 95.84           | 100 |
| highschool\_district | MOUNT VERNON     | 9.43         | 90.57           | 100 |
| highschool\_district | OAKTON           | 4.15         | 95.85           | 100 |
| highschool\_district | ROBINSON         | 8.44         | 91.56           | 100 |
| highschool\_district | SOUTH COUNTY     | 7.4          | 92.6            | 100 |
| highschool\_district | SOUTH LAKES      | 6.44         | 93.56           | 100 |
| highschool\_district | WEST POTOMAC     | 4.24         | 95.76           | 100 |
| highschool\_district | WEST SPRINGFIELD | 5.42         | 94.58           | 100 |
| highschool\_district | WESTFIELD        | 3.97         | 96.03           | 100 |
| highschool\_district | WOODSON          | 7.94         | 92.06           | 100 |

``` r
poverty_highschool <- acs.poverty.highschool %>% select(id_type,id,poverty_pct) 
```

``` r
acs.poverty.supervisor = (acs.poverty.df %>% filter(id_type=="supervisor_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.poverty.supervisor %>%
  knitr::kable() 
```

| id\_type             | id           | poverty\_pct | nonpoverty\_pct | SUM |
|:---------------------|:-------------|:-------------|:----------------|:----|
| supervisor\_district | BRADDOCK     | 5.42         | 94.58           | 100 |
| supervisor\_district | DRANESVILLE  | 5.93         | 94.07           | 100 |
| supervisor\_district | HUNTER MILL  | 4.21         | 95.79           | 100 |
| supervisor\_district | LEE          | 3.74         | 96.26           | 100 |
| supervisor\_district | MASON        | 6.19         | 93.81           | 100 |
| supervisor\_district | MOUNT VERNON | 5.42         | 94.58           | 100 |
| supervisor\_district | PROVIDENCE   | 6.9          | 93.1            | 100 |
| supervisor\_district | SPRINGFIELD  | 12.17        | 87.83           | 100 |
| supervisor\_district | SULLY        | 7.8          | 92.2            | 100 |

``` r
poverty_supervisor <- acs.poverty.supervisor %>% select(id_type,id,poverty_pct) 
```

RECOMBINE POVERTY

``` r
poverty_proportions = do.call("rbind", list(
  poverty_tract,
  poverty_highschool,
  poverty_supervisor))
```

HEALTH INSURANCE COVERAGE

``` r
#1. Data Description  
acs.HICOV.df <- acs.df %>% select("id_type","id",contains('hicov')) 

#Dimension
HICOV.dim  <- acs.HICOV.df %>% select(ends_with("est")) %>% dim()

HICOV.vars <- str_extract(names(acs.HICOV.df), "(?<=nohicov|hicov).*(?=_est)") %>% 
  na.omit() %>% 
  unique() %>%
  gsub("_","-",.)

#Missing data
miss.df <- tibble(
  Variable = names(acs.HICOV.df),
  Count = acs.HICOV.df %>% map_dbl(.x = ., ~is.na(.x) %>% sum()),
  Percentage = acs.HICOV.df %>% map_dbl(.x = ., ~is.na(.x) %>% mean())) %>%
  mutate(
    Percentage = as.character(Percentage * 100) %>% str_c(., "%")
  )
```

``` r
acs.HICOV.tract = (acs.HICOV.df %>% filter(id_type=="census_tract") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.HICOV.tract %>%
  knitr::kable() 
```

| id\_type      | id                                             | hicov\_pct | no\_hicov\_pct | SUM |
|:--------------|:-----------------------------------------------|:-----------|:---------------|:----|
| census\_tract | Census Tract 4154.01, Fairfax County, Virginia | 77.04      | 22.96          | 100 |
| census\_tract | Census Tract 4215, Fairfax County, Virginia    | 73.05      | 26.95          | 100 |
| census\_tract | Census Tract 4216, Fairfax County, Virginia    | 74.68      | 25.32          | 100 |
| census\_tract | Census Tract 4218, Fairfax County, Virginia    | 71.57      | 28.43          | 100 |
| census\_tract | Census Tract 4514, Fairfax County, Virginia    | 69.85      | 30.15          | 100 |
| census\_tract | Census Tract 4515.02, Fairfax County, Virginia | 80.76      | 19.24          | 100 |
| census\_tract | Census Tract 4528.01, Fairfax County, Virginia | 81.34      | 18.66          | 100 |
| census\_tract | Census Tract 4810, Fairfax County, Virginia    | 81.27      | 18.73          | 100 |
| census\_tract | Census Tract 4821, Fairfax County, Virginia    | 87.47      | 12.53          | 100 |

``` r
HICOV_tract <- acs.HICOV.tract %>% select(id_type,id,no_hicov_pct) 
```

``` r
acs.HICOV.highschool = (acs.HICOV.df %>% filter(id_type=="highschool_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.HICOV.highschool %>%
  knitr::kable() 
```

| id\_type             | id               | hicov\_pct | no\_hicov\_pct | SUM |
|:---------------------|:-----------------|:-----------|:---------------|:----|
| highschool\_district | ANNANDALE        | 93.59      | 6.41           | 100 |
| highschool\_district | CENTREVILLE      | 88.28      | 11.72          | 100 |
| highschool\_district | CHANTILLY        | 94.1       | 5.9            | 100 |
| highschool\_district | EDISON           | 96.37      | 3.63           | 100 |
| highschool\_district | FAIRFAX          | 93.91      | 6.09           | 100 |
| highschool\_district | FALLS CHURCH     | 91.98      | 8.02           | 100 |
| highschool\_district | HAYFIELD         | 93.66      | 6.34           | 100 |
| highschool\_district | HERNDON          | 82.98      | 17.02          | 100 |
| highschool\_district | JUSTICE          | 90.19      | 9.81           | 100 |
| highschool\_district | LAKE BRADDOCK    | 93.42      | 6.58           | 100 |
| highschool\_district | LANGLEY          | 83.4       | 16.6           | 100 |
| highschool\_district | LEE              | 78.72      | 21.28          | 100 |
| highschool\_district | MADISON          | 87.93      | 12.07          | 100 |
| highschool\_district | MARSHALL         | 94.73      | 5.27           | 100 |
| highschool\_district | MCLEAN           | 94.84      | 5.16           | 100 |
| highschool\_district | MOUNT VERNON     | 84.44      | 15.56          | 100 |
| highschool\_district | OAKTON           | 95.49      | 4.51           | 100 |
| highschool\_district | ROBINSON         | 86.86      | 13.14          | 100 |
| highschool\_district | SOUTH COUNTY     | 93.34      | 6.66           | 100 |
| highschool\_district | SOUTH LAKES      | 90.25      | 9.75           | 100 |
| highschool\_district | WEST POTOMAC     | 93.55      | 6.45           | 100 |
| highschool\_district | WEST SPRINGFIELD | 89.09      | 10.91          | 100 |
| highschool\_district | WESTFIELD        | 93.48      | 6.52           | 100 |
| highschool\_district | WOODSON          | 90.04      | 9.96           | 100 |

``` r
HICOV_highschool <- acs.HICOV.highschool %>% select(id_type,id,no_hicov_pct) 
```

``` r
acs.HICOV.supervisor = (acs.HICOV.df %>% filter(id_type=="supervisor_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 
  
acs.HICOV.supervisor %>%
  knitr::kable() 
```

| id\_type             | id           | hicov\_pct | no\_hicov\_pct | SUM |
|:---------------------|:-------------|:-----------|:---------------|:----|
| supervisor\_district | BRADDOCK     | 91.23      | 8.77           | 100 |
| supervisor\_district | DRANESVILLE  | 91.04      | 8.96           | 100 |
| supervisor\_district | HUNTER MILL  | 92.28      | 7.72           | 100 |
| supervisor\_district | LEE          | 93.52      | 6.48           | 100 |
| supervisor\_district | MASON        | 91.39      | 8.61           | 100 |
| supervisor\_district | MOUNT VERNON | 94.41      | 5.59           | 100 |
| supervisor\_district | PROVIDENCE   | 90.29      | 9.71           | 100 |
| supervisor\_district | SPRINGFIELD  | 82.32      | 17.68          | 100 |
| supervisor\_district | SULLY        | 87.21      | 12.79          | 100 |

``` r
HICOV_supervisor <- acs.HICOV.supervisor %>% select(id_type,id,no_hicov_pct) 
```

RECOMBINE HICOV

``` r
HICOV_proportions = do.call("rbind", list(
  HICOV_tract,
  HICOV_highschool,
  HICOV_supervisor))
```

COMMUTE TIME

``` r
#1. Data Description  
acs.Commute.df <- acs.df %>% select("id_type","id",contains('min')) 

#Dimension
Commute.dim  <- acs.Commute.df %>% select(ends_with("est")) %>% dim()

Commute.vars <- str_extract(names(acs.Commute.df), "(?<=min).*(?=_est)") %>% 
  na.omit() %>% 
  unique() %>%
  gsub("_","-",.)

#Missing data
miss.df <- tibble(
  Variable = names(acs.Commute.df),
  Count = acs.Commute.df %>% map_dbl(.x = ., ~is.na(.x) %>% sum()),
  Percentage = acs.Commute.df %>% map_dbl(.x = ., ~is.na(.x) %>% mean())) %>%
  mutate(
    Percentage = as.character(Percentage * 100) %>% str_c(., "%")
  )

#Adding moe to the new df. The function below requires it. 
acs.Commute.df$over_30min_commute_est<- (acs.Commute.df$min_30to60_est + acs.Commute.df$min_60to90_est + acs.Commute.df$min_over90_est)
acs.Commute.df$over_30min_commute_moe<- (acs.Commute.df$min_30to60_moe + acs.Commute.df$min_60to90_moe + acs.Commute.df$min_over90_moe)


acs.Commute_adjusted.df = acs.Commute.df %>% select(id_type,id,min_under30_est,min_under30_moe, over_30min_commute_est,over_30min_commute_moe)
```

``` r
acs_commute_tract = (acs.Commute_adjusted.df %>% filter(id_type=="census_tract") %>%
  find_pct()  %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 

acs_commute_tract %>%
knitr::kable()
```

| id\_type      | id                                             | min\_under30\_pct | over\_30min\_commute\_pct | SUM |
|:--------------|:-----------------------------------------------|:------------------|:--------------------------|:----|
| census\_tract | Census Tract 4154.01, Fairfax County, Virginia | 43.35             | 56.65                     | 100 |
| census\_tract | Census Tract 4215, Fairfax County, Virginia    | 38.69             | 61.31                     | 100 |
| census\_tract | Census Tract 4216, Fairfax County, Virginia    | 27.12             | 72.88                     | 100 |
| census\_tract | Census Tract 4218, Fairfax County, Virginia    | 43.41             | 56.59                     | 100 |
| census\_tract | Census Tract 4514, Fairfax County, Virginia    | 48.63             | 51.37                     | 100 |
| census\_tract | Census Tract 4515.02, Fairfax County, Virginia | 56.41             | 43.59                     | 100 |
| census\_tract | Census Tract 4528.01, Fairfax County, Virginia | 52.44             | 47.56                     | 100 |
| census\_tract | Census Tract 4810, Fairfax County, Virginia    | 59.95             | 40.05                     | 100 |
| census\_tract | Census Tract 4821, Fairfax County, Virginia    | 57.31             | 42.69                     | 100 |

``` r
commute_tract <- acs_commute_tract %>% select(id_type,id,over_30min_commute_pct) 
```

``` r
acs_commute_highschool = (acs.Commute_adjusted.df %>% filter(id_type=="highschool_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 

acs_commute_highschool %>%
knitr::kable() 
```

| id\_type             | id               | min\_under30\_pct | over\_30min\_commute\_pct | SUM |
|:---------------------|:-----------------|:------------------|:--------------------------|:----|
| highschool\_district | ANNANDALE        | 40.49             | 59.51                     | 100 |
| highschool\_district | CENTREVILLE      | 43.45             | 56.55                     | 100 |
| highschool\_district | CHANTILLY        | 46.42             | 53.58                     | 100 |
| highschool\_district | EDISON           | 45.03             | 54.97                     | 100 |
| highschool\_district | FAIRFAX          | 37.99             | 62.01                     | 100 |
| highschool\_district | FALLS CHURCH     | 50.92             | 49.08                     | 100 |
| highschool\_district | HAYFIELD         | 50                | 50                        | 100 |
| highschool\_district | HERNDON          | 39.73             | 60.27                     | 100 |
| highschool\_district | JUSTICE          | 53.23             | 46.77                     | 100 |
| highschool\_district | LAKE BRADDOCK    | 43.14             | 56.86                     | 100 |
| highschool\_district | LANGLEY          | 48.56             | 51.44                     | 100 |
| highschool\_district | LEE              | 45.45             | 54.55                     | 100 |
| highschool\_district | MADISON          | 44.81             | 55.19                     | 100 |
| highschool\_district | MARSHALL         | 38.73             | 61.27                     | 100 |
| highschool\_district | MCLEAN           | 50.09             | 49.91                     | 100 |
| highschool\_district | MOUNT VERNON     | 47.79             | 52.21                     | 100 |
| highschool\_district | OAKTON           | 50.95             | 49.05                     | 100 |
| highschool\_district | ROBINSON         | 42.06             | 57.94                     | 100 |
| highschool\_district | SOUTH COUNTY     | 55.84             | 44.16                     | 100 |
| highschool\_district | SOUTH LAKES      | 43.91             | 56.09                     | 100 |
| highschool\_district | WEST POTOMAC     | 39.63             | 60.37                     | 100 |
| highschool\_district | WEST SPRINGFIELD | 58.74             | 41.26                     | 100 |
| highschool\_district | WESTFIELD        | 38.75             | 61.25                     | 100 |
| highschool\_district | WOODSON          | 44.41             | 55.59                     | 100 |

``` r
commute_highschool <- acs_commute_highschool %>% select(id_type,id,over_30min_commute_pct) 
```

``` r
acs_commute_supervisor = (acs.Commute_adjusted.df %>% filter(id_type=="supervisor_district") %>%
  find_pct() %>%
  mutate_if(is.numeric, funs(as.character() %>% str_c(., "")))) 

acs_commute_supervisor %>%
knitr::kable() 
```

| id\_type             | id           | min\_under30\_pct | over\_30min\_commute\_pct | SUM |
|:---------------------|:-------------|:------------------|:--------------------------|:----|
| supervisor\_district | BRADDOCK     | 42.64             | 57.36                     | 100 |
| supervisor\_district | DRANESVILLE  | 47.44             | 52.56                     | 100 |
| supervisor\_district | HUNTER MILL  | 53.68             | 46.32                     | 100 |
| supervisor\_district | LEE          | 40.31             | 59.69                     | 100 |
| supervisor\_district | MASON        | 41.73             | 58.27                     | 100 |
| supervisor\_district | MOUNT VERNON | 54.96             | 45.04                     | 100 |
| supervisor\_district | PROVIDENCE   | 47.73             | 52.27                     | 100 |
| supervisor\_district | SPRINGFIELD  | 47.65             | 52.35                     | 100 |
| supervisor\_district | SULLY        | 40.75             | 59.25                     | 100 |

``` r
commute_supervisor <- acs_commute_supervisor %>% select(id_type,id,over_30min_commute_pct) 
```

RECOMBINE COMMUTE

``` r
commute_proportions = do.call("rbind", list(
  commute_tract,
  commute_highschool,
  commute_supervisor))
```

FINAL MASTER CV

``` r
master_census_dataset = (Reduce(function(x, y) merge(x, y, all=TRUE),list(PAI_proportions, SSI_proportions, unenrolled_proportions, poverty_proportions, HICOV_proportions, commute_proportions))) 
  



master_census_dataset = bind_cols(master_census_dataset %>%
                                    select(c(id, id_type)),
                                  master_census_dataset %>%
                                    select(-c(id,id_type)) %>% 
                                    map_df(., as.numeric))


write.csv(master_census_dataset, "master_census_dataset.csv")
```
