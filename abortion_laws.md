Abortion laws by state
================
Mohammad Fouda

## Abortion restrictions data

### 2018-2021

First, we read in the data on abortions laws restriction status by state
2018-2021 found [here](https://lawatlas.org/datasets/abortion-bans). The
`bans2018_df1` is then cleaned and tidied to prepare for merging with
other datasets. Note that states that had no abortion restrictions were
intentionally coded as `.` by the original coder. These were replaced
with `0` to reflect their status for further data analysis.

``` r
bans2018_df1 <-
  read_excel("data/abortion_bans_data_Oct 2021.xlsx", sheet = "Statistical Data", range = "A1:H127") %>% 
  janitor::clean_names() %>% 
  rename(
    state = jurisdiction,
    abortion_restricted = prohibit_req) %>% 
  mutate(
    state_abv = state.abb[match(state, state.name)],
    state_abv = ifelse(is.na(state_abv), "DC", state_abv),
    year = lubridate::year(effective_date),
    abortion_restricted = replace(abortion_restricted, abortion_restricted == ".", "0")) %>% 
    select(state, state_abv, year, abortion_restricted) %>% 
  distinct()

head(bans2018_df1, 12)
```

    ## # A tibble: 12 × 4
    ##    state       state_abv  year abortion_restricted
    ##    <chr>       <chr>     <dbl> <chr>              
    ##  1 Alabama     AL         2018 1                  
    ##  2 Alabama     AL         2019 1                  
    ##  3 Alaska      AK         2018 0                  
    ##  4 Arizona     AZ         2018 1                  
    ##  5 Arizona     AZ         2021 1                  
    ##  6 Arkansas    AR         2018 1                  
    ##  7 Arkansas    AR         2019 1                  
    ##  8 Arkansas    AR         2020 1                  
    ##  9 Arkansas    AR         2021 1                  
    ## 10 California  CA         2018 1                  
    ## 11 Colorado    CO         2018 0                  
    ## 12 Connecticut CT         2018 1

Using the same 2018-2021 dataset, we include thresholds for abortion
restrictions as defined by laws across the US for the states. The values
for `threshold` variable are re-coded to match codes with the same
meaning in the 2022 data.

``` r
bans2018_df2 <-
  read_excel("data/abortion_bans_data_Oct 2021.xlsx", sheet = "Statistical Data", range = "A1:Z127") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    bans_gest_any_point_in_pregnancy:bans_gest_3rd_trimester,
    names_to = "threshold",
    values_to = "threshold_values") %>% 
  rename(
    state = jurisdiction,
    abortion_banned = bans_prohibit,
    abortion_restricted = prohibit_req) %>% 
  mutate(
    state_abv = state.abb[match(state, state.name)],
    state_abv = ifelse(is.na(state_abv), "DC", state_abv),
    year = lubridate::year(effective_date),
    threshold = recode(threshold,
      "bans_gest_viability" = "Fetal viability",
      "bans_gest_any_point_in_pregnancy" = "Conception",
      "bans_gest_fetal_heartbeat_detected" = "Fetal heartbeat detected",
      "bans_gest_3rd_trimester" = "3rd trimester",
      "bans_gest_4_weeks_postfertilization_6_weeks_lmp" = "4 weeks post-fertilization (6 weeks LMP)",
      "bans_gest_6_weeks_postfertilization_8_weeks_lmp" = "6 weeks post-fertilization (8 weeks LMP)",
      "bans_gest_8_weeks_postfertilization_10_weeks_lmp" = "8 weeks post-fertilization (10 weeks LMP)",
      "bans_gest_10_weeks_postfertilization_12_weeks_lmp" = "10 weeks post-fertilization (12 weeks LMP)",
      "bans_gest_12_weeks_postfertilization_14_weeks_lmp" = "12 weeks post-fertilization (14 weeks LMP)",
      "bans_gest_13_weeks_postfertilization_15_weeks_lmp" = "13 weeks post-fertilization (15 weeks LMP)",
      "bans_gest_16_weeks_postfertilization_18_weeks_lmp" = "16 weeks post-fertilization (18 weeks LMP)",
      "bans_gest_18_weeks_postfertilization_20_weeks_lmp" = "18 weeks post-fertilization (20 weeks LMP)",
      "bans_gest_19_weeks_postfertilization_21_weeks_lmp" = "19 weeks post-fertilization (21 weeks LMP)",
      "bans_gest20_weeks_postfertilization_22_weeks_lmp" = "20 weeks post-fertilization (22 weeks LMP)",
      "bans_gest21_weeks_postfertilization_23_weeks_lmp" = "21 weeks post-fertilization (23 weeks LMP)",
      "bans_gest22_weeks_postfertilization_24_weeks_lmp" = "22 weeks post-fertilization (24 weeks LMP)",
      "bans_gest_24_weeks_postfertilization_26_weeks_lmp" = "24 weeks post-fertilization (26 weeks LMP)")) %>%
  filter(threshold_values == 1) %>% 
  select(state, state_abv, year, abortion_restricted, threshold) %>% 
  distinct()
```

Next, we join the 2018-2021 datasets. `threshold` missing values that
were introduced by the merge are re-coded to reflect the actual status
of abortion restrictions (or its absence) as found in the raw data.

``` r
bans2018 <-
  full_join(bans2018_df1, bans2018_df2) %>% 
  mutate(threshold = ifelse(is.na(threshold), "None", threshold))
```

    ## Joining, by = c("state", "state_abv", "year", "abortion_restricted")

``` r
bans2018 
```

    ## # A tibble: 192 × 5
    ##    state   state_abv  year abortion_restricted threshold                        
    ##    <chr>   <chr>     <dbl> <chr>               <chr>                            
    ##  1 Alabama AL         2018 1                   20 weeks post-fertilization (22 …
    ##  2 Alabama AL         2018 1                   Fetal viability                  
    ##  3 Alabama AL         2019 1                   20 weeks post-fertilization (22 …
    ##  4 Alabama AL         2019 1                   Fetal viability                  
    ##  5 Alabama AL         2019 1                   Conception                       
    ##  6 Alaska  AK         2018 0                   None                             
    ##  7 Arizona AZ         2018 1                   18 weeks post-fertilization (20 …
    ##  8 Arizona AZ         2018 1                   Fetal viability                  
    ##  9 Arizona AZ         2021 1                   18 weeks post-fertilization (20 …
    ## 10 Arizona AZ         2021 1                   Fetal viability                  
    ## # … with 182 more rows

### 2022

Next, we read in the 2022 data on abortions restrictions by state found
[here](https://ballotpedia.org/Abortion_regulations_by_state). A `year`
and `state_abv` variables were added, Washington DC was renamed, and
`threshold` values were re-coded to match those in 2018-2021 dataset in
preparation for the merge. Though this dataset was not availabe to
download, we were able to scrape it from the source webpage.

``` r
bans_link <- "https://ballotpedia.org/Abortion_regulations_by_state"
bans_page <- read_html(bans_link)

bans2022 <- 
  bans_page %>%  html_nodes("table") %>% .[2] %>% 
  html_table() %>% .[[1]] %>% 
  janitor::clean_names() %>% 
  rename(
    state = state_abortion_restrictions_based_on_stage_of_pregnancy,
    abortion_restricted = state_abortion_restrictions_based_on_stage_of_pregnancy_2,
    threshold = state_abortion_restrictions_based_on_stage_of_pregnancy_3) %>% 
  mutate(
  state = replace(state, state == "Washington, D.C.", "District of Columbia"),
  state_abv = state.abb[match(state, state.name)],
  state_abv = ifelse(is.na(state_abv), "DC", state_abv),
  year = 2022,
  abortion_restricted = recode(abortion_restricted, "Yes" = "1", "No" = "0"),
  threshold = recode(threshold,
    "Six weeks post-fertilization" = "6 weeks post-fertilization (8 weeks LMP)",
    "15 weeks since last menstrual period" = "13 weeks post-fertilization (15 weeks LMP)",
    "18 weeks since last menstrual period" = "16 weeks post-fertilization (18 weeks LMP)",
    "20 weeks post-fertilization" = "20 weeks post-fertilization (22 weeks LMP)",
    "20 weeks since last menstrual period" = "18 weeks post-fertilization (20 weeks LMP)",
    "22 weeks since last menstrual period" = "20 weeks post-fertilization (22 weeks LMP)",
    "24 weeks since last menstrual period" = "22 weeks post-fertilization (24 weeks LMP)",
    "24 weeks post-fertilization" = "24 weeks post-fertilization (26 weeks LMP)",
    "Third trimester since last menstrual period" = "3rd trimester")) %>% 
  slice(-1) %>% 
  head(51) %>% 
  arrange(state)

bans2022
```

    ## # A tibble: 51 × 5
    ##    state                abortion_restricted threshold              state…¹  year
    ##    <chr>                <chr>               <chr>                  <chr>   <dbl>
    ##  1 Alabama              1                   Conception             AL       2022
    ##  2 Alaska               0                   None                   AK       2022
    ##  3 Arizona              1                   Fetal viability        AZ       2022
    ##  4 Arkansas             1                   Conception             AR       2022
    ##  5 California           1                   Fetal viability        CA       2022
    ##  6 Colorado             0                   None                   CO       2022
    ##  7 Connecticut          1                   Fetal viability        CT       2022
    ##  8 Delaware             1                   Fetal viability        DE       2022
    ##  9 District of Columbia 0                   None                   DC       2022
    ## 10 Florida              1                   13 weeks post-fertili… FL       2022
    ## # … with 41 more rows, and abbreviated variable name ¹​state_abv

### Merge the tables

Finally, we merge 2018-2021 and 2022 datasets together. The resulting
dataset is saved as a CSV file.

``` r
abortion_bans <-
  full_join(bans2018, bans2022)%>% 
  arrange(state, year) %>% 
  write_csv("data/abortion_bans_final.csv")
```

    ## Joining, by = c("state", "state_abv", "year", "abortion_restricted",
    ## "threshold")

``` r
abortion_bans
```

    ## # A tibble: 243 × 5
    ##    state   state_abv  year abortion_restricted threshold                        
    ##    <chr>   <chr>     <dbl> <chr>               <chr>                            
    ##  1 Alabama AL         2018 1                   20 weeks post-fertilization (22 …
    ##  2 Alabama AL         2018 1                   Fetal viability                  
    ##  3 Alabama AL         2019 1                   20 weeks post-fertilization (22 …
    ##  4 Alabama AL         2019 1                   Fetal viability                  
    ##  5 Alabama AL         2019 1                   Conception                       
    ##  6 Alabama AL         2022 1                   Conception                       
    ##  7 Alaska  AK         2018 0                   None                             
    ##  8 Alaska  AK         2022 0                   None                             
    ##  9 Arizona AZ         2018 1                   18 weeks post-fertilization (20 …
    ## 10 Arizona AZ         2018 1                   Fetal viability                  
    ## # … with 233 more rows
