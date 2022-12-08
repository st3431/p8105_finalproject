Abortion laws by state
================
Mohammad Fouda

## Abortion restrictions data

### 2018-2021

First, we read in the data on abortion laws restriction status by state
2018-2021 found [here](https://lawatlas.org/datasets/abortion-bans). The
`bans2018` is then cleaned and tidied to prepare for merging with other
datasets. Note that states that had no abortion restrictions in 2018
were intentionally coded as `.` by the original coder. These were
replaced with `0` to reflect their status for further data analysis. A
`year` and `state_abv` variables were created, and Washington DC was
renamed to District of Columbia.

``` r
bans2018 <-
  read_excel("data/abortion_bans_data_Oct 2021.xlsx", sheet = "Statistical Data", range = "A1:H127") %>% 
  janitor::clean_names() %>% 
  rename(
    state = jurisdiction,
    abstatus = prohibit_req) %>% 
  mutate(
    state_abv = state.abb[match(state, state.name)],
    state_abv = ifelse(is.na(state_abv), "DC", state_abv),
    year = lubridate::year(effective_date),
    abstatus = replace(abstatus, abstatus == ".", "0")) %>% 
    select(state, state_abv, year, abstatus) %>% 
  filter(year == 2018)

bans2018
```

    ## # A tibble: 51 × 4
    ##    state                state_abv  year abstatus
    ##    <chr>                <chr>     <dbl> <chr>   
    ##  1 Alabama              AL         2018 1       
    ##  2 Alaska               AK         2018 0       
    ##  3 Arizona              AZ         2018 1       
    ##  4 Arkansas             AR         2018 1       
    ##  5 California           CA         2018 1       
    ##  6 Colorado             CO         2018 0       
    ##  7 Connecticut          CT         2018 1       
    ##  8 Delaware             DE         2018 1       
    ##  9 District of Columbia DC         2018 0       
    ## 10 Florida              FL         2018 1       
    ## # … with 41 more rows

### 2022

Next, we read in the 2022 data on abortion restrictions by state found
[here](https://ballotpedia.org/Abortion_regulations_by_state). A `year`
and `state_abv` variables were added, and Washington DC was renamed
District of Columbia to match the 2018 dataset. Though this dataset was
not availabe to download, we were able to scrape it from the source
webpage.

``` r
bans_link <- "https://ballotpedia.org/Abortion_regulations_by_state"
bans_page <- read_html(bans_link)

bans2022 <- 
  bans_page %>%  html_nodes("table") %>% .[2] %>% 
  html_table() %>% .[[1]] %>% 
  janitor::clean_names() %>% 
  rename(
    state = state_abortion_restrictions_based_on_stage_of_pregnancy,
    abstatus = state_abortion_restrictions_based_on_stage_of_pregnancy_2,
    threshold = state_abortion_restrictions_based_on_stage_of_pregnancy_3) %>% 
  mutate(
  state = replace(state, state == "Washington, D.C.", "District of Columbia"),
  state_abv = state.abb[match(state, state.name)],
  state_abv = ifelse(is.na(state_abv), "DC", state_abv),
  year = 2022,
  abstatus = recode(abstatus, "Yes" = "1", "No" = "0")) %>% 
  select(- threshold) %>% 
  slice(-1) %>% 
  head(51) %>% 
  arrange(state) 

bans2022
```

    ## # A tibble: 51 × 4
    ##    state                abstatus state_abv  year
    ##    <chr>                <chr>    <chr>     <dbl>
    ##  1 Alabama              1        AL         2022
    ##  2 Alaska               0        AK         2022
    ##  3 Arizona              1        AZ         2022
    ##  4 Arkansas             1        AR         2022
    ##  5 California           1        CA         2022
    ##  6 Colorado             0        CO         2022
    ##  7 Connecticut          1        CT         2022
    ##  8 Delaware             1        DE         2022
    ##  9 District of Columbia 0        DC         2022
    ## 10 Florida              1        FL         2022
    ## # … with 41 more rows

### Merge the tables

Finally, we merge 2018-2021 and 2022 datasets together. The resulting
dataset is saved as a CSV file to merge with datasets on voter turnout
and clinic abortion distances.

``` r
abortion_bans <-
  full_join(bans2018, bans2022)%>% 
  arrange(state, year) %>% 
  pivot_wider(
    names_from = year,
    values_from = abstatus) %>% 
  rename(abstatus18 = '2018', abstatus22 = '2022') %>% 
  write_csv("data/abortion_bans_final.csv")
```

    ## Joining, by = c("state", "state_abv", "year", "abstatus")

``` r
abortion_bans
```

    ## # A tibble: 51 × 4
    ##    state                state_abv abstatus18 abstatus22
    ##    <chr>                <chr>     <chr>      <chr>     
    ##  1 Alabama              AL        1          1         
    ##  2 Alaska               AK        0          0         
    ##  3 Arizona              AZ        1          1         
    ##  4 Arkansas             AR        1          1         
    ##  5 California           CA        1          1         
    ##  6 Colorado             CO        0          0         
    ##  7 Connecticut          CT        1          1         
    ##  8 Delaware             DE        1          1         
    ##  9 District of Columbia DC        0          0         
    ## 10 Florida              FL        1          1         
    ## # … with 41 more rows
