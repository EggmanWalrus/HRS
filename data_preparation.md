Data Preparation
================
Yanwen Wang
9/2/2021

# Import libraries and the HRS dataset

``` r
library(tidyverse)
library(haven)

hrs <- read_sav("randhrs1992_2018v1.sav") %>% 
  select(all_of(Variables))
```

# Clean the dataset

1.  Exclude respondents who didn’t respond but alive in any wave
    (n=28,561)

``` r
hrs <- hrs %>% 
  #Exclude cases with no response but alive
  filter_at(vars(ends_with("IWSTAT")), all_vars(.!=4)) %>% 
  #Exclude cases with no response, don't know if alive or died
  filter_at(vars(ends_with("IWSTAT")), all_vars(.!=9))
```

2.  Exclude respondents and spouses with discrepant marital status in
    any wave (n=27,406)

``` r
hrs <- hrs %>% 
  #Exclude cases with discrepant marital status
  filter_at(vars(ends_with("MSTATF")), all_vars(!(. %in% seq(2, 6))))
```

3.  Select respondents who have married and then widowed (n=3,366)

``` r
hrs_widow <- hrs %>% 
  #Select cases with marital history of only marriage and widowhood
  filter_at(vars((ends_with("MSTATH") & (starts_with("R")))),
            all_vars(. %in% c(NA, 1, 7))) %>%
  #Select cases who entered widowhood in any wave
  filter_at(vars((ends_with("MSTATH") & (starts_with("R")))),
            any_vars(.==7)) %>%
  #Exclude cases who were widows/widowers when entering the survey
  filter_at(vars((ends_with("MSTATH") & (starts_with("R")))),
            any_vars(.==1))

#Exclude cases who widowed, then married, then died
hrs_diemarried <- hrs_widow %>% 
  select("HHIDPN", crosswave('R%dMSTATH')) %>% 
  pivot_longer(cols = 2:15,
               names_to = "Wave",
               values_to = "Status") %>% 
  group_by(HHIDPN) %>% 
  #Find the last occurrence of a valid marital status
  mutate(Last = last(Wave[!is.na(Status)])) %>% 
  filter(Wave == Last) %>% 
  #Select cases that died married
  filter(Status == 1)

hrs_widow <- hrs_widow[!(hrs_widow$HHIDPN %in% hrs_diemarried$HHIDPN), ]
```

4.  Exclude cases with multiple deceased spouses (n=3,243)

``` r
hrs_widow <- hrs_widow %>% 
  #Select cases with only one spouse in all waves
  filter(RASPCT == 1)
```

5.  Identify the deceased spouses of the widowed
