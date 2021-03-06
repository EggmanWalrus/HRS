Data Preparation
================
Yanwen Wang
9/2/2021

# Import libraries and the HRS dataset

``` r
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(haven)
library(lubridate)
library(MASS)
library(car)
library(lme4)

select <- dplyr::select
theme_set(theme_classic())

hrs <- read_sav("randhrs1992_2018v1.sav") %>% 
  select(all_of(Variables))
cam <- read_sav("randcams_2001_2017v1.sav") %>% 
  select(all_of(VariablesC))
```

# Clean the CAM dataset

``` r
#Select variables of consumption and transform into a long format
cam <- cam %>% 
  select("HHIDPN", all_of(Consumption)) %>% 
  pivot_longer(cols = -c("HHIDPN"),
               names_to = c(".value", "Wave", ".value"),
               names_pattern = "([a-z+A-Z+]+)([0-9]+)([a-z+A-Z+]+)") %>% 
  mutate(Wave = as.numeric(Wave)) %>% 
  group_by(HHIDPN) %>% 
  arrange(Wave, .by_group = TRUE) %>% 
  ungroup()

#Adjust for inflation
cam <- cam %>% 
  left_join(yearly_cpi %>% select(wave, adj_factor),
            by=c("Wave"="wave")) %>% 
  mutate(HCTOTC = HCTOTC * adj_factor) %>% 
  select(HHIDPN, Wave, HCTOTC)
```

# Clean the HRS dataset

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
  #Exclude cases who were widows/widowers in all waves
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

5.  Add constant and exit variables of the deceased spouse (n=3,084)

``` r
#Identify the deceased spouses of the widowed
hrs_deceased <- hrs %>% 
  filter(HHIDPN %in% hrs_widow$RASPID1)

#Inner join their constant and exit variables
hrs_widow <- hrs_widow %>% 
  inner_join(hrs_deceased %>% 
               select("HHIDPN", all_of(RA), all_of(Exit)) %>% 
               rename("SHHIDPN" = HHIDPN) %>% 
               #Rename variables names starting with R to S
               rename_with(~gsub("^R", "S", .x)),
             by=c("RASPID1"="SHHIDPN"))
```

6.  Add variables:
    -   RDAge, SDAge (lifespan)
    -   FirstWave (first wave in interview)
    -   FirstWaveM (first wave with a married status)
    -   LastWave (last wave in interview)
    -   WidowWave (first wave widowed after marriage)
    -   WidowAge (age at widowhood after marriage)
    -   MarryLength (length of the current marriage)
    -   MarryAge (age at the start of the current marriage)

``` r
#Add the age of death of both the widowed and the deceased (n=3.059)
hrs_widow <- hrs_widow %>% 
  mutate(RDAge = RADYEAR - RABYEAR,
         SDAge = SADYEAR - SABYEAR) %>% 
  #Exclude cases with the spouse missing death year
  filter(!is.na(SDAge))

attr(hrs_widow$RDAge, "label") <- "RDAge: R death age"
attr(hrs_widow$SDAge, "label") <- "SDAge: S death age"

#Add LastWave
hrs_widow <- hrs_widow %>% 
  inner_join(hrs_widow %>% 
               select("HHIDPN", crosswave('R%dMSTATH')) %>% 
               pivot_longer(cols = 2:15,
                            names_to = "Wave",
                            values_to = "Status") %>% 
               group_by(HHIDPN) %>% 
               #Find the last occurrence of a valid marital status
               mutate(Last = last(Wave[!is.na(Status)])) %>% 
               filter(Wave == Last) %>% 
               mutate(Last = as.numeric(str_extract(Last, "[[:digit:]]+"))) %>% 
               rename("LastWave" = Last) %>% 
               select(HHIDPN, LastWave),
             by="HHIDPN")

attr(hrs_widow$LastWave, "label") <- "LastWave: R Last wave in survey"

#Add FirstWave
hrs_widow <- hrs_widow %>% 
  inner_join(hrs_widow %>% 
               select("HHIDPN", crosswave('R%dMSTATH')) %>%
               pivot_longer(cols = 2:15,
                            names_to = "Wave",
                            values_to = "Status") %>% 
               group_by(HHIDPN) %>% 
               #Find the first occurrence of a valid marital status
               mutate(First = first(Wave[!is.na(Status)])) %>% 
               filter(Wave == First) %>% 
               mutate(First = as.numeric(str_extract(First, "[[:digit:]]+"))) %>% 
               rename("FirstWave" = First) %>% 
               select(HHIDPN, FirstWave),
             by="HHIDPN")

attr(hrs_widow$FirstWave, "label") <- "FirstWave: R first wave in survey"

#Add FirstWaveM
hrs_widow <- hrs_widow %>% 
  inner_join(hrs_widow %>% 
                select("HHIDPN", crosswave('R%dMSTATH')) %>%
                pivot_longer(cols = 2:15,
                             names_to = "Wave",
                             values_to = "Status") %>% 
                group_by(HHIDPN) %>%
                filter(!is.na(Status)) %>% 
                #Find the first occurrence of a married status
                mutate(First = first(Wave[Status==1])) %>% 
                filter(Wave == First) %>% 
                mutate(First = as.numeric(str_extract(First, "[[:digit:]]+"))) %>% 
                rename("FirstWaveM" = First) %>% 
                select(HHIDPN, FirstWaveM),
              by="HHIDPN")

attr(hrs_widow$FirstWaveM, "label") <- "FirstWaveM: R first wave married"

#Add WidowWave
hrs_widow <- hrs_widow %>% 
  inner_join(hrs_widow %>% 
               select("HHIDPN", crosswave('R%dMSTATH')) %>%
               pivot_longer(cols = 2:15,
                            names_to = "Wave",
                            values_to = "Status") %>% 
               group_by(HHIDPN) %>%
               filter(!is.na(Status)) %>% 
               #Find the last occurrence of a married status
               mutate(Last = last(Wave[Status==1])) %>% 
               filter(Wave == Last) %>% 
               #The next wave of the last married status is the first wave widowed
               mutate(Last = as.numeric(str_extract(Last, "[[:digit:]]+")) + 1) %>% 
               rename("WidowWave" = Last) %>% 
               select(HHIDPN, WidowWave),
             by="HHIDPN")

attr(hrs_widow$WidowWave, "label") <- "WidowWave: R first wave widowed"

#Add WidowAge
hrs_widow <- hrs_widow %>% 
  mutate(WidowAge = SADYEAR - RABYEAR)

attr(hrs_widow$WidowAge, "label") <- "WidowAge: R age widowed"

#Add MarryLength, MarryAge (n=3,025)
hrs_widow <- hrs_widow %>% 
  inner_join(hrs_widow %>% 
               select("HHIDPN", crosswave('R%dMCURLN')) %>% 
               pivot_longer(cols = -1,
                            names_to = "Wave",
                            values_to = "Years") %>% 
               group_by(HHIDPN) %>% 
               mutate(Last = last(Wave[!is.na(Years)])) %>% 
               filter(Wave == Last) %>% 
               rename("MarryLength" = Years) %>% 
               select(HHIDPN, MarryLength),
             by="HHIDPN") %>%
  mutate(MarryAge = WidowAge - MarryLength)

attr(hrs_widow$MarryLength, "label") <- "MarryLength: R current marriage length"
attr(hrs_widow$MarryAge, "label") <- "MarryAge: R age start of current marriage"
```

7.  Transform dataset with relevant variables into a long format

``` r
hrs_widowL <- hrs_widow %>% 
  select(all_of(Variables_sorted)) %>% 
  pivot_longer(cols = -c("HHIDPN":"SEHILTC"),
               names_to = c(".value", "Wave", ".value"),
               names_pattern = "([a-z+A-Z+]+)([0-9]+)([a-z+A-Z+]+)") %>% 
  mutate(Wave = as.numeric(Wave)) %>% 
  relocate(Wave, .after=RASPID1) %>% 
  group_by(HHIDPN) %>% 
  arrange(Wave, .by_group=TRUE) %>% 
  ungroup()
```

8.  Adjust for inflation, add (H)OOPMDy (yearly data), R/S total income,
    WidowWaveR and WidowYearR

``` r
hrs_widowL <-  hrs_widowL %>% 
  left_join(yearly_cpi %>% select(wave, adj_factor),
            by=c("Wave"="wave")) %>% 
  mutate_at(Variables_inflation, .funs = funs(. * adj_factor)) %>% 
  #Add OOPMD yearly, WidowWaveR and WidowYearR
  mutate(ROOPMDy = ROOPMD/2,
         SOOPMDy = SOOPMD/2,
         #Set the WidowWave as 0
         WidowWaveR = Wave - WidowWave,
         WidowYearR = WidowWaveR*2) %>% 
  relocate(c("WidowWaveR", "WidowYearR"), .after = WidowWave)

#Fix ROOPMDy and SOOPMD in wave 2, since it's in previous 12 months
hrs_widowL[hrs_widowL$Wave==2, "ROOPMDy"] <- hrs_widowL[hrs_widowL$Wave==2, "ROOPMDy"] * 2
hrs_widowL[hrs_widowL$Wave==2, "SOOPMDy"] <- hrs_widowL[hrs_widowL$Wave==2, "SOOPMDy"] * 2

#Calculate household out-of-pocket medical expenditure
hrs_widowL <- hrs_widowL %>% 
  rowwise() %>% 
  mutate(HOOPMDy = sum(ROOPMDy, SOOPMDy, na.rm = TRUE),
         HOOPMD = sum(ROOPMD, SOOPMD, na.rm = TRUE))

hrs_widowL$HOOPMD[is.na(hrs_widowL$ROOPMD) & is.na(hrs_widowL$SOOPMD)] <- NA
hrs_widowL$HOOPMDy[is.na(hrs_widowL$ROOPMDy) & is.na(hrs_widowL$SOOPMDy)] <- NA

#Calculate respondent's total income (the last calender year)
hrs_widowL <- hrs_widowL %>% 
  rowwise() %>% 
  mutate(RITOT = sum(RIEARN, RIPENA, RISSDI, RISRET, RIUNWC, RIGXFR, na.rm = TRUE),
         SITOT = sum(SIEARN, SIPENA, SISSDI, SISRET, SIUNWC, SIGXFR, na.rm = TRUE))

hrs_widowL$RITOT[is.na(hrs_widowL$RIEARN) & is.na(hrs_widowL$RIPENA) & is.na(hrs_widowL$RISSDI)
                 & is.na(hrs_widowL$RISRET) & is.na(hrs_widowL$RIUNWC) & is.na(hrs_widowL$RIGXFR)] <- NA
hrs_widowL$SITOT[is.na(hrs_widowL$SIEARN) & is.na(hrs_widowL$SIPENA) & is.na(hrs_widowL$SISSDI)
                 & is.na(hrs_widowL$SISRET) & is.na(hrs_widowL$SIUNWC) & is.na(hrs_widowL$SIGXFR)] <- NA
```

# Calculate Catastrophic Health Expenditure

1.  Join two datasets

``` r
#Join two datasets
hrs_widowL <- hrs_widowL %>% 
  left_join(cam, by = c("HHIDPN", "Wave"))
```

2.  Calculate catastrophic health expenditure
    1.  Ratio of out-of-pocket medical expenditure and income
        (household/respondent)
    2.  Ratio of out-of-pocket medical expenditure and total consumption
        (household)
    3.  CHE based on income (categorical)
    4.  CHE based on consumption (categorical)

``` r
#Calculate ratio of OOPMD and income (H/R)
hrs_widowL <- hrs_widowL %>% 
  rowwise() %>% 
  mutate(RRatio = ROOPMDy / RITOT,
         HRatio = sum(ROOPMDy, SOOPMDy, na.rm = TRUE) / HITOT)

hrs_widowL$HRatio[is.na(hrs_widowL$ROOPMDy) & is.na(hrs_widowL$SOOPMDy)] <- NA

#Calculate CHE: 10% of total income (H/R)
hrs_widowL <- hrs_widowL %>% 
  mutate(RCHE_income = cut(RRatio,
                           breaks = c(-Inf, 0.1, Inf),
                           labels = c(0, 1)),
         RCHE_income = as.numeric(as.character(RCHE_income)),
         HCHE_income = cut(HRatio,
                           breaks = c(-Inf, 0.1, Inf),
                           labels = c(0, 1)),
         HCHE_income = as.numeric(as.character(HCHE_income)))
```

``` r
#Calculate ratio of OOPMD and consumption (H); CHE (10% of consumption H)
hrs_widowL <- hrs_widowL %>% 
  rowwise() %>% 
  mutate(HRatioC = sum(ROOPMDy, SOOPMDy, na.rm = TRUE) / HCTOTC,
         HCHE_C = cut(HRatioC,
                      breaks = c(-Inf, 0.1, Inf),
                      labels = c(0, 1)),
         HCHE_C = as.numeric(as.character(HCHE_C)))
```

# Descriptive Analysis

## Health expenditure before and after the loss of a spouse

![](data_preparation_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](data_preparation_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](data_preparation_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

## CHE before and after the loss of a spouse

``` r
#CHE based on household income
hrs_widowL %>% 
  filter((WidowYearR >= -16) & (WidowYearR <= 16)) %>% 
  ungroup() %>% 
  group_by(WidowYearR) %>% 
  summarise(HCHE_income = mean(HCHE_income, na.rm=TRUE)) %>% 
  drop_na(HCHE_income) %>% 
  
  ggplot(aes(WidowYearR, HCHE_income)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(breaks = seq(-16, 16, by = 2)) + 
    scale_y_continuous(labels = scales::percent) + 
    geom_vline(xintercept = 0, color="grey") +
    labs(title = "Risk of CHE Based on Household Income",
         x="Years before and after loss of a spouse",
         y="Likelihood of CHE based on household income")
```

![](data_preparation_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#CHE based on respondent income
hrs_widowL %>% 
  filter((WidowYearR >= -16) & (WidowYearR <= 16)) %>% 
  ungroup() %>% 
  group_by(WidowYearR) %>% 
  summarise(RCHE_income = mean(RCHE_income, na.rm=TRUE)) %>% 
  drop_na(RCHE_income) %>% 
  
  ggplot(aes(WidowYearR, RCHE_income)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(breaks = seq(-16, 16, by = 2)) + 
    scale_y_continuous(labels = scales::percent) + 
    geom_vline(xintercept = 0, color="grey") +
    labs(title = "Risk of CHE Based on Widoer's Income",
         x="Years before and after loss of a spouse",
         y="Likelihood of CHE based on respondent's income")
```

![](data_preparation_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
#CHE based on respondent income by gender
hrs_widowL %>% 
  filter((WidowYearR >= -16) & (WidowYearR <= 16)) %>% 
  ungroup() %>% 
  group_by(WidowYearR, RAGENDER) %>% 
  summarise(RCHE_income = mean(RCHE_income, na.rm=TRUE)) %>% 
  drop_na(RCHE_income) %>% 
  mutate(RAGENDER = as.numeric(RAGENDER),
         RAGENDER = if_else(RAGENDER==1, "male", "female")) %>% 
  
  ggplot(aes(WidowYearR, RCHE_income)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(breaks = seq(-16, 16, by = 2)) + 
    scale_y_continuous(labels = scales::percent) + 
    geom_vline(xintercept = 0, color="grey") +
    labs(title = "Risk of CHE Based on Widoer's Income, by gender",
         x="Years before and after loss of a spouse",
         y="Likelihood of CHE based on respondent's income") +
    facet_wrap(~RAGENDER)
```

![](data_preparation_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
#CHE based on consumption
hrs_widowL %>% 
  filter((WidowYearR >= -16) & (WidowYearR <= 16)) %>% 
  ungroup() %>% 
  group_by(WidowYearR) %>% 
  summarise(HCHE_C = mean(HCHE_C, na.rm = TRUE)) %>% 
  drop_na(HCHE_C) %>% 
  
  ggplot(aes(WidowYearR, HCHE_C)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(breaks = seq(-16, 16, by = 2)) + 
    scale_y_continuous(labels = scales::percent) + 
    geom_vline(xintercept = 0, color="grey") +
    labs(title = "Risk of CHE Based on Household Consumption",
         x="Years before and after loss of a spouse",
         y="Likelihood of CHE based on household consumption")
```

![](data_preparation_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

# Models

## Linear mixed model

``` r
lmm <- lmer(RCHE_income ~ WidowYearR + (1|RAGENDER) + (1|RARACEM) + (1|RUNEMP), 
            data=hrs_widowL)
summary(lmm)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RCHE_income ~ WidowYearR + (1 | RAGENDER) + (1 | RARACEM) + (1 |  
    ##     RUNEMP)
    ##    Data: hrs_widowL
    ## 
    ## REML criterion at convergence: 6186.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9904 -0.5861 -0.5104 -0.3396  2.1924 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  RARACEM  (Intercept) 0.0001872 0.01368 
    ##  RUNEMP   (Intercept) 0.0095385 0.09767 
    ##  RAGENDER (Intercept) 0.0033440 0.05783 
    ##  Residual             0.1700496 0.41237 
    ## Number of obs: 5776, groups:  RARACEM, 3; RUNEMP, 2; RAGENDER, 2
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## (Intercept)  0.2453190  0.0820805   2.989
    ## WidowYearR  -0.0022307  0.0006573  -3.394
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## WidowYearR 0.041

``` r
Anova(lmm)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: RCHE_income
    ##             Chisq Df Pr(>Chisq)    
    ## WidowYearR 11.518  1  0.0006892 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
