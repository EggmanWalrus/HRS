#inflation

library(lubridate)
library(tidyverse)

#Import CPI dataset
monthly_cpi <-read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                         skip = 53, header = TRUE)
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% 
  group_by(cpi_year) %>% 
  summarize(cpi = mean(VALUE))

#Create interview wave and year dataset
wave_year <- data.frame(year = seq(1992, 2018, 2),
           wave = seq(1, 14))

#Join two datasets
yearly_cpi <- inner_join(yearly_cpi, wave_year, by=c("cpi_year"="year"))

#Create adjust_factor
yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi$cpi_year == 2018]/yearly_cpi$cpi
