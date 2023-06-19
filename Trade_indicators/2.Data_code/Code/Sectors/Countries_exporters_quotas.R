
library(tidyverse)


# list of countries to loop over
countries <- c(124, 484, 842) #484 for MEX, 842 for USA & 124 for CAN

# function to apply to each country
process_country <- function(country_code) {
  # read in data
  load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_imp.rds")
  
  country_data <- data_complete_dummies %>% 
    filter(imp == country_code) %>% 
    mutate(
      exp = ifelse(UE_exp == 1, 999, exp)
    ) %>%
    group_by(sector, year) %>%
    mutate(
      TotalVTF = sum(vtf), 
      TotalVTFEX = sum(ifelse(exp != country_code, vtf, 0))
    )  %>%
    group_by(sector, year, exp) %>%
    mutate(
      TotalVTFCountry =  sum(ifelse(exp != country_code, vtf, 0), na.rm = TRUE)
    ) %>%
    group_by(sector, year) %>% 
    mutate(
      s_i =  ifelse(exp != country_code, TotalVTFCountry/TotalVTFEX, 0)
    ) %>%
    distinct(year, exp, sector, s_i, sector, TotalVTF, TotalVTFEX) %>%
    filter(s_i > 0.0099999) %>%
    arrange(year, sector, desc(s_i))
  
  # write data to csv
  write_csv(country_data, paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Sector_quotas/", country_code, "_exporters_quotas.csv"))
}

# loop over countries and apply function
lapply(countries, process_country)

