
library(tidyverse)

# list of countries to loop over
countries <- c(124, 484, 842)

# function to apply to each country
process_country <- function(country_code) {
  # read in data
  load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_imp.rds")
  
  # filter data by country
  country_data <- data_complete_dummies %>% 
    filter(imp == country_code & exp != country_code) %>%
    mutate(
      exp = if_else(UE_exp == 1, 999, exp)
    ) %>%
    group_by(pc, year) %>%
    mutate(
      TotalVTF = sum(vtf), 
      TotalVTFEX = sum(ifelse(exp != country_code, vtf, 0))
    )  %>%
    group_by(pc, year, exp) %>%
    mutate(
      TotalVTFCountry =  sum(ifelse(exp != country_code, vtf, 0), na.rm = TRUE)
    ) %>%
    group_by(pc, year) %>% 
    mutate(
      s_i =  ifelse(exp != country_code, TotalVTFCountry/TotalVTFEX, 0)
    ) %>%
    distinct(year, exp, pc, s_i, sector, TotalVTF, TotalVTFEX) %>%
    mutate(
      s_i2 =  s_i^2,
      cdi_1 = sum(s_i2, na.rm = TRUE)
    ) %>%
    distinct(year, pc, sector, TotalVTF, TotalVTFEX, cdi_1) %>%
    arrange(year, pc)
  
  # write data to csv
  write_csv(country_data, paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Imports/", country_code, "_imports.csv"))
}

# loop over countries and apply function
lapply(countries, process_country)
