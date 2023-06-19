
library(tidyverse)


# list of countries to loop over
countries <- c(124, 484, 842)

# function to apply to each country
process_country <- function(country_code) {
  # read in data
  load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_exp.rds")
  
  country_data <- data_complete_dummies %>% 
  filter(exp == country_code) %>% #484 for MEX, 842 for USA & 124 for CAN
  mutate(
    imp = ifelse(UE_imp == 1, 999, imp)
  ) %>%
  group_by(sector, year) %>%
  mutate(
    TotalVTF = sum(vtf), 
    TotalVTFEX = sum(ifelse(imp != country_code, vtf, 0))
  )  %>%
  group_by(sector, year, imp) %>%
  mutate(
    TotalVTFCountry =  sum(ifelse(imp != country_code, vtf, 0), na.rm = TRUE)
  ) %>%
  group_by(sector, year) %>% 
  mutate(
    s_i =  ifelse(imp != country_code, TotalVTFCountry/TotalVTFEX, 0)
  ) %>%
  distinct(year, imp, sector, s_i, sector, TotalVTF, TotalVTFEX) %>%
  group_by(year, sector) %>%
  mutate(
    max_importer = ifelse(s_i == max(s_i), 1, 0)
  ) %>%
  filter(max_importer == 1)
  
  # write data to csv
  write_csv(country_data, paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Exports/", country_code, "_max_importer.csv"))
}

# loop over countries and apply function
lapply(countries, process_country)



  