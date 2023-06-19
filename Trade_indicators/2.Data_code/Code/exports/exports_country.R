
library(tidyverse)

countries <- c(124, 484, 842)

# function to apply to each country
process_country <- function(country_code) {
  # read in data
  load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_exp.rds")
  
  country_data <- data_complete_dummies %>% 
  filter(exp == country_code & imp != country_code) %>% #484 for MEX, 842 for USA & 124 for CAN
  mutate(
    imp = ifelse(UE_imp == 1, 999, imp)
  ) %>%
  group_by(pc, year) %>%
  mutate(
    TotalVTF = sum(vtf), 
    TotalVTFEX = sum(ifelse(imp != country_code, vtf, 0))
  )  %>%
  group_by(pc, year, imp) %>%
  mutate(
    TotalVTFCountry =  sum(ifelse(imp != country_code, vtf, 0), na.rm = TRUE)
  ) %>%
  group_by(pc, year) %>% 
  mutate(
    s_i =  ifelse(imp != country_code, TotalVTFCountry/TotalVTFEX, 0)
  ) %>%
  distinct(year, imp, pc, s_i, sector, TotalVTF, TotalVTFEX) %>%
  mutate(
    s_i2 =  s_i^2,
    cdi_1 = sum(s_i2, na.rm = TRUE)
  ) %>%
  distinct(year, pc, sector, TotalVTF, TotalVTFEX, cdi_1) %>%
  arrange(year, pc)

write_csv(country_data, paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Exports/", country_code, "_exports.csv"))
}

# loop over countries and apply function
lapply(countries, process_country)

