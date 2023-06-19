
library(tidyverse)

load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_imp.rds")

country_data <- data_complete_dummies %>% 
  mutate(
    exp = ifelse(UE_exp == 1, 999, exp)
  ) %>%
  group_by(sector, year) %>%
  mutate(
    TotalVTF = sum(vtf), 
    TotalVTFEX = sum(ifelse(NAFTA_exp == 0, vtf, 0))
  )  %>%
  group_by(sector, year, exp) %>%
  mutate(
    TotalVTFCountry =  sum(ifelse(NAFTA_exp == 0, vtf, 0), na.rm = TRUE)
  ) %>%
  group_by(sector, year) %>% 
  mutate(
    s_i =  ifelse(NAFTA_exp == 0, TotalVTFCountry/TotalVTFEX, 0)
  ) %>%
  distinct(year, exp, sector, s_i, TotalVTF, TotalVTFEX) %>%
  filter(s_i > 0.0099999) %>%
  arrange(year, sector, desc(s_i))

# write data to csv
write_csv(country_data, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Sector_quotas/NAFTA_exporters_quotas.csv")
