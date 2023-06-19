library(tidyverse)

load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_imp.rds")

data_complete_dummies <- data_complete_dummies %>% 
  mutate(
    exp = ifelse(UE_exp == 1, 999, exp)
  ) %>%
  group_by(sector, year) %>%
  mutate(
    TotalVTF = sum(vtf), 
    TotalVTFEX = sum(ifelse(NAFTA_exp == 0, vtf, 0))
  ) %>% 
  group_by(sector, year, exp) %>%
  mutate(
    TotalVTFCountry = sum(ifelse(NAFTA_exp == 0, vtf, 0), na.rm = TRUE)
  ) %>%
  group_by(sector, year) %>% 
  mutate(
    s_i = ifelse(NAFTA_exp == 0, TotalVTFCountry/TotalVTFEX, 0)
  ) %>%
  distinct(year, exp, sector, s_i, sector, TotalVTF, TotalVTFEX) %>%
  group_by(year, sector) %>%
  mutate(
    max_exporter = ifelse(s_i == max(s_i), 1, 0)
  ) %>%
  filter(max_exporter == 1)

write_csv(data_complete_dummies, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Imports/NAFTA_max_exporter.csv")

