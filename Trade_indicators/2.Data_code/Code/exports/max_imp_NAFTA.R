library(tidyverse)

load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_exp.rds")

data_complete_dummies <- data_complete_dummies %>% 
  mutate(
    imp = ifelse(UE_imp == 1, 999, imp)
  ) %>%
  group_by(sector, year) %>%
  mutate(
    TotalVTF = sum(vtf), 
    TotalVTFEX = sum(ifelse(NAFTA_imp == 0, vtf, 0))
  ) %>% 
  group_by(sector, year, imp) %>%
  mutate(
    TotalVTFCountry = sum(ifelse(NAFTA_imp == 0, vtf, 0), na.rm = TRUE)
  ) %>%
  group_by(sector, year) %>% 
  mutate(
    s_i = ifelse(NAFTA_imp == 0, TotalVTFCountry/TotalVTFEX, 0)
  ) %>%
  distinct(year, imp, sector, s_i, sector, TotalVTF, TotalVTFEX) %>%
  group_by(year, sector) %>%
  mutate(
    max_importer = ifelse(s_i == max(s_i), 1, 0)
  ) %>%
  filter(max_importer == 1)

write_csv(data_complete_dummies, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Exports/NAFTA_max_importer.csv")

