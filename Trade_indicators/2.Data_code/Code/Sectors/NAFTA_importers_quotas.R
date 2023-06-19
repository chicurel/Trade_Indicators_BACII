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
  distinct(year, imp, sector, s_i, sector, TotalVTF, TotalVTFEX)  %>%
  filter(s_i > 0.0099999) %>%
  arrange(year, sector, desc(s_i))
 

write_csv(data_complete_dummies, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Sector_quotas/NAFTA_importers_quotas.csv")
