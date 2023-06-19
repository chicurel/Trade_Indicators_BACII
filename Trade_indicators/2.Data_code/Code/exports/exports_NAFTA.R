
library(tidyverse)

load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_exp.rds")

data_complete_dummies <- data_complete_dummies %>% 
  mutate(
    imp = ifelse(UE_imp == 1, 999, imp)
  ) %>%
  group_by(pc, year) %>%
  mutate(
    TotalVTF = sum(vtf), 
    TotalVTFEX = sum(ifelse(NAFTA_imp == 0, vtf, 0))
  ) %>% 
  group_by(pc, year, imp) %>%
  mutate(
    TotalVTFCountry = sum(ifelse(NAFTA_imp == 0, vtf, 0), na.rm = TRUE)
  ) %>%
  group_by(pc, year) %>% 
  mutate(
    s_i = ifelse(NAFTA_imp == 0, TotalVTFCountry/TotalVTFEX, 0)
  ) %>%
  distinct(year, imp, pc, s_i, sector, TotalVTF, TotalVTFEX) %>%
  mutate(
    s_i2 =  s_i^2,
    cdi_1 = sum(s_i2, na.rm = TRUE),
    cdi_2 = TotalVTFEX/TotalVTF
  ) %>%
  distinct(year, pc, sector, TotalVTF, TotalVTFEX, cdi_1, cdi_2 )

write_csv(data_complete_dummies, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Exports/NAFTA_exports.csv")
