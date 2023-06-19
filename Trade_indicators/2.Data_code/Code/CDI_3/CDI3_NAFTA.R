
library(tidyverse)


load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_exp.rds")

data_exp <- data_complete_dummies %>% 
  filter(UE_imp == 0) %>%
  group_by(year, pc) %>%
  mutate(TotalVTF = sum(vtf)) %>%
  distinct(year, pc, sector, TotalVTF) 

# Save the output
#write_csv(data_exp, "C:/Users/q64393/Desktop/TFM/2.Data_code/Output/Substitutability/UE_CD3_exp.csv")

# Clear the data and read it in again
rm(data_complete_dummies)
load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_imp.rds")

# Subset data where imp equals NAFTA
data_imp <- data_complete_dummies %>% 
  filter(UE_exp == 0) %>%
  group_by(year, pc) %>%
  mutate(TotalVTFPC = sum(vtf)) %>%
  distinct(year, pc, sector, TotalVTFPC) 

data_imp = data_imp %>%
  left_join(data_exp, by = c("year", "pc","sector"))

# Calculate CDI3
data_imp <- data_imp %>%
  group_by(year, pc) %>%
  mutate(CDI3 = ifelse(TotalVTFPC == 0, 0, TotalVTFPC/TotalVTF),
         CDI3 = ifelse(is.na(CDI3), 9.999, CDI3)) %>%
  rename(
    TotalVTFEXP = TotalVTF,
    TotalVTFIMP = TotalVTFPC
  ) %>%
  distinct(year, pc, sector, TotalVTFEXP, TotalVTFIMP, CDI3) %>%
  arrange(year, pc)

# Save the output
write_csv(data_imp, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Substitutability/NAFTA_CD3.csv")