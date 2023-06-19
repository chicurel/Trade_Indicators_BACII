
library(tidyverse)

# list of countries to loop over
countries <- c(124, 484, 842)

# function to apply to each country
process_country <- function(country_code) {
  # read in data
load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_exp.rds")

data_exp <- data_complete_dummies %>% 
  filter(exp == country_code) %>%
  filter(imp != country_code) %>%
  group_by(year, pc) %>%
  mutate(TotalVTF = sum(vtf)) %>%
  distinct(year, pc, sector, TotalVTF) 
  
# Clear the data and read it in again
rm(data_complete_dummies)
load("C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_imp.rds")

# Subset data where imp equals 484
data_imp <- data_complete_dummies %>% 
  filter(imp == country_code) %>%
  filter(exp != country_code) %>%
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

# write data to csv
write_csv(data_imp, paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Substitutability/", country_code, "_CD3.csv"))
}

# loop over countries and apply function
lapply(countries, process_country)


