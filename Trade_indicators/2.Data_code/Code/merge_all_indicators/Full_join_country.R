
library(tidyverse)

countries <- c("MEX", "US", "CAN")

# function to apply to each country
process_country <- function(country_code) {
  
imp = read_csv(paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Imports/", country_code, "_imports_complete.csv")) 
exp = read_csv(paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Exports/", country_code, "_exports_complete.csv"))


imp = imp %>%
  mutate(
    pc = NULL,
    cdi_1 = NULL,
    cdi_3 = NULL,
    TotalVTFIMP = NULL,
    TotalVTFEXP = NULL,
    weights = NULL,
    s_i_adjustedcdi1 = NULL,
    s_i_adjustedcdi3 = NULL,
    thresh1 = NULL,
    thresh3 = NULL
  ) %>%
  rename(
    cdi1_imports = cd1_adjusted,
    cdi3_imports = cd3_adjusted,
    VTFSECimports = TotalVTFEXSEC
  ) %>%
  distinct(year, sector,VTFSECimports, cdi1_imports, cdi3_imports)


exp = exp %>%
  mutate(
    pc = NULL,
    cdi_1 = NULL,
    cdi_3 = NULL,
    TotalVTFIMP = NULL,
    TotalVTFEXP = NULL,
    weights = NULL,
    s_i_adjustedcdi1 = NULL,
    s_i_adjustedcdi3 = NULL,
    thresh1 = NULL,
    thresh3 = NULL
  ) %>%
  rename(
    cdi1_exports = cd1_adjusted,
    cdi3_exports = cd3_adjusted,
    VTFSECexports = TotalVTFEXSEC
  ) %>%
  distinct(year, sector,VTFSECexports, cdi1_exports, cdi3_exports)

full = imp %>%
  left_join(exp, by = c("year", "sector")) %>%
  mutate(Trade_balance = VTFSECexports - VTFSECimports) %>%
  group_by(year) %>%
  mutate(exp_share = VTFSECexports/sum(VTFSECexports),
         imp_share = VTFSECimports/sum(VTFSECimports))

write_csv(full, paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/", country_code, "_full.csv"))
}

# loop over countries and apply function
lapply(countries, process_country)





















