
library(tidyverse)


imp = read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Imports/NAFTA_imports_complete.csv") 
exp = read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Exports/NAFTA_exports_complete.csv")


imp = imp %>%
  mutate(
    pc = NULL,
    cdi_1 = NULL,
    cdi_2 = NULL,
    cdi_3 = NULL,
    TotalVTFIMP = NULL,
    TotalVTFEXP = NULL,
    weights = NULL,
    s_i_adjustedcdi1 = NULL,
    s_i_adjustedcdi3 = NULL,
    thresh1 = NULL,
    thresh2 = NULL,
    thresh3 = NULL
  ) %>%
  rename(
    cdi1_imports = cd1_adjusted,
    cdi2_imports = cd2_adjusted,
    cdi3_imports = cd3_adjusted,
    VTFSECimports = TotalVTFEXSEC
  ) %>%
  distinct(year, sector,VTFSECimports, cdi1_imports, cdi2_imports, cdi3_imports)


exp = exp %>%
  mutate(
    pc = NULL,
    cdi_1 = NULL,
    cdi_2 = NULL,
    cdi_3 = NULL,
    TotalVTFIMP = NULL,
    TotalVTFEXP = NULL,
    weights = NULL,
    s_i_adjustedcdi1 = NULL,
    s_i_adjustedcdi3 = NULL,
    thresh1 = NULL,
    thresh2 = NULL,
    thresh3 = NULL
  ) %>%
  rename(
    cdi1_exports = cd1_adjusted,
    cdi2_exports = cd2_adjusted,
    cdi3_exports = cd3_adjusted,
    VTFSECexports = TotalVTFEXSEC
  ) %>%
  distinct(year, sector,VTFSECexports, cdi1_exports, cdi2_exports, cdi3_exports)

full = imp %>%
  left_join(exp, by = c("year", "sector")) %>%
  mutate(Trade_balance = VTFSECexports - VTFSECimports) %>%
  group_by(year) %>%
  mutate(exp_share = VTFSECexports/sum(VTFSECexports),
         imp_share = VTFSECimports/sum(VTFSECimports))

write_csv(full, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/NAFTA_full.csv")
