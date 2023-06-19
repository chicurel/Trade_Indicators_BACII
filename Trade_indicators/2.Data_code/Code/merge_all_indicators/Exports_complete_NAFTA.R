
library(tidyverse)

exports <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Exports/NAFTA_exports.csv")
cd3 <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Substitutability/NAFTA_CD3.csv")


# merge data
data <- exports %>%
  left_join(cd3, by = c("year", "pc", "sector")) %>%
  drop_na() %>% # drop rows with missing values
  rename(cdi_3 = CDI3)

# generate threshold variables
data <- data %>%
  group_by(year, pc) %>%
  mutate(
    thresh1 = if_else(cdi_1 > 0.4, 1, 0),
    thesh2 = if_else(cdi_2 > 0.5, 1, 0),
    thresh3 = if_else(cdi_3 > 1, 1, 0)
  )

# Calculate TotalVTFEXSEC
data <- data %>%
  group_by(sector, year) %>%
  mutate(TotalVTFEXSEC = sum(TotalVTFEX))

# calculate weights and adjusted variables
data <- data %>%
  mutate(
    weights = TotalVTFEX/TotalVTFEXSEC,
    s_i_adjustedcdi1 = cdi_1 * weights,
    s_i_adjustedcdi2 = cdi_2 * weights,
    s_i_adjustedcdi3 = cdi_3 * weights
  ) %>%
  group_by(sector, year) %>%
  mutate(cd1_adjusted = sum(s_i_adjustedcdi1),
         cd2_adjusted = sum(s_i_adjustedcdi2),
         cd3_adjusted = sum(s_i_adjustedcdi3)
  ) %>%
  ungroup() 

write_csv(data, "C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Exports/NAFTA_exports_complete.csv")

