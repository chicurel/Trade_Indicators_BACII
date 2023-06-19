library(tidyverse)

# list of countries to loop over
countries <- c("MEX", "CAN", "US")

# function to apply to each country
process_country <- function(country_code) {
  
  # read in data
  exports <- read_csv(paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Exports/", country_code, "_exports.csv"))
  cd3 <- read_csv(paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Substitutability/", country_code, "_CD3.csv"))
  
  
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
      thresh3 = if_else(cdi_3 > 1, 1, 0)
    )
  
  # Calculate TotalVTFEXSEC
  data <- data %>%
    group_by(sector, year) %>%
    mutate(TotalVTFEXSEC = sum(TotalVTF))
  
  # calculate weights and adjusted variables
  data <- data %>%
    mutate(
      weights = TotalVTF/TotalVTFEXSEC,
      s_i_adjustedcdi1 = cdi_1 * weights,
      s_i_adjustedcdi3 = cdi_3 * weights
    ) %>%
    group_by(sector, year) %>%
    mutate(cd1_adjusted = sum(s_i_adjustedcdi1),
           cd3_adjusted = sum(s_i_adjustedcdi3)) %>%
    ungroup() %>%
    mutate(
      TotalVTF = NULL,
      TotalVTFEX = NULL
    )
  
  write_csv(data, paste0("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Exports/", country_code, "_exports_complete.csv"))
}

# loop over countries and apply function
lapply(countries, process_country)

