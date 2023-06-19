# Code to open, clean and divide by sectors import data

library(tidyverse)

# Define the path where data is located
path <- "C:/Users/usuario/Desktop/TFM_R/2.Data_code/CEPII_BACI/"

# Define the range of years 
years <- 1995:2021

# Create an empty list to store the data frames
data_list <- list()

# Loop over each year, read  the data, and add it to the list
for (year in years) {
  filename <- paste0("BACI_HS92_Y", year, "_V202301.csv")
  filepath <- paste0(path, filename)
  data <- readr::read_csv(filepath, col_types = cols(X4 = col_character(), X6 = col_number())) %>%
    filter(j == "484" | j == "842" | j == "124")
  data_list[[year]] <- data
}

# Combine all the data frames into a single data frame
data_combined <- dplyr::bind_rows(data_list, .id = "year") %>%
  mutate(
    year = NULL
  ) %>%
  rename(year = t,
         exp = i,
         imp = j,
         pc = k,
         vtf = v,
         qnt = q)

# create NAFTA and EU dummy variable
data_complete_dummies <- data_combined %>%
  mutate(NAFTA_exp = ifelse(exp %in% c(484, 842, 124), 1, 0),
         UE_exp = ifelse(exp %in% c(40, 56, 58, 100, 191, 196, 203, 
                                         208, 233, 246, 251, 276, 300, 348, 
                                         372, 380, 428, 440, 442, 470, 528, 
                                         616, 620, 642, 703, 705, 724, 752), 1, 0))

# Create Sector Variable

data_complete_dummies = data_complete_dummies %>% 
  mutate(sector = case_when(
    substr(pc,1,2) %in% c("01","02","03","04","05") ~ "Animal and Animal Products",
    substr(pc,1,2) %in% c("06","07","08","09","10","11","12","13","14","15") ~ "Vegetable Products",
    substr(pc,1,2) %in% c("16","17","18","19","20","21","22","23","24") ~ "Foodstuff",
    substr(pc,1,2) %in% c("25","26","27") ~ "Mineral Products",
    substr(pc,1,2) %in% c("28","29","30","31","32","33","34","35","36","37","38") ~ "Chemical Products",
    substr(pc,1,2) %in% c("39","40") ~ "Plastics/Rubbers",
    substr(pc,1,2) %in% c("41","42","43") ~ "Raw hides/skins/leathers/furs",
    substr(pc,1,2) %in% c("44","45","46","47","48","49") ~ "Wood and Wood Products",
    substr(pc,1,2) %in% c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63") ~ "Textiles", 
    substr(pc,1,2) %in% c("64", "65", "66", "67") ~ "Footwear/headgear", 
    substr(pc,1,2) %in% c("68", "69", "70", "71") ~ "Stone/glass",
    substr(pc,1,2) %in% c("72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83") ~ "Metals",
    substr(pc,1,2) %in% c("84", "85") ~ "Machinery/Eelectrical",
    substr(pc,1,2) %in% c("86", "87", "88", "89") ~ "Transportation", 
    substr(pc,1,2) %in% c("90", "91", "92", "93", "94", "95", "96", "97") ~ "Miscellaneous"))



#Save the combined data frame to a file
save(data_complete_dummies, file = "C:/Users/usuario/Desktop/TFM_R/2.Data_code/data_complete_dummies_imp.rds")

