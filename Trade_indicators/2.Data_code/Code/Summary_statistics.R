
library(tidyverse)
library(haven)
library(vtable)
library(showtext)
library(sysfonts)

# Read Data ----
mex <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Imports/MEX_imports_complete.csv")
can <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Imports/CAN_imports_complete.csv")
usa <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Imports/US_imports_complete.csv")
nafta <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/Imports/NAFTA_imports_complete.csv")
gravity <- readRDS("C:/Users/usuario/Desktop/TFM_R/Gravity/Gravity_V202211.rds")
pol <- read_dta("C:/Users/usuario/Desktop/TFM_R/Gravity/DPI2020/DPI2020.dta")

# Clean and filter data ----
pol <- pol %>% 
  filter(countryname == "Mexico") %>%
  select(year, ifs, execme)

pol[nrow(pol) + 1,] <- list(2021, "MEX", "Morena")

gravity <- gravity %>%
  filter(country_id_o == "MEX")%>% 
  select(year, country_id_o, country_id_d, tradeflow_baci, gdp_o, 
         gdp_d, pop_o, pop_d, contig, distcap, comlang_off, rta_type) %>%
  left_join(pol, by=c("year", 'country_id_o'='ifs')) %>%
  mutate(
    rta_type = as.character(rta_type)
  ) %>%
  replace_na(list(tradeflow_baci=0, rta_type="none")) %>%
  filter(year > 1995) %>%
  mutate(
    contig = as.factor(contig),
    comlang_off = as.factor(comlang_off),
    rta_type = as.factor(rta_type),
    execme = as.factor(execme),
    country_id_d = as.factor(country_id_d),
    year = as.factor(year),
    country_id_o = NULL
  )


# Combined them with Regions ----
combined <- bind_rows(
  mex %>% select(TotalVTFIMP, TotalVTFEXP, year) %>% mutate(Region = "MEX"),
  can %>% select(TotalVTFIMP, TotalVTFEXP, year) %>% mutate(Region = "CAN"),
  usa %>% select(TotalVTFIMP, TotalVTFEXP, year) %>% mutate(Region = "USA"),
  nafta %>% select(TotalVTFIMP, TotalVTFEXP, year) %>% mutate(Region = "NAFTA")
)

# Summary Sattistics ----

combined %>%
  st(group = "Region", group.long = TRUE, out = "browser")


gravity %>%
  select(gdp_o,pop_o, tradeflow_baci) %>%
  st(out = "browser")

# Graphs ----
sysfonts::font_add_google("Tinos")
showtext::showtext_auto()

combined %>%
  group_by(year, Region) %>%
  summarise(
    avg_imp = sum(TotalVTFIMP)
  ) %>%
  ggplot(aes(x = year, y = avg_imp, color = Region)) +
  geom_line() +
  labs(title = "Total Value Imported by Region",
       x = "Year",
       y = "Imported Value ") +
  theme(text = element_text(family = "Tinos"),
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, l = 0)))

# ggsave("Imported.png",  width = 10, height = 10, units = c("in"))

combined %>%
  group_by(year, Region) %>%
  summarise(
    avg_imp = sum(TotalVTFEXP)
  ) %>%
  ggplot(aes(x = year, y = avg_imp, color = Region)) +
  geom_line() +
  labs(title = "Total Value Exported by Region",
       x = "Year",
       y = "Exported Value")+
  theme(text = element_text(family = "Tinos"),
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, l = 0)))


# ggsave("Exported.png",  width = 10, height = 10, units = c("in"))




