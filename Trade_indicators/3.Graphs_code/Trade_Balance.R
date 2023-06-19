
library(tidyverse)

setwd("C:/Users/usuario/Desktop/TFM_R/3.Graphs_code/")

# Mexico ----
Mex_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/MEX_full.csv")

Mex_complete %>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
ggplot(aes(x = year, y = Trade_balance)) +
  ggtitle("Mexico Trade Balances per Sector")+
  geom_line() +
  facet_wrap(vars(sector),  scales = "fixed") +
  scale_x_continuous(guide = guide_axis(n.dodge=2)) +
  geom_hline(alpha = 0.5, yintercept=0, linetype="dashed")+
  geom_vline(alpha = 0.5, xintercept=2018)

ggsave("Results/MEX_Trade_balance.pdf",  width = 10, height = 10, units = c("in"))


# US ----
US_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/US_full.csv")

US_complete %>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
  ggplot(aes(x = year, y = Trade_balance)) +
  ggtitle("US Trade Balances per Sector")+
  geom_line() +
  facet_wrap(vars(sector),  scales = "fixed") +
  scale_x_continuous(guide = guide_axis(n.dodge=2)) +
  geom_hline(alpha = 0.5, yintercept=0, linetype="dashed")+
  geom_vline(alpha = 0.5, xintercept=2018)

ggsave("Results/US_Trade_balance.pdf",  width = 10, height = 10, units = c("in"))


# Canada ----
CAN_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/CAN_full.csv")

CAN_complete %>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
  ggplot(aes(x = year, y = Trade_balance)) +
  ggtitle("CAN Trade Balances per Sector")+
  geom_line() +
  facet_wrap(vars(sector),  scales = "fixed") +
  scale_x_continuous(guide = guide_axis(n.dodge=2)) +
  geom_hline(alpha = 0.5, yintercept=0, linetype="dashed")+
  geom_vline(alpha = 0.5, xintercept=2018)

ggsave("Results/CAN_Trade_balance.pdf",  width = 10, height = 10, units = c("in"))


# NAFTA ----
NAFTA_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/NAFTA_full.csv")

NAFTA_complete %>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
  ggplot(aes(x = year, y = Trade_balance)) +
  ggtitle("NAFTA Trade Balances per Sector")+
  geom_line() +
  facet_wrap(vars(sector),  scales = "fixed") +
  scale_x_continuous(guide = guide_axis(n.dodge=2)) +
  geom_hline(alpha = 0.5, yintercept=0, linetype="dashed")+
  geom_vline(alpha = 0.5, xintercept=2018)

ggsave("Results/NAFTA_Trade_balance.pdf",  width = 10, height = 10, units = c("in"))