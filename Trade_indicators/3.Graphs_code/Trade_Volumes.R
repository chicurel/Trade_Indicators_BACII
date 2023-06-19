#From Energy_Countries_CDS_VTF extract the p per country
library(tidyverse)

setwd("C:/Users/usuario/Desktop/TFM_R/3.Graphs_code/")

# Mexico ----
Mex_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/MEX_full.csv") %>%
  filter(year > 2015)%>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
  arrange(desc(sector)) %>%
  group_by(year)%>%
  mutate(VTFYEAR = sum(VTFSECimports),
         propvalue = VTFSECimports/VTFYEAR) %>%
  arrange(year, desc(propvalue))

ggplot(Mex_complete, aes(x = sector, fill=sector, y=VTFSECimports,)) +
  ggtitle("Mexico absolute imported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/MEX_absolute_VTFimports_pattern.pdf",  width = 10, height = 10, units = c("in"))

ggplot(Mex_complete, aes(x = sector, fill=sector, y=VTFSECexports,)) +
  ggtitle("Mexico absolute exported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/MEX_absolute_VTFexports_pattern.pdf",  width = 10, height = 10, units = c("in"))


# US ----

US_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/US_full.csv") %>%
  filter(year > 2015)%>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
  arrange(desc(sector)) %>%
  group_by(year)%>%
  mutate(VTFYEAR = sum(VTFSECimports),
         propvalue = VTFSECimports/VTFYEAR) %>%
  arrange(year, desc(propvalue))

ggplot(US_complete, aes(x = sector, fill=sector, y=VTFSECimports,)) +
  ggtitle("US absolute imported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/US_absolute_VTFimports_pattern.pdf",  width = 10, height = 10, units = c("in"))

ggplot(US_complete, aes(x = sector, fill=sector, y=VTFSECexports,)) +
  ggtitle("US absolute exported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/US_absolute_VTFexports_pattern.pdf",  width = 10, height = 10, units = c("in"))

# Canada ----

CAN_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/CAN_full.csv") %>%
  filter(year > 2015)%>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
  arrange(desc(sector)) %>%
  group_by(year)%>%
  mutate(VTFYEAR = sum(VTFSECimports),
         propvalue = VTFSECimports/VTFYEAR) %>%
  arrange(year, desc(propvalue))

ggplot(CAN_complete, aes(x = sector, fill=sector, y=VTFSECimports,)) +
  ggtitle("Canada absolute imported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/CAN_absolute_VTFimports_pattern.pdf",  width = 10, height = 10, units = c("in"))

ggplot(CAN_complete, aes(x = sector, fill=sector, y=VTFSECexports,)) +
  ggtitle("Canada absolute exported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/CAN_absolute_VTFexports_pattern.pdf",  width = 10, height = 10, units = c("in"))



# NAFTA ----

NAFTA_complete <- read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Data_with_indicators/NAFTA_full.csv") %>%
  filter(year > 2015)%>%
  filter(
    sector == "Machinery/Eelectrical" | sector == "Transportation" | sector == "Mineral Products" | sector == "Miscellaneous" | sector == "Metals" | sector == "Plastics/Rubbers"
  ) %>%
  arrange(desc(sector)) %>%
  group_by(year)%>%
  mutate(VTFYEAR = sum(VTFSECimports),
         propvalue = VTFSECimports/VTFYEAR) %>%
  arrange(year, desc(propvalue))

ggplot(NAFTA_complete, aes(x = sector, fill=sector, y=VTFSECimports,)) +
  ggtitle("NAFTA absolute imported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/NAFTA_absolute_VTFimports_pattern.pdf",  width = 10, height = 10, units = c("in"))

ggplot(NAFTA_complete, aes(x = sector, fill=sector, y=VTFSECexports,)) +
  ggtitle("NAFTA absolute exported VTF per sector")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide_axis(title = "pc",n.dodge=1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Results/NAFTA_absolute_VTFexports_pattern.pdf",  width = 10, height = 10, units = c("in"))
