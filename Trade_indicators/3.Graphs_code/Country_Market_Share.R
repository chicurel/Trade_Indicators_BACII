library(tidyverse)
library(haven)

setwd("C:/Users/usuario/Desktop/TFM_R/3.Graphs_code/")

MEX = read_csv("C:/Users/usuario/Desktop/TFM_R/2.Data_code/Output/Sector_quotas/MEX_importers_quotas.csv") %>%
  mutate(
    exp = "MEX"
  )


#MEX per Sector
# Machinery/Electrical ----
MEX1 = MEX %>% filter(
  sector == "Machinery/Eelectrical" ) %>%
  filter(year > 2014) %>%
  arrange(year, desc(s_i)) %>%
  mutate(imp = as.character(imp)) %>%
  filter(imp == "124" | imp == "156" | imp == "170" | imp == "214" | 
           imp == "392" | imp == "410" | imp == "490" | imp == "699" |
           imp == "702" | imp == "842" | imp == "999")  


ggplot(MEX1, aes(x = imp, fill=as.factor(imp), y=s_i)) +
  ggtitle("Mexico VTF for Machinery/Electrical %")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1)) + 
  theme_bw() +
  theme(legend.title=element_blank())+
  labs(x = "Importing Country", 
       y = "Percentage of Imported Value from each Country") +
  scale_fill_discrete(labels = c("Canada", "China", "US", "Europe")) 

ggsave("Results/MEX_Machinery_Electrical.pdf",  width = 10, height = 10, units = c("in"))



# Mineral Products ----
MEX1 = MEX %>% filter(
  sector == "Mineral Products" ) %>%
  filter(year > 2014) %>%
  arrange(year, desc(s_i)) %>%
  mutate(imp = as.character(imp)) %>%
  filter(imp == "124" | imp == "156" | imp == "170" | imp == "214" | 
           imp == "392" | imp == "410" | imp == "490" | imp == "699" |
           imp == "702" | imp == "842" | imp == "999")


ggplot(MEX1, aes(x = imp, fill=as.factor(imp), y=s_i)) +
  ggtitle("Mexico VTF for Mineral Products %")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1)) + 
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x = "Importing Country", 
       y = "Percentage of Imported Value from each Country") +
  scale_fill_discrete(labels = c("Canada", "China", "Colombia", "Dominican Rep.", 
                                 "Japan", "Korea", "Other Asian", "India", "Singapore",
                                 "US" ,"Europe")) 

ggsave("Results/MEX_Mineral_Products.pdf",  width = 10, height = 10, units = c("in"))


# Metals ----
MEX1 = MEX %>% filter(
  sector == "Metals" ) %>%
  filter(year > 2014) %>%
  arrange(year, desc(s_i)) %>%
  mutate(imp = as.character(imp)) %>%
  filter(imp == "76" | imp == "124" | imp == "152" | imp == "156" | 
           imp == "170" | imp == "320" | imp == "410" | imp == "704" |
           imp == "780" | imp == "842" | imp == "999")


ggplot(MEX1, aes(x = imp, fill=as.factor(imp), y=s_i)) +
  ggtitle("Mexico VTF for Metals %")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1)) + 
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x = "Importing Country", 
       y = "Percentage of Imported Value from each Country") +
  scale_fill_discrete(labels = c("Brazil", "Canada", "Chile", "China", 
                                 "Colombia", "Guatemala", "Korea", "Vietnam", "Trinidad and Tobago",
                                 "US" ,"Europe")) 

ggsave("Results/MEX_Metals.pdf",  width = 10, height = 10, units = c("in"))


# Miscellaneous ----
MEX1 = MEX %>% filter(
  sector == "Miscellaneous" ) %>%
  filter(year > 2014) %>%
  arrange(year, desc(s_i)) %>%
  mutate(imp = as.character(imp)) %>%
  filter(imp == "124" | imp == "156" | imp == "392" | imp == "702" | 
           imp == "826" | imp == "842" | imp == "999")


ggplot(MEX1, aes(x = imp, fill=as.factor(imp), y=s_i)) +
  ggtitle("Mexico VTF for Miscellaneous %")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1)) + 
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x = "Importing Country", 
       y = "Percentage of Imported Value from each Country") +
  scale_fill_discrete(labels = c("Canada", "China", 
                                 "Japan", "Singapore", "UK", "US", "Europe"
                                 )) 

ggsave("Results/MEX_Miscellaneous.pdf",  width = 10, height = 10, units = c("in"))




# Transportation  ----
MEX1 = MEX %>% filter(
  sector == "Transportation" ) %>%
  filter(year > 2014) %>%
  arrange(year, desc(s_i)) %>%
  mutate(imp = as.character(imp)) %>%
  filter(imp == "76" | imp == "124" | imp == "156" |  imp == "842" | imp == "999")


ggplot(MEX1, aes(x = imp, fill=as.factor(imp), y=s_i)) +
  ggtitle("Mexico VTF for Transportation %")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1)) + 
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x = "Importing Country", 
       y = "Percentage of Imported Value from each Country") +
  scale_fill_discrete(labels = c("Brazil", "Canada", 
                                 "China", "US", "Europe")) 

ggsave("Results/MEX_Transportation.pdf",  width = 10, height = 10, units = c("in"))



# Plastic/Rubber ----
MEX1 = MEX %>% filter(
  sector == "Plastics/Rubbers" ) %>%
  filter(year > 2014) %>%
  arrange(year, desc(s_i)) %>%
  mutate(imp = as.character(imp)) %>%
  filter(imp == "124" | imp == "156" | imp == "170" | imp == "76" | 
           imp == "320" | imp == "604" | imp == "842" | imp == "999")


ggplot(MEX1, aes(x = imp, fill=as.factor(imp), y=s_i)) +
  ggtitle("Mexico VTF for Plastics/Rubbers %")+
  geom_bar(stat = "identity") +
  facet_wrap(vars(year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=1)) + 
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x = "Importing Country", 
       y = "Percentage of Imported Value from each Country") +
  scale_fill_discrete(labels = c("Canada", "China", "Colombia", "Brazil", 
                                 "Guatemala", "Peru", "US", "Europe")) 

ggsave("Results/MEX_Plastic_Rubber.pdf",  width = 10, height = 10, units = c("in"))
