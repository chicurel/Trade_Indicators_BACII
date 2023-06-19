
# Library ----
library(tidyverse)
library(haven)
library(caret)
library(estimatr)
library(foreign)
library(plm)
library(lmtest)
library(stargazer)
library(modelsummary)

# Read data ----
data <- readRDS("C:/Users/usuario/Desktop/TFM_R/Gravity/Gravity_V202211.rds")
pol <- read_dta("C:/Users/usuario/Desktop/TFM_R/Gravity/DPI2020/DPI2020.dta")

# Clean and filter data ----
pol <- pol %>% 
  filter(countryname == "Mexico") %>%
  select(year, ifs, execme)

pol[nrow(pol) + 1,] <- list(2021, "MEX", "Morena")

data <- data %>%
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

  
#Panel Format ----
data <- pdata.frame(data, index = c("country_id_d", "year"))


# Homoskedasticity test ----
bptest(log(tradeflow_baci + 1) ~ log(gdp_o + 1) + log(gdp_d + 1) + log(pop_o + 1) + 
         log(pop_d + 1) +log(distcap + 1) + contig + comlang_off + rta_type + execme + 
         country_id_d, data = data, studentize=F)

# There is heteroskedasticity so going to use robust standard errros

#OLS panel data ----
ols <- plm(log(tradeflow_baci + 1) ~ log(gdp_o + 1) + log(gdp_d + 1) + log(pop_o + 1) + 
             log(pop_d + 1) +log(distcap + 1) + contig + comlang_off + rta_type + execme, data=data, index=c("country_id_d", "year"), model = "pooling")
summary(ols)
t = coeftest(ols, vcovHC)

# Fixed effects ----
fixed <- plm(log(tradeflow_baci + 1) ~ log(gdp_o + 1) + log(gdp_d + 1) + log(pop_o + 1) + 
               log(pop_d + 1) +log(distcap + 1) + contig + comlang_off + rta_type + execme, data=data, index=c("country_id_d", "year"), model="within")
summary(fixed)
r = coeftest(fixed, vcovHC)


# Random effects model ----
random <- plm(log(tradeflow_baci + 1) ~ log(gdp_o + 1) + log(gdp_d + 1) + log(pop_o + 1) + 
                log(pop_d + 1) +log(distcap + 1) +  contig + comlang_off + rta_type + execme, data=data, index=c("country_id_d", "year"), model="random")
summary(random)
y = coeftest(random, vcovHC)



R_statistic = data.frame(
  "name" = c("R^2", "Adjusted R^2"),
  "value1" = c(0.57606,0.57512),
  "value2" = c(0.19238, 0.15618 ),
  "value3" = c(0.25115, 0.24949)
)


nnnn = list(t,r,y)
names(nnnn) <- c("OLS", "Fixed Effects", "Random Effects")

options(modelsummary_model_labels = "roman")
msummary(nnnn, stars = c('*' = .1, '**' = .05, '***' = .01),
         notes = "standard errors in ()",  
         coef_rename = c('log(gdp_o + 1)' = 'loggdp_mex', 'log(gdp_d + 1)' = 'loggdp_j',
                         'log(pop_o + 1)' = 'logpop_mex', 'log(pop_d + 1)' = 'logpop_j',
                         'log(distcap + 1)' = 'log_dist_km', 'contig1' = "contig",
                         "comlang_off1" = "LA", "rta_typeFTA & EIA" = "rta_FTA_&_EIA",
                         "rta_typenone" = "rta_none"),
         gof_omit = "BIC|AIC", add_rows = R_statistic, 
         estimate =  "{estimate} ({std.error}){stars}",
         statistic = NULL,output = "table.png")




phtest(fixed, random) # Random effects better than fixed







options(scipen=999) #to get rid of mathematical notation for small numbers



#For example, errors
#are likely to be correlated by country pair in the gravity model context, so it is important to allow for
#clustering by country pair. To do this, it is necessary to specify a clustering variable that separately
#identifies each country pair independently of the direction of trade. An example is distance, which is
#unique to each country pair but is identical for both directions of trade. A common option specification is therefore cluster = distance


#FE
#Due to these effects, all unilateral influences such as GDPs can no longer
#be estimated. A disadvantage of the use of fixed effects is that, when applied to panel data,
#the number of country-year or country-pair fixed effects can be computationally too high for
#estimation. In addition, no comparative statics are possible with fixed effects as the MR terms
#are not estimated explicitly. Nevertheless, Head and Mayer (2014) highlight the importance of
#the use of fixed effects. Country specific fixed effects are considered by incorporating "iso_o"
#and "iso_d" in fe (see Table 5) next to other possible bilateral variables. When applying the
#function Fixed_Effects to panel data, country-pair fixed effects or interaction effects with
#the time variable may be applied, but the function and especially the inclusion of distance as
#an independent variable may have to be changed depending on the effects used. 


### Not sure ----
options(max.print = 2000)

# Simple OLS ----
ols_data = data %>%
  mutate(
    year = as.numeric(year),
    country_id_d = as.factor(country_id_d)
  )

ols_simple <- lm(tradeflow_baci ~ ., data = ols_data)
summary(ols_simple)


#Normal Fixed effects with year and country ----
fe <- lm(tradeflow_baci ~ ., data = data)
summary(fe)


# Robust OLS with cluster standard errors and logs ----
reg1 <- lm_robust(log(tradeflow_baci + 1) ~ log(distcap + 1) + log(gdp_o + 1) + log(gdp_d + 1) + log(pop_o + 1) + 
                    log(pop_d + 1) + contig + comlang_off + rta_type + execme, cluster = distcap, data = ols,
                  se_type = "stata")
summary(reg1)


# Robust FE with cluster standard errors and logs with year and country ----
reg1 <- lm_robust(log(tradeflow_baci + 1) ~ log(distcap + 1) + log(gdp_o + 1) + log(gdp_d + 1) + log(pop_o + 1) + 
                    log(pop_d + 1) + contig + comlang_off + rta_type + execme + country_id_d + year, cluster = distcap, data = data,
                  se_type = "stata")
summary(reg1)