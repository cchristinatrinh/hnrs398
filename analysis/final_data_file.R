library(tidyverse)
library(readxl)
library(writexl)
library(ggiraph)
library(mosaic) #for easy histograms, boxplots,and t-tests etc
library(car)
library(stats)


all_vars <- read_xlsx("HNRS_data.xlsx")
head(all_vars)

era_1 <- read_xlsx("era1.xlsx")
era_2 <- read_xlsx("era2.xlsx")

head(era_1)

#main control is gdp growth
#include vc and education investment in all models

#percent change: ((new value - original value) / original value) * 100

pc_era_1 <- era_1 %>% 
  mutate(vc_pc=(venture_capital_2001-venture_capital_1996)/venture_capital_1996,
         patents_pc=((patents_2001-patents_1996)/patents_1996),
         firm_death_pc=((firm_deaths_2001-firm_deaths_1996)/firm_deaths_1996),
         bach_degrees_pc=(bach_degrees_2001-bach_degrees_1996)/bach_degrees_1996,
         net_jobs_pc=(net_jobs_2001-net_jobs_1996)/net_jobs_1996,
         edu_investment_pc=(edu_investment_2001-edu_investment_1996)/edu_investment_1996,
         info_net_jobs_pc=(info_net_jobs_2001-info_net_jobs_1996)/info_net_jobs_1996,
         employment_pc=(employment_2001-employment_1996)/employment_1996,
         gdp_pc=(gdp_2001-gdp_1996)/gdp_1996,
         gdp_per_capita_pc=(gdp_per_capita_2001-gdp_per_capita_1996)/gdp_per_capita_1996,
         population_pc=(population_2001-population_1996)/population_1996)

pc_era_1 <- pc_era_1 %>% 
  mutate_all(~ifelse(is.nan(.), NA, .))
pc_era_1[sapply(pc_era_1, is.infinite)] <- NA

era1_net_jobs <- lm(net_jobs_pc~vc_pc+gdp_pc+edu_investment_pc+bach_degrees_pc+patents_pc+firm_death_pc, data=pc_era_1, na.rm=TRUE)
summary(era1_net_jobs)

era1_info_net_jobs <- lm(info_net_jobs_pc~vc_pc+gdp_pc+edu_investment_pc+bach_degrees_pc+patents_pc, data=pc_era_1, na.rm=TRUE)
summary(era1_info_net_jobs)

era1_firm_death <- lm(firm_death_pc~vc_pc+gdp_pc+edu_investment_pc+bach_degrees_pc+patents_pc, data=pc_era_1, na.rm=TRUE)
summary(era1_firm_death)

era1_employment <- lm(employment_pc~vc_pc+gdp_pc+edu_investment_pc+bach_degrees_pc+patents_pc, data=pc_era_1, na.rm=TRUE)
summary(era1_employment)

#net jobs vs 
#look into gdp


#ERA 2
pc_era_2 <- era_2 %>% 
  mutate(vc_pc=(venture_capital_2015-venture_capital_2010)/venture_capital_2010,
         patents_pc=((patents_2015-patents_2010)/patents_2010),
         firm_death_pc=((firm_deaths_2015-firm_deaths_2010)/firm_deaths_2010),
         bach_degrees_pc=(bach_degrees_2015-bach_degrees_2010)/bach_degrees_2010,
         net_jobs_pc=(net_jobs_2015-net_jobs_2010)/net_jobs_2010,
         edu_investment_pc=(edu_investment_2015-edu_investment_2010)/edu_investment_2010,
         info_net_jobs_pc=(info_net_jobs_2015-info_net_jobs_2010)/info_net_jobs_2010,
         employment_pc=(employment_2015-employment_2010)/employment_2010,
         gdp_pc=(gdp_2015-gdp_2010)/gdp_2010,
         gdp_per_capita_pc=(gdp_per_capita_2015-gdp_per_capita_2010)/gdp_per_capita_2010,
         population_pc=(population_2015-population_2010)/population_2010)
pc_era_2 <- pc_era_2 %>% 
  mutate_all(~ifelse(is.nan(.), NA, .))
pc_era_2[sapply(pc_era_2, is.infinite)] <- NA
