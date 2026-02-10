library(dplyr)
library(tidyr)
library(fixest)
library(arrow)


twfe_foret = clc1990 %>%
  mutate(time = 1) %>%
  bind_rows( clc2000 %>% mutate(time = 2)) %>%
  bind_rows( clc2012 %>% mutate(time = 3)) %>%
  select("id","time","prop_foret","prop_foret_alt") 

agri1988 =  read_parquet("agri1988.parquet") %>% rename(id = com) %>% mutate(time = 1) %>% select("id","time","ratio_prod_surface","production","superficie")
agri2000 = read_parquet("agri2000.parquet") %>% rename(id = com) %>% mutate(time = 2) %>% select("id","time","ratio_prod_surface","production","superficie")
agri2010 = read_parquet("agri2010.parquet") %>% rename(id = com) %>% mutate(time = 3) %>% select("id","time","ratio_prod_surface","production","superficie")


twfe_agri = agri1988 %>%
  bind_rows(agri2000) %>%
  bind_rows(agri2010)


twfe_data = twfe_foret %>% inner_join(twfe_agri, by = join_by(id == id,time== time) ) %>%
  filter(!(is.na(ratio_prod_surface)|is.na(prop_foret)))

table(is.na(twfe_data$ratio_prod_surface))
table(twfe_data %>% select(id) %>% group_by(id) %>% mutate(n_count = n()) %>% ungroup() %>% filter(n_count != 3) %>%  select(n_count))

fit_tw <- feols(ratio_prod_surface ~ prop_foret | id + time, data = twfe_data)
summary(fit_tw)
#
fit_tw <- feols(log(ratio_prod_surface) ~ (prop_foret_alt) | id + time, data = twfe_data)
summary(fit_tw)
fit_tw <- feols(ratio_prod_surface ~ (prop_foret_alt) | id + time, data = twfe_data)
summary(fit_tw)
# Lorsque la couverture forestière augmente d'un %, la production décroit de 322 euros

fit_tw <- feols(production ~ prop_foret_alt | id + time, data = twfe_data)
summary(fit_tw)

fit_tw <- feols(log(superficie) ~ prop_foret_alt | id + time, data = twfe_data)
summary(fit_tw)

