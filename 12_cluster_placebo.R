# On utilise le fichier csv pour faire le clustering, mais la spé est seulement numérique
# On utilise le fichier excel (xlsx) pour faire une table : id - nom de la spe

library(readxl)

spe_table <- read_excel("spe_com.xlsx") %>% select("cible"=  "...3")
spe_table = spe_table[-c(1:3),]

spe_table <- spe_table %>%
  mutate(
    cible = na_if(str_trim(cible), "NA"),
    code  = str_extract(cible, "^\\d+"),
    lib = str_trim(str_remove(cible, "^\\d+\\s*-\\s*"))
  ) %>%
  select(-cible) %>%
  distinct()

spe_com <- read.csv("spe_communes.csv", sep = ";")

spe_com = spe_com %>%
  mutate(
    id = row.names(spe_com)
  )
  
colnames(spe_com) = c("lib_com","spe_code","sup","id")


spe_com = spe_com[-c(1,2),] %>%
  select(-c("sup","lib_com"))





df <- twfe_data %>%
  group_by(id) %>%
  filter(n() == 3) %>%
  ungroup() %>%
  select(id, time, D = prop_foret_alt, Y = ratio_prod_surface) %>%
  pivot_wider(
    names_from = time,
    values_from = c(D, Y),
    names_sep = ""
  ) %>%
  mutate(
    # (A) Analyse principale (t1 à t2) en niveau
    delta_D    = D2 - D1,
    delta_Y    = Y2 - Y1,
    S          = as.integer(delta_D != 0),
    
    # (B) Analyse principale en log (uniquement si Y>0)
    delta_logY = ifelse(Y1 > 0 & Y2 > 0, log(Y2) - log(Y1), NA_real_),
    
    # Test Placebo (t2 à t3) en niveau
    delta_Y_12 = Y2 - Y1,
    delta_D_23 = D3 - D2,
    
    # Placebo en log (optionnel, mais propre)
    delta_logY_12 = ifelse(Y1 > 0 & Y2 > 0, log(Y2) - log(Y1), NA_real_)
  ) %>%
  left_join(spe_com, by = "id") %>%
  filter(!is.na(spe_code))

dg <- twfe_data %>%
  group_by(id) %>%
  filter(n() == 3) %>%
  ungroup() %>%
  select(id, time, D = prop_foret_alt, Y = ratio_prod_surface) %>%
  pivot_wider(
    names_from = time,
    values_from = c(D, Y),
    names_sep = ""
  ) %>%
  mutate(
    # (A) Analyse principale (t1 à t2) en niveau
    delta_D    = D2 - D1,
    delta_Y    = Y2 - Y1,
    S          = as.integer(delta_D != 0),
    
    # (B) Analyse principale en log (uniquement si Y>0)
    delta_logY = ifelse(Y1 > 0 & Y2 > 0, log(Y2) - log(Y1), NA_real_),
    
    # Test Placebo (t2 à t3) en niveau
    delta_Y_12 = Y2 - Y1,
    delta_D_23 = D3 - D2,
    
    # Placebo en log (optionnel, mais propre)
    delta_logY_12 = ifelse(Y1 > 0 & Y2 > 0, log(Y2) - log(Y1), NA_real_),
    
    # Département
    dep = substring(id,1,2)
  )
  

################################################################################
# 1. Test des tendances parallèles (placebo)
################################################################################

# On teste si le changement FUTUR du traitement (D2 à D3)
# explique le changement PASSÉ du résultat (Y1 à Y2)

## 1.1 Test linéaire
library(sandwich)
library(lmtest)
library(fixest)

# Cluterisation par la culture de spé (y compris élévage)
model_placebo <- lm(delta_logY_12 ~ D1 + delta_D_23, data = df, na.action = na.omit)
vc <- vcovCL(model_placebo, cluster = ~ spe_code, data = df)
coeftest(model_placebo, vcov. = vc)
#              Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -0.022919   0.013682 -1.6751  0.09392 .
# D1          -0.038538   0.025379 -1.5185  0.12890  
# delta_D_23  -0.200127   0.108687 -1.8413  0.06559 .


# Cluterisation par la culture de département France (France métro hors Corse)
model_placebo <- lm(delta_logY_12 ~ D1 + delta_D_23, data = dg, na.action = na.omit)
vc <- vcovCL(model_placebo, cluster = ~ dep, data = dg)
coeftest(model_placebo, vcov. = vc)
#               Estimate Std. Error t value Pr(>|t|)   
#  (Intercept) -0.0228308  0.0079439 -2.8740 0.004056 **
#  D1          -0.0378620  0.0232255 -1.6302 0.103072   
#  delta_D_23  -0.1849712  0.2072747 -0.8924 0.372188 
