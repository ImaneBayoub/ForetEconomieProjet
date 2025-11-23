library(dplyr)
library(fixest)

# ------------------------------
# 1. Charger données
# ------------------------------
df <- read.csv("data/resultat_avec_foret.csv")

# On garde seulement 2010 et 2020
df <- df %>% filter(ANNREF %in% c(2010, 2020))

# ------------------------------
# 2. Construire delta forêt
# ------------------------------
df <- df %>%
  mutate(delta_foret = part_foret_2018 - part_foret_2012)

# ------------------------------
# 3. Agrégation commune × année
# ------------------------------
df_commune <- df %>%
  group_by(CODE_GEO, ANNREF) %>%
  summarise(
    VALEUR_BRUTE = sum(VALEUR_BRUTE, na.rm = TRUE),
    NB_ETAB = sum(NB_ETAB, na.rm = TRUE),
    delta_foret = first(delta_foret)
  ) %>% ungroup()

# ------------------------------
# 4. Productivité
# ------------------------------
df_commune <- df_commune %>%
  mutate(
    prod_etab = VALEUR_BRUTE / NB_ETAB,
    log_prod_etab = log(prod_etab),
    POST = ifelse(ANNREF == 2020, 1, 0)
  )

# ------------------------------
# 5. Vrai modèle DiD intensité
# ------------------------------
mod <- feols(
  log_prod_etab ~ delta_foret:POST | CODE_GEO + ANNREF,
  data = df_commune
)

summary(mod)
