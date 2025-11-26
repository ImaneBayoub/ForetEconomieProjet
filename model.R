library(dplyr)
library(fixest)

# ------------------------------
# 1. Charger données
# ------------------------------
df <- read.csv("data/resultat_avec_foret.csv")

# ------------------------------
# PRINT NA AVANT TOUT
# ------------------------------
cat("\n===== CHECK NA =====\n")
cat("NA dans N110d :", sum(is.na(df$N110d)), "\n")
cat("NA dans NB_ETAB :", sum(is.na(df$NB_ETAB)), "\n")
cat("NA dans VALEUR_BRUTE :", sum(is.na(df$VALEUR_BRUTE)), "\n")
cat("NA dans part_foret_2012 :", sum(is.na(df$part_foret_2012)), "\n")
cat("NA dans part_foret_2018 :", sum(is.na(df$part_foret_2018)), "\n")
cat("=====================\n\n")

# ------------------------------
# 2. Construire delta forêt
# ------------------------------
df <- df %>%
  mutate(delta_foret = part_foret_2018 - part_foret_2012)

# ------------------------------
# 3. Filtrer les NA pour la moyenne pondérée
# ------------------------------
df_clean <- df %>%
  filter(!is.na(N110d), !is.na(NB_ETAB))

# ------------------------------
# 4. Agrégation commune × année + composition agricole
# ------------------------------
df_commune <- df_clean %>%
  group_by(CODE_GEO, ANNREF) %>%
  summarise(
    VALEUR_BRUTE = sum(VALEUR_BRUTE, na.rm = TRUE),
    NB_ETAB = sum(NB_ETAB, na.rm = TRUE),
    delta_foret = first(delta_foret),

    # Moyenne pondérée stable
    N110d_mean = sum(N110d * NB_ETAB) / sum(NB_ETAB)
  ) %>% 
  ungroup()

# ------------------------------
# 5. Productivité + POST
# ------------------------------
df_commune <- df_commune %>%
  mutate(
    prod_etab = VALEUR_BRUTE / NB_ETAB,
    log_prod_etab = log(prod_etab),
    POST = ifelse(ANNREF == 2020, 1, 0)
  )

# ------------------------------
# 6. MODELES
# ------------------------------
mod_simple <- feols(
  log_prod_etab ~ delta_foret:POST | CODE_GEO + ANNREF,
  data = df_commune
)

mod_type <- feols(
  log_prod_etab ~ delta_foret:POST + N110d_mean | CODE_GEO + ANNREF,
  data = df_commune
)

# ------------------------------
# 7. PRINT
# ------------------------------
cat("\n===================== MODELE SIMPLE =====================\n")
print(summary(mod_simple))

cat("\n===================== MODELE AVEC TYPES ==================\n")
print(summary(mod_type))
