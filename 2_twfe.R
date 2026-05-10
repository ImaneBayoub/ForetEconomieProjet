# ============================================================
# 2_twfe.R
# TWFE : effet de la forêt et de la lisière
# sur la productivité agricole
# ============================================================

library(dplyr)
library(fixest)

# ── Charger les deux jeux de données ─────────────────────────

df_foret   <- read.csv("data/twfe_data.csv")
df_lisiere <- read.csv("data/twfe_data_lisiere.csv")

# Garder communes présentes aux 3 périodes
df_foret <- df_foret %>%
  group_by(id) %>% filter(n() == 3) %>% ungroup()

df_lisiere <- df_lisiere %>%
  filter(!is.na(lisiere_pct.x)) %>%
  group_by(id) %>% filter(n() == 3) %>% ungroup()

cat("========================================\n")
cat("TWFE — PROP. FORÊT (prop_foret_alt)\n")
cat("========================================\n")
cat("Obs :", nrow(df_foret), "| Communes :", n_distinct(df_foret$id), "\n\n")

fit <- feols(ratio_prod_surface ~ prop_foret_alt | id + time, data = df_foret)
cat("--- ratio_prod_surface ~ prop_foret_alt ---\n"); print(summary(fit))

fit <- feols(log(ratio_prod_surface) ~ prop_foret_alt | id + time, data = df_foret)
cat("--- log(ratio_prod_surface) ~ prop_foret_alt ---\n"); print(summary(fit))

fit <- feols(production ~ prop_foret_alt | id + time, data = df_foret)
cat("--- production ~ prop_foret_alt ---\n"); print(summary(fit))

fit <- feols(log(superficie) ~ prop_foret_alt | id + time, data = df_foret)
cat("--- log(superficie) ~ prop_foret_alt ---\n"); print(summary(fit))

cat("\n\n========================================\n")
cat("TWFE — LISIÈRE (lisiere_pct)\n")
cat("========================================\n")
cat("Obs :", nrow(df_lisiere), "| Communes :", n_distinct(df_lisiere$id), "\n\n")

fit <- feols(ratio_prod_surface ~ lisiere_pct.x | id + time, data = df_lisiere)
cat("--- ratio_prod_surface ~ lisiere_pct.x ---\n"); print(summary(fit))

fit <- feols(log(ratio_prod_surface) ~ lisiere_pct.x | id + time, data = df_lisiere)
cat("--- log(ratio_prod_surface) ~ lisiere_pct.x ---\n"); print(summary(fit))

fit <- feols(production ~ lisiere_pct.x | id + time, data = df_lisiere)
cat("--- production ~ lisiere_pct.x ---\n"); print(summary(fit))

fit <- feols(log(superficie) ~ lisiere_pct.x | id + time, data = df_lisiere)
cat("--- log(superficie) ~ lisiere_pct.x ---\n"); print(summary(fit))
