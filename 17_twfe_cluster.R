library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)

data = read.csv("data/df_cluster_prod_2000_2012.csv")

# Pivotage du dataframe pour avoir une ligne par commune x année
df_long <- data %>%
  # 1. Sélection
  select(
    insee, nom,
    pct_foret_1990, pct_foret_2000, pct_foret_2012,
    prod_1990, prod_2000, prod_2012,
    pixels_total_1990, pixels_total_2000, pixels_total_2012, cluster,
    pct_agri_adj_foret_1990, pct_agri_adj_foret_2000, pct_agri_adj_foret_2012
  ) %>%
  
  # 2. Pivot long
  pivot_longer(
    cols = -c(insee, nom, cluster),
    names_to = c(".value", "year"),
    names_pattern = "(pct_foret|prod|pixels_total|pct_agri_adj_foret)_(1990|2000|2012)"
  ) %>%
  
  # 3. Transformations
  mutate(
    val_prod   = as.numeric(prod),
    val_pixels = as.numeric(pixels_total),
    val_foret  = as.numeric(pct_foret),
    val_adj = as.numeric(pct_agri_adj_foret),

    time = case_when(
      year == "1990" ~ 1,
      year == "2000" ~ 2,
      year == "2012" ~ 3
    ),

    Y = val_prod,
    D = val_foret,
    Z = val_adj
  ) %>%
  
  filter(!is.na(Y)) %>%
  select(insee, nom, time, cluster, Y, D, Z)

colnames(df_long)
# [1] "insee"   "nom"    "time"    "cluster"    "Y"    "D"   "Z"

# 4. Passage au format large pour les Deltas
df_large <- df_long %>%
  pivot_wider(
    names_from = time,
    values_from = c(D, Y, Z),
    names_sep = ""
  ) %>%
  filter(!is.na(Y1) & !is.na(Y2) & !is.na(Y3)) %>%
  mutate(
    delta_D     = D3 - D2,
    delta_Y     = Y3 - Y2,
    delta_Z     = Z3 - Z2,
    S           = as.integer(delta_D != 0),
    delta_logY  = ifelse(Y2 > 0 & Y3 > 0, log(Y3) - log(Y2), NA_real_),
    delta_Y_pre = Y2 - Y1,
    delta_logY_pre = ifelse(Y1 > 0 & Y2 > 0, log(Y2) - log(Y1), NA_real_)
  )

colnames(df_large)
#  [1] "insee"          "nom"            "cluster"        "D1"
#  [5] "D2"             "D3"             "Y1"             "Y2"
#  [9] "Y3"             "Z1"             "Z2"             "Z3"
# [13] "delta_D"        "delta_Y"        "delta_Z"        "S"
# [17] "delta_logY"     "delta_Y_pre"    "delta_logY_pre"


################################################################################
# 1. Test des tendances parallèles (sur 1990 -> 2000)
################################################################################

# On teste si le changement FUTUR du traitement (D2 à D3)
# explique la pré-tendance du résultat (Y1 à Y2)
# => si delta_D "n'explique pas" delta_logY_pre, c'est rassurant.

## 1.1 Test linéaire
model_placebo <- lm(delta_logY_pre ~ D1 + delta_D, data = df_large, na.action = na.omit)
summary(model_placebo)
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.295e-02  2.702e-03  -8.492  < 2e-16 ***
# D1          -3.555e-04  9.815e-05  -3.622 0.000293 ***
# delta_D      2.097e-03  5.232e-04   4.007 6.16e-05 ***
# Mauvaise nouvelle : le changement de traitement futur (delta_D) est corrélé à la pré-tendance (delta_logY_pre) => violation des tendances parallèles.
# Cela suggère que les switchers (delta_D != 0) avaient déjà une tendance différente avant le changement de traitement, ce qui peut biaiser l'estimation de l'effet du traitement.

## 1.2 Test non-paramétrique
placebo_gam_int <- gam(
  delta_logY_pre ~ s(D1) + ti(D1, delta_D),
  data = df_large,
  na.action = na.omit
)
summary(placebo_gam_int)

################################################################################
# 2. Test de l'overlap condition (pour l'estimation 2000 -> 2012)
################################################################################
# Support commun entre Switchers et Stayers doit tenir sur le baseline de la période
# -> ici baseline = D2 (pas D1)

ggplot(df_large, aes(x = D2, fill = factor(S))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("skyblue", "tomato"),
                    name = "Groupe",
                    labels = c("Stayers (S=0)", "Switchers (S=1)")) +
  labs(title = "Test du Support Commun (Distribution de D2)",
       x = "Niveau de forêt initial (D2)",
       y = "Densité") +
  theme_minimal()

range(df_large$D2[df_large$S==1], na.rm=TRUE)
range(df_large$D2[df_large$S==0], na.rm=TRUE)

################################################################################
# 3. Trimming : enlever les observations qui n'ont pas de support commun (sur D2)
################################################################################
# On enlève les obs dont D2 est en dessous du 5e percentile
# ou au dessus du 95e percentile parmi les stayers (S=0).
# On enlève aussi les switchers dont le changement de traitement est trop faible.

lower <- quantile(df_large$D2[df_large$S==0], 0.05, na.rm=TRUE)
upper <- quantile(df_large$D2[df_large$S==0], 0.95, na.rm=TRUE)

df_trim <- df_large %>%
  filter(
    !is.na(delta_logY),      # delta_logY = log(Y3)-log(Y2)
    D2 >= lower,
    D2 <= upper,
    abs(delta_D) > 0.005 | S==0
  )

message("Nombre d'observations avant : ", nrow(df_large),
        " | Après : ", nrow(df_trim))

# Vérification du support commun après trimming
ggplot(df_trim, aes(D2, delta_logY, color=factor(S)))+
  geom_point(alpha=.3)+
  geom_smooth(method="gam", formula=y~s(x))+
  theme_minimal()

range(df_trim$D2[df_trim$S==1], na.rm=TRUE)
range(df_trim$D2[df_trim$S==0], na.rm=TRUE)

table(df_trim$S)
colnames(df_trim)

################################################################################
# 4. TWFE
################################################################################
# TWFE benchmark
m_twfe <- feols(
  Y ~ D | insee + time,
  data = df_long,
  cluster = ~insee
)
summary(m_twfe)

# TWFE avec tendances spécifiques par commune
m_trend_commune <- feols(
  Y ~ D | insee + time + insee[time],
  data = df_long,
  cluster = ~insee
)
summary(m_trend_commune)

# TWFE avec tendances spécifiques par cluster
m_trend_cluster <- feols(
  Y ~ D | insee + time + cluster[time],
  data = df_long,
  cluster = ~insee
)
summary(m_trend_cluster)

# Ajout variable Z qui représente la part de pixels agricoles adjacents à de la forêt
m_Z <- feols(
  Y ~ D + Z | insee + time + cluster[time],
  data = df_long,
  cluster = ~insee
)
summary(m_Z)
#    Estimate Std. Error  t value  Pr(>|t|)
# D  0.000131    1.3e-04  1.00967 0.3126600
# Z -0.000223    8.3e-05 -2.68153 0.0073329 **

# Z avec interactions
m_interact <- feols(
  Y ~ D * Z | insee + time + cluster[time],
  data = df_long,
  cluster = ~insee
)
summary(m_interact)

etable(m_twfe, m_trend_cluster, m_interact)

# Effets différenciés par cluster
m_hetero <- feols(
  Y ~ D * factor(cluster) | insee + time,
  data = df_long,
  cluster = ~insee
)
summary(m_hetero)

# Effets différenciés par bins de D
breaks <- unique(quantile(df_long$D, probs = seq(0, 1, 0.2), na.rm = TRUE))
df_long$D_bin <- cut(
  df_long$D, 
  breaks = breaks,
  include.lowest = TRUE
)

m_bins <- feols(
  Y ~ i(D_bin, ref = levels(D_bin)[1]) | insee + time,
  data = df_long,
  cluster = ~insee
)

iplot(m_bins)
summary(m_bins) # Le premier bin sert de référence

# Plus simple : division en 5 bins égaux
df_long <- df_long %>%
  mutate(D_bin2 = ntile(D, 5)) # Crée des groupes de 1 à 5

m2_bins <- feols(Y ~ i(D_bin2, ref = 1) | insee + time, data = df_long, cluster = ~insee)
summary(m2_bins)

# Effets non linéaires avec interactions
library(marginaleffects)
m_hetero <- feols(
  Y ~ D * Z | insee + time + cluster[time],
  data = df_long,
  cluster = ~insee
)

slopes(
  m_hetero,
  variables = "D",
  condition = "Z"
)

# Synthetic control
library(gsynth)
gs <- gsynth(
  Y ~ D + Z,
  data = df_long,
  index = c("insee","time"),
  force = "two-way",
  CV = TRUE
)

################################################################################
# 4. Calcul de l'estimateur AS sur l'échantillon trimmed (période 2000 -> 2012)
################################################################################
# Pour donner plus de poids à ceux qui ont un changement plus important.

# 4.1 Estimation de la tendance chez les stayers (S=0), en fonction du baseline D2
mod_stayers <- gam(delta_logY ~ s(D2), data=df_trim[df_trim$S==0,], na.action = na.omit)

# 4.2 Prédire le delta_logY "attendu" pour les switchers (S=1)
switchers <- df_trim[df_trim$S == 1, ]
switchers$delta_logY_hat <- predict(mod_stayers, newdata = switchers)

# 4.3 Calcul de l'estimateur AS (sur 2000 -> 2012)
delta_AS <- with(switchers,
  sum(delta_D * (delta_logY - delta_logY_hat), na.rm=TRUE) / sum(delta_D^2, na.rm=TRUE)
)

print(delta_AS)

# Visualisation
ggplot(df_trim, aes(D2, delta_logY, color=factor(S)))+
  geom_point(alpha=.3)+
  geom_smooth(method="gam", formula=y~s(x))+
  theme_minimal()

################################################################################
# 5. Calcul de l'erreur standard par bootstrap (sur 2000 -> 2012)
################################################################################

set.seed(123)
n_bootstrap <- 1000
boot_results <- numeric(n_bootstrap)

calculate_did_l <- function(data) {
  stayers_boot <- data[data$S == 0, ]
  switchers_boot <- data[data$S == 1, ]

  if(nrow(stayers_boot) < 2 | nrow(switchers_boot) < 2) return(NA)

  mod_stayers <- gam(delta_logY ~ s(D2), data = stayers_boot, na.action = na.omit)
  y_hat <- predict(mod_stayers, newdata = switchers_boot)

  delta <- sum(switchers_boot$delta_D * (switchers_boot$delta_logY - y_hat), na.rm = TRUE) /
           sum(switchers_boot$delta_D^2, na.rm = TRUE)

  return(delta)
}

for (i in 1:n_bootstrap) {
  ids <- unique(df_trim$id)
  boot_ids <- sample(ids, replace = TRUE)
  boot_df <- df_trim[df_trim$id %in% boot_ids, ]
  boot_results[i] <- calculate_did_l(boot_df)
}

boot_results <- boot_results[!is.na(boot_results)]

se_delta <- sd(boot_results)
t_stat   <- delta_AS / se_delta
p_val    <- 2 * (1 - pnorm(abs(t_stat)))
ci_low   <- quantile(boot_results, 0.025)
ci_high  <- quantile(boot_results, 0.975)

cat("Estimateur AS :", round(delta_AS, 4), "\n",
    "Erreur Standard  :", round(se_delta, 4), "\n",
    "p-value          :", round(p_val, 4), "\n",
    "IC 95%           : [", round(ci_low, 4), ";", round(ci_high, 4), "]\n")

################################################################################
# 6. Choix du threshold pour définir les switchers (SUR 2000 -> 2012)
################################################################################
p_abs <- ggplot(df, aes(x = abs(delta_D))) +
  geom_histogram(bins = 200, fill = "orange", alpha = 0.7) +
  geom_vline(xintercept = c(0.001, 0.005, 0.01, 0.02),
             linetype = "dashed") +
  coord_cartesian(xlim = c(0, 0.1)) +
  labs(
    title = "Distribution de |ΔD| (2000->2012)",
    subtitle = "Chercher le 'coude' pour fixer un threshold",
    x = "|ΔD|",
    y = "Nombre d'observations"
  ) +
  theme_minimal()

ggsave("abs_deltaD_hist.png", plot = p_abs, width = 9, height = 5, dpi = 200)

thresholds <- c(0.001, 0.005, 0.01, 0.02)

n_switchers <- sapply(thresholds, function(th) {
  sum(abs(df$delta_D) > th, na.rm=TRUE)
})

share_switchers <- n_switchers / nrow(df)

data.frame(
  threshold = thresholds,
  n_switchers = n_switchers,
  share_switchers = round(share_switchers, 3)
)

share_variation <- sapply(thresholds, function(th) {
  sum(abs(df$delta_D[abs(df$delta_D) > th]), na.rm=TRUE) / sum(abs(df$delta_D), na.rm=TRUE)
})

data.frame(
  threshold = thresholds,
  share_total_variation = round(share_variation, 3)
)

################################################################################
# 7bis. Recalcul de `res` (effet AS sur 2000 -> 2012) en fonction du seuil
################################################################################
library(purrr)

# grille de seuils (à adapter)
threshold_grid <- seq(0.0005, 0.05, by = 0.0005)

estimate_AS_threshold <- function(df_base, th) {

  # 1) définir switchers/stayers selon le seuil sur |ΔD23|
  df_th <- df_base %>%
    mutate(
      S = as.integer(abs(delta_D) > th)
    ) %>%
    filter(S == 0 | abs(delta_D) > th)  # garde stayers + "vrais" switchers

  # 2) trimming overlap sur D2 parmi stayers (baseline de 2->3)
  lower <- quantile(df_th$D2[df_th$S==0], 0.05, na.rm = TRUE)
  upper <- quantile(df_th$D2[df_th$S==0], 0.95, na.rm = TRUE)

  df_th <- df_th %>%
    filter(!is.na(delta_logY),
           D2 >= lower, D2 <= upper)

  n_switch <- sum(df_th$S == 1, na.rm = TRUE)
  n_stay   <- sum(df_th$S == 0, na.rm = TRUE)

  # garde-fous pour éviter des estimations débiles
  if(n_switch < 30 || n_stay < 30) {
    return(tibble(seuil = th, delta_1 = NA_real_, n_switchers = n_switch,
                  n_stayers = n_stay))
  }

  # 3) modèle de tendance chez les stayers : ΔlogY23 ~ s(D2)
  mod_stayers <- gam(delta_logY ~ s(D2),
                     data = df_th[df_th$S==0,],
                     na.action = na.omit)

  # 4) prédictions et estimateur AS pondéré par ΔD23
  switchers <- df_th[df_th$S==1,]
  y_hat <- predict(mod_stayers, newdata = switchers)

  delta_AS <- sum(switchers$delta_D * (switchers$delta_logY - y_hat), na.rm = TRUE) /
              sum(switchers$delta_D^2, na.rm = TRUE)

  tibble(
    seuil = th,
    delta_1 = delta_AS,
    n_switchers = n_switch,
    n_stayers = n_stay
  )
}

# IMPORTANT: df doit déjà contenir delta_D = D3-D2, delta_logY = log(Y3)-log(Y2)
res <- map_dfr(threshold_grid, ~estimate_AS_threshold(df, .x))

# option: sauvegarde
write.csv(res, "data/res_finaux.csv", row.names = FALSE)

library(zoo)

plot_seuil_ASS <- res %>%
  arrange(seuil) %>%
  mutate(delta_ma = zoo::rollmean(delta_1, k = 50, fill = NA, align = "center"))

p <- ggplot(plot_seuil_ASS, aes(x = seuil, y = delta_1)) +
  geom_point(alpha = 0.4) +
  # geom_line(aes(y = delta_ma), linewidth = 1) +  # si tu veux le lissage
  labs(
    x = "Seuil",
    y = "Effet estimé",
    title = "Estimation de l'effet ASS en fonction du seuil (2->3)"
  ) +
  xlim(0,0.05) +
  ylim(-15,25) +
  theme_minimal()

# Affichage à l'écran
print(p)

# Sauvegarde
ggsave(
  filename = "plots/ass_effet_selon_seuil_23.png",
  plot = p,
  width = 9,
  height = 5,
  dpi = 300
)