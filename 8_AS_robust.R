library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)

twfe_data = read.csv("data/twfe_data.csv")


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
    )



################################################################################
# 1. Test des tendances parallèles (placebo)
################################################################################

# On teste si le changement FUTUR du traitement (D2 à D3)
# explique le changement PASSÉ du résultat (Y1 à Y2)

## 1.1 Test linéaire
model_placebo <- lm(delta_logY_12 ~ D1 + delta_D_23, data = df, na.action = na.omit)
summary(model_placebo)

#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.03880    0.02011   1.929   0.0537 .
# D1           0.02041    0.06879   0.297   0.7667
# delta_D_23  -0.17423    0.48496  -0.359   0.7194

# L'hypothèse de tendances parallèles est validée (p-value > 0.05 pour delta_D_23).

## 1.2 Test non-paramétrique
# On utilise une régression non-paramétrique (GAM) pour tester la relation entre delta_Y_12 et D1, ainsi que delta_D_23.
# Si les fonctions de lissage ne sont pas significatives, cela suggère que les tendances parallèles sont plausibles.

placebo_gam_int <- gam(
  delta_logY_12 ~ s(D1) + ti(D1, delta_D_23),
  data = df,
  na.action = na.omit
)
summary(placebo_gam_int)


# Approximate significance of smooth terms:
#                     edf Ref.df     F p-value
# s(D1)             6.620  7.755 0.737   0.675
# ti(D1,delta_D_23) 3.345  4.363 1.563   0.175

# L'hypothèse de tendances parallèles est validée (p-value > 0.05 pour les fonctions de lissage).


################################################################################
# 2. Test de l'overlap condition
################################################################################
# Il faut que le support des Switchers soit inclus dans celui des Stayers (condition d'overlap)

# On compare la distribution du traitement initial (D1) entre Switchers (S=1) et Stayers (S=0)
ggplot(df, aes(x = D1, fill = factor(S))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("skyblue", "tomato"), 
                    name = "Groupe", 
                    labels = c("Stayers (S=0)", "Switchers (S=1)")) +
  labs(title = "Test du Support Commun (Distribution de D1)",
       x = "Niveau de forêt initial (D1)",
       y = "Densité") +
  theme_minimal()

range(df$D1[df$S==1])
range(df$D1[df$S==0])

# > range(df$D1[df$S==1])
# [1] 0.0000000 0.9929215
# > range(df$D1[df$S==0])
# [1] 0.0000000 0.9329911

# Les switchers ont un support légèrement plus large que les stayers
# On procède à un trimming pour enlever les observations qui n'ont pas de support commun


################################################################################
# 3. Trimming : enlever les observations qui n'ont pas de support commun.
################################################################################
# On enlève les observations dont D1 est en dessous du 5e percentile
# ou au dessus du 95e percentile parmi les stayers.
# On peut aussi enlever les switchers qui ont un changement de traitement très faible (delta_D proche de 0).

lower <- quantile(df$D1[df$S==0], 0.05)
upper <- quantile(df$D1[df$S==0], 0.95)

df_trim <- df %>%
  filter(
    !is.na(delta_logY),      # <- AJOUTE ÇA
    D1 >= lower,
    D1 <= upper,
    abs(delta_D) > 0.005 | S==0
  )

message("Nombre d'observations avant : ", nrow(df), 
        " | Après : ", nrow(df_trim))


# Nombre d'observations avant : 28550 | Après : 7589


# Vérification du support commun après trimming
ggplot(df_trim, aes(D1, delta_logY, color=factor(S)))+
  geom_point(alpha=.3)+
  geom_smooth(method="gam", formula=y~s(x))+
  theme_minimal()
range(df_trim$D1[df_trim$S==1])
range(df_trim$D1[df_trim$S==0])

# > range(df_trim$D1[df_trim$S==1])
# [1] 0.0000000 0.4733817
# > range(df_trim$D1[df_trim$S==0])
# [1] 0.0000000 0.4733097

# Les supports sont désormais alignés entre switchers et stayers, ce qui valide la condition d'overlap.

table(df_trim$S)
#    0    1 
# 5427 2162


################################################################################
# 4. Calcul de l'estimateur AS sur l'échantillon trimmed
################################################################################
# Pour donner plus de poids à ceux qui ont un changement plus important.

# 5.1. Estimation de la tendance chez les stayers (S=0)
mod_stayers <- gam(delta_logY ~ s(D1), data=df_trim[df_trim$S==0,], na.action = na.omit)

# 5.2. Prédire le delta_Y "attendu" pour les switchers (S=1)
switchers <- df_trim[df_trim$S == 1, ]
switchers$delta_logY_hat <- predict(mod_stayers, newdata = switchers)

# 5.3. Calcul de l'estimateur AS
delta_AS <- with(switchers,
  sum(delta_D * (delta_logY - delta_logY_hat)) / sum(delta_D^2)
)


print(delta_AS)
# [1] -0.2330814

# Visualisation de la relation entre D1 et delta_Y, en différenciant switchers et stayers
ggplot(df_trim, aes(D1, delta_logY, color=factor(S)))+
  geom_point(alpha=.3)+
  geom_smooth(method="gam", formula=y~s(x))+
  theme_minimal()
# On dirait que pour des valeurs de D1 proches de 0 (peu de forêt), les switchers sont au-dessus des stayers
# mais que pour des valeurs de D1 plus élevées, les switchers sont en dessous des stayers.
# Cela pourrait expliquer l'estimateur AS négatif.

################################################################################
# 5. Calcul de l'erreur standard par bootstrap
################################################################################

set.seed(123)
n_bootstrap <- 1000
boot_results <- numeric(n_bootstrap)

calculate_did_l <- function(data) {
  stayers_boot <- data[data$S == 0, ]
  switchers_boot <- data[data$S == 1, ]

  if(nrow(stayers_boot) < 2 | nrow(switchers_boot) < 2) return(NA)

  mod_stayers <- gam(delta_logY ~ s(D1), data = stayers_boot, na.action = na.omit)
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


# Nettoyage des NA éventuels (si un tirage manque de stayers/switchers)
boot_results <- boot_results[!is.na(boot_results)]

# --- Résultats Finaux ---
se_delta <- sd(boot_results)
t_stat   <- delta_AS / se_delta
p_val    <- 2 * (1 - pnorm(abs(t_stat)))
ci_low   <- quantile(boot_results, 0.025)
ci_high  <- quantile(boot_results, 0.975)

# Affichage propre
cat("Estimateur AS :", round(delta_AS, 4), "\n",
    "Erreur Standard  :", round(se_delta, 4), "\n",
    "p-value          :", round(p_val, 4), "\n",
    "IC 95%           : [", round(ci_low, 4), ";", round(ci_high, 4), "]\n")


# Estimateur AS : -0.2331
#  Erreur Standard  : 0.8786
#  p-value          : 0.7908
#  IC 95%           : [ -1.8458 ; 1.233 ]

# Résultats non significatifs, avec un intervalle de confiance très large.
# Cela suggère que l'effet estimé n'est pas statistiquement différent de zéro.


################################################################################
# 6. Choix du threshold pour définir les switchers
################################################################################
p_abs <- ggplot(df, aes(x = abs(delta_D))) +
  geom_histogram(bins = 200, fill = "orange", alpha = 0.7) +
  geom_vline(xintercept = c(0.001, 0.005, 0.01, 0.02),
             linetype = "dashed") +
  coord_cartesian(xlim = c(0, 0.1)) +
  labs(
    title = "Distribution de |ΔD|",
    subtitle = "Chercher le 'coude' pour fixer un threshold",
    x = "|ΔD|",
    y = "Nombre d'observations"
  ) +
  theme_minimal()

ggsave("abs_deltaD_hist.png", plot = p_abs, width = 9, height = 5, dpi = 200)

thresholds <- c(0.001, 0.005, 0.01, 0.02)

n_switchers <- sapply(thresholds, function(th) {
  sum(abs(df$delta_D) > th)
})

share_switchers <- n_switchers / nrow(df)

data.frame(
  threshold = thresholds,
  n_switchers = n_switchers,
  share_switchers = round(share_switchers, 3)
)

share_variation <- sapply(thresholds, function(th) {
  sum(abs(df$delta_D[abs(df$delta_D) > th])) / sum(abs(df$delta_D))
})

data.frame(
  threshold = thresholds,
  share_total_variation = round(share_variation, 3)
)

################################################################################
# 7. Graphique valeur de l'estimateur selon la valeur du seuil
################################################################################


library(purrr)
library(zoo)

plot_seuil_ASS <- res %>%
  arrange(seuil) %>%
  mutate(delta_ma = zoo::rollmean(delta_1, k = 50, fill = NA, align = "center"))
# k = taille de la fenetre (a ajuster)

ggplot(plot_seuil_ASS, aes(x = seuil, y = delta_1)) +
  geom_point(alpha = 0.4) +
  #geom_line(aes(y = delta_ma), color = "red", linewidth = 1) +
  labs(
    x = "Seuil",
    y = "Effet estimé",
    title = "Estimation de l'effet avec une méthode d'ASS en fonction du seuil"
  ) +
  xlim(0,0.05) +
  ylim(-15,25) +
  theme_minimal()

