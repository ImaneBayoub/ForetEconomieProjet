# ============================================================
# 9_AS_lisiere.R
# Adaptation de l'analyse AS robuste (8_AS_robust.R)
# avec la lisière comme traitement au lieu de prop_foret_alt
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(zoo)
library(purrr)

twfe_data <- read.csv("data/twfe_data_lisiere.csv", stringsAsFactors = FALSE)

# Nettoyer : on vire les lignes sans lisière
twfe_data <- twfe_data %>% filter(!is.na(lisiere_pct.x))
cat("Observations avec lisière :", nrow(twfe_data), "\n")

df <- twfe_data %>%
  group_by(id) %>%
  filter(n() == 3) %>%
  ungroup() %>%
  select(id, time, D = lisiere_pct.x, Y = ratio_prod_surface) %>%
  pivot_wider(
    names_from = time,
    values_from = c(D, Y),
    names_sep = ""
  ) %>%
  mutate(
    # Analyse principale 2 -> 3
    delta_D    = D3 - D2,
    delta_Y    = Y3 - Y2,
    S          = as.integer(delta_D != 0),
    delta_logY = ifelse(Y2 > 0 & Y3 > 0, log(Y3) - log(Y2), NA_real_),
    # Placebo 1 -> 2
    delta_Y_12 = Y2 - Y1,
    delta_D_23 = D3 - D2,
    delta_logY_12 = ifelse(Y1 > 0 & Y2 > 0, log(Y2) - log(Y1), NA_real_)
  )

################################################################################
# 1. Test des tendances parallèles (placebo sur 1->2)
################################################################################

model_placebo <- lm(delta_logY_12 ~ D1 + delta_D_23, data = df, na.action = na.omit)
cat("=== Test placebo (tendances parallèles) ===\n")
print(summary(model_placebo))

placebo_gam <- gam(delta_logY_12 ~ s(D1) + ti(D1, delta_D_23),
  data = df, na.action = na.omit)
cat("\n=== Test placebo non-paramétrique ===\n")
print(summary(placebo_gam))

################################################################################
# 2. Support commun
################################################################################

p_support <- ggplot(df, aes(x = D2, fill = factor(S))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("skyblue", "tomato"),
    name = "Groupe", labels = c("Stayers (S=0)", "Switchers (S=1)")) +
  labs(title = "Support Commun - Lisière (D2)",
       x = "Lisière (proportion)", y = "Densité") +
  theme_minimal()
ggsave("plots/support_commun_lisiere.png", plot = p_support, width = 9, height = 5, dpi = 200)

cat("\nRange D2 - Stayers:", range(df$D2[df$S==0], na.rm=TRUE), "\n")
cat("Range D2 - Switchers:", range(df$D2[df$S==1], na.rm=TRUE), "\n")

################################################################################
# 3. Trimming
################################################################################

lower <- quantile(df$D2[df$S==0], 0.05, na.rm=TRUE)
upper <- quantile(df$D2[df$S==0], 0.95, na.rm=TRUE)

df_trim <- df %>%
  filter(!is.na(delta_logY), D2 >= lower, D2 <= upper,
         abs(delta_D) > 0.005 | S == 0)

cat("\nAvant trimming:", nrow(df), "| Après:", nrow(df_trim), "\n")

################################################################################
# 4. Estimateur AS
################################################################################

mod_stayers <- gam(delta_logY ~ s(D2), data = df_trim[df_trim$S==0,], na.action = na.omit)
switchers <- df_trim[df_trim$S == 1, ]
switchers$delta_logY_hat <- predict(mod_stayers, newdata = switchers)

delta_AS <- with(switchers,
  sum(delta_D * (delta_logY - delta_logY_hat), na.rm=TRUE) / sum(delta_D^2, na.rm=TRUE)
)

cat("\n=== Estimateur AS (lisière) ===\n")
cat("delta_AS =", round(delta_AS, 6), "\n")

################################################################################
# 5. Bootstrap
################################################################################

set.seed(123)
n_bootstrap <- 1000
boot_results <- numeric(n_bootstrap)

calc_as <- function(data) {
  stayers_b <- data[data$S == 0, ]
  switchers_b <- data[data$S == 1, ]
  if (nrow(stayers_b) < 2 | nrow(switchers_b) < 2) return(NA)
  mod <- gam(delta_logY ~ s(D2), data = stayers_b, na.action = na.omit)
  yh <- predict(mod, newdata = switchers_b)
  sum(switchers_b$delta_D * (switchers_b$delta_logY - yh), na.rm=TRUE) /
    sum(switchers_b$delta_D^2, na.rm=TRUE)
}

for (i in 1:n_bootstrap) {
  ids <- unique(df_trim$id)
  boot_ids <- sample(ids, replace = TRUE)
  boot_df <- df_trim[df_trim$id %in% boot_ids, ]
  boot_results[i] <- calc_as(boot_df)
}

boot_results <- boot_results[!is.na(boot_results)]
se <- sd(boot_results)
t_stat <- delta_AS / se
p_val <- 2 * (1 - pnorm(abs(t_stat)))
ci_low <- quantile(boot_results, 0.025, na.rm=TRUE)
ci_high <- quantile(boot_results, 0.975, na.rm=TRUE)

cat("Erreur Standard (boot):", round(se, 6), "\n")
cat("p-value:", round(p_val, 4), "\n")
cat("IC 95%: [", round(ci_low, 6), ";", round(ci_high, 6), "]\n")

################################################################################
# 6. Sensibilité au seuil
################################################################################

threshold_grid <- seq(0.0005, 0.05, by = 0.0005)

estimate_AS <- function(df_base, th) {
  df_th <- df_base %>% mutate(S = as.integer(abs(delta_D) > th)) %>%
    filter(S == 0 | abs(delta_D) > th)
  lower <- quantile(df_th$D2[df_th$S==0], 0.05, na.rm = TRUE)
  upper <- quantile(df_th$D2[df_th$S==0], 0.95, na.rm = TRUE)
  df_th <- df_th %>% filter(!is.na(delta_logY), D2 >= lower, D2 <= upper)
  n_switch <- sum(df_th$S == 1, na.rm = TRUE)
  n_stay   <- sum(df_th$S == 0, na.rm = TRUE)
  if (n_switch < 30 || n_stay < 30)
    return(tibble(seuil = th, delta_1 = NA_real_, n_switchers = n_switch, n_stayers = n_stay))
  mod <- gam(delta_logY ~ s(D2), data = df_th[df_th$S==0,], na.action = na.omit)
  sw <- df_th[df_th$S == 1, ]
  yh <- predict(mod, newdata = sw)
  delta <- sum(sw$delta_D * (sw$delta_logY - yh), na.rm = TRUE) / sum(sw$delta_D^2, na.rm = TRUE)
  tibble(seuil = th, delta_1 = delta, n_switchers = n_switch, n_stayers = n_stay)
}

res <- map_dfr(threshold_grid, ~estimate_AS(df, .x))
write.csv(res, "data/res_finaux_lisiere.csv", row.names = FALSE)

p_sens <- ggplot(res %>% arrange(seuil) %>% mutate(delta_ma = rollmean(delta_1, k = 50, fill = NA, align = "center")),
  aes(x = seuil, y = delta_1)) +
  geom_point(alpha = 0.4) +
  labs(x = "Seuil", y = "Effet estimé", title = "Effet AS selon le seuil (lisière)") +
  xlim(0, 0.05) + theme_minimal()
ggsave("plots/ass_effet_selon_seuil_lisiere.png", plot = p_sens, width = 9, height = 5, dpi = 300)

cat("\nFini. Résultats sauvegardés.\n")
