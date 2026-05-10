# ============================================================
# 10_AS_lisiere_par_groupe_LCA.R
# AS de lisiere_pct par groupe LCA (annuel, mixte, pérenne)
# ============================================================

library(dplyr)
library(tidyr)
library(mgcv)
library(readr)

set.seed(123)

# ── Charger les données ──────────────────────────────────────

df_lis <- read_csv("data/twfe_data_lisiere.csv", show_col_types = FALSE,
                   col_types = cols(id = col_character()))
lca    <- read_csv("data/lca_communes_classes.csv", show_col_types = FALSE,
                   col_types = cols(insee = col_character()))

# Fusion avec les classes LCA
df <- df_lis %>% left_join(lca %>% select(insee, classe), by = c("id" = "insee"))

# Regroupement LCA → type
df <- df %>% mutate(type_lca = case_when(
  classe %in% c(1, 3, 5) ~ "annuel",
  classe == 2             ~ "mixte",
  classe %in% c(4, 6)    ~ "perenne"
))

# ── Fonction AS pour un groupe ───────────────────────────────

estimer_AS_groupe <- function(df_grp, nom_groupe) {

  cat(sprintf("\n=== AS lisière — Groupe %s ===\n", toupper(nom_groupe)))

  # 1) Garder les communes avec lisière et 3 périodes
  df_grp <- df_grp %>% filter(!is.na(lisiere_pct.x)) %>%
    group_by(id) %>% filter(n() == 3) %>% ungroup()

  # 2) Passer en format large
  dw <- df_grp %>%
    select(id, time, D = lisiere_pct.x, Y = ratio_prod_surface) %>%
    pivot_wider(names_from = time, values_from = c(D, Y), names_sep = "") %>%
    mutate(
      delta_D    = D3 - D2,
      delta_logY = ifelse(Y2 > 0 & Y3 > 0, log(Y3) - log(Y2), NA_real_),
      S          = as.integer(delta_D != 0)
    )

  n_avant <- n_distinct(dw$id)
  cat(sprintf("  N communes (avant trim) : %d\n", n_avant))

  # 3) Support commun (D2)
  cat(sprintf("  Range D2 stayers  : [%.4f ; %.4f]\n",
              min(dw$D2[dw$S==0], na.rm=TRUE), max(dw$D2[dw$S==0], na.rm=TRUE)))
  cat(sprintf("  Range D2 switchers : [%.4f ; %.4f]\n",
              min(dw$D2[dw$S==1], na.rm=TRUE), max(dw$D2[dw$S==1], na.rm=TRUE)))

  # 4) Trimming
  lower <- quantile(dw$D2[dw$S==0], 0.05, na.rm = TRUE)
  upper <- quantile(dw$D2[dw$S==0], 0.95, na.rm = TRUE)

  dw_trim <- dw %>%
    filter(!is.na(delta_logY), D2 >= lower, D2 <= upper,
           abs(delta_D) > 0.005 | S == 0)

  n_apres <- n_distinct(dw_trim$id)
  cat(sprintf("  N communes (après trim) : %d\n", n_apres))

  # 5) Estimateur AS (modèle linéaire pour robustesse)
  stayers <- dw_trim[dw_trim$S == 0, ]
  switchers <- dw_trim[dw_trim$S == 1, ]

  mod <- lm(delta_logY ~ D2, data = stayers, na.action = na.omit)
  switchers$delta_logY_hat <- predict(mod, newdata = switchers)

  delta_AS <- sum(switchers$delta_D * (switchers$delta_logY - switchers$delta_logY_hat), na.rm = TRUE) /
              sum(switchers$delta_D^2, na.rm = TRUE)

  n_switchers <- nrow(switchers)
  n_stayers   <- nrow(stayers)

  if (n_switchers < 30) {
    cat(sprintf("  ⚠  Switchers après trim = %d (< 30) — estimation instable\n", n_switchers))
  }

  # 6) Bootstrap
  n_boot <- 1000
  boot_res <- numeric(n_boot)
  ids <- unique(dw_trim$id)

  for (i in 1:n_boot) {
    boot_ids <- sample(ids, replace = TRUE)
    boot_df <- dw_trim[dw_trim$id %in% boot_ids, ]

    stayers_b <- boot_df[boot_df$S == 0, ]
    switchers_b <- boot_df[boot_df$S == 1, ]

    if (nrow(stayers_b) < 5 || nrow(switchers_b) < 5) {
      boot_res[i] <- NA
      next
    }

    mod_b <- lm(delta_logY ~ D2, data = stayers_b, na.action = na.omit)
    yh <- predict(mod_b, newdata = switchers_b)
    boot_res[i] <- sum(switchers_b$delta_D * (switchers_b$delta_logY - yh), na.rm = TRUE) /
                   sum(switchers_b$delta_D^2, na.rm = TRUE)
  }

  boot_res <- boot_res[!is.na(boot_res)]
  se <- sd(boot_res)
  p_val <- 2 * (1 - pnorm(abs(delta_AS / se)))
  ci_low <- quantile(boot_res, 0.025, na.rm = TRUE)
  ci_high <- quantile(boot_res, 0.975, na.rm = TRUE)

  cat(sprintf("  delta_AS  = %.4f\n", delta_AS))
  cat(sprintf("  se (boot) = %.4f\n", se))
  cat(sprintf("  p-value   = %.4f\n", p_val))
  cat(sprintf("  IC 95%%    = [%.4f ; %.4f]\n", ci_low, ci_high))
  cat(sprintf("  bootstrap valides = %d / %d\n", length(boot_res), n_boot))

  # Retour
  tibble(
    groupe = nom_groupe,
    N_avant = n_avant,
    N_apres = n_apres,
    N_switchers = n_switchers,
    N_stayers = n_stayers,
    delta_AS = delta_AS,
    se = se,
    p_value = p_val,
    ic_bas = ci_low,
    ic_haut = ci_high,
    boot_valides = length(boot_res)
  )
}

# ── Boucle sur les 3 groupes ─────────────────────────────────

resultats <- bind_rows(
  estimer_AS_groupe(df %>% filter(type_lca == "annuel"),  "ANNUEL (classes 1,3,5)"),
  estimer_AS_groupe(df %>% filter(type_lca == "mixte"),   "MIXTE (classe 2)"),
  estimer_AS_groupe(df %>% filter(type_lca == "perenne"), "PÉRENNE (classes 4,6)")
)

# ── Sauvegarde ──────────────────────────────────────────────

write.csv(resultats, "data/AS_lisiere_par_groupe_LCA.csv", row.names = FALSE)
cat("\nSauvegardé : data/AS_lisiere_par_groupe_LCA.csv\n")
