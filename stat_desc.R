# ============================================================
# Stats descriptives pour la table du rapport
# ============================================================

library(dplyr)
library(readr)

# ── Charger les données ──────────────────────────────────────

df_lisiere <- read_csv("data/twfe_data_lisiere.csv")
df_foret   <- read_csv("data/twfe_data.csv")

# time: 1 = 1990, 2 = 2000, 3 = 2012
annee_map <- c("1" = "1990", "2" = "2000", "3" = "2012")

df_lisiere <- df_lisiere %>% mutate(annee = annee_map[as.character(time)])
df_foret   <- df_foret   %>% mutate(annee = annee_map[as.character(time)])

# ── Fonction helper ──────────────────────────────────────────

stats_var <- function(x) {
  x <- x[!is.na(x)]
  cat(sprintf(
    "  Moy = %.4f | Méd = %.4f | SD = %.4f | N = %d\n",
    mean(x), median(x), sd(x), length(x)
  ))
}

# ============================================================
# 1. TRAITEMENT PRINCIPAL : lisière
# ============================================================

cat("=== Lisière D_it (panel complet, toutes périodes) ===\n")
stats_var(df_lisiere$lisiere_pct.x)

# Delta lisière : passer en large
df_lis_wide <- df_lisiere %>%
  filter(!is.na(lisiere_pct.x)) %>%
  select(id, annee, lisiere_pct.x) %>%
  tidyr::pivot_wider(names_from = annee, values_from = lisiere_pct.x,
                     names_prefix = "lis_")

if (all(c("lis_1990", "lis_2000") %in% names(df_lis_wide))) {
  delta_lis_1 <- df_lis_wide$lis_2000 - df_lis_wide$lis_1990
  cat("\n=== Delta lisière (2000-1990) ===\n")
  stats_var(delta_lis_1)
} else {
  delta_lis_1 <- NULL
  cat("\n⚠  Delta lisière 2000-1990 : données insuffisantes\n")
}

if ("lis_2012" %in% names(df_lis_wide) && "lis_2000" %in% names(df_lis_wide)) {
  delta_lis_2 <- df_lis_wide$lis_2012 - df_lis_wide$lis_2000
  cat("\n=== Delta lisière (2012-2000) ===\n")
  stats_var(delta_lis_2)
} else {
  delta_lis_2 <- NULL
  cat("\n⚠  Delta lisière 2012-2000 : données insuffisantes\n")
}

# ============================================================
# 2. TRAITEMENT ALTERNATIF : proportion de forêt
# ============================================================

cat("\n=== Prop. forêt D_it (panel complet, toutes périodes) ===\n")
stats_var(df_foret$prop_foret_alt)

df_for_wide <- df_foret %>%
  select(id, annee, prop_foret_alt) %>%
  tidyr::pivot_wider(names_from = annee, values_from = prop_foret_alt,
                     names_prefix = "for_")

if (all(c("for_1990", "for_2000") %in% names(df_for_wide))) {
  delta_for_1 <- df_for_wide$for_2000 - df_for_wide$for_1990
  cat("\n=== Delta forêt (2000-1990) ===\n")
  stats_var(delta_for_1)
} else {
  delta_for_1 <- NULL
  cat("\n⚠  Delta forêt 2000-1990 : données insuffisantes\n")
}

if ("for_2012" %in% names(df_for_wide)) {
  delta_for_2 <- df_for_wide$for_2012 - df_for_wide$for_2000
  cat("\n=== Delta forêt (2012-2000) ===\n")
  stats_var(delta_for_2)
}

# ============================================================
# 3. VARIABLE D'INTÉRÊT : ratio production / SAU
# ============================================================

cat("\n=== Ratio prod./SAU Y_it (€/ha) ===\n")
stats_var(df_foret$ratio_prod_surface)

# ============================================================
# 4. SORTIE FORMATÉE pour copier dans LaTeX
# ============================================================

cat("\n\n============================================================\n")
cat("VALEURS À COPIER DANS LA TABLE LaTeX\n")
cat("============================================================\n")

fmt <- function(label, x) {
  if (is.null(x) || all(is.na(x))) {
    sprintf("%-40s  ---  &  ---  &  ---  &  --- \\\\\n", label)
  } else {
    x <- x[!is.na(x)]
    sprintf("%-40s  %.3f  &  %.3f  &  %.3f  &  %d \\\\\n",
            label, mean(x), median(x), sd(x), length(x))
  }
}

cat(fmt("Lisière D_it",                df_lisiere$lisiere_pct.x))
cat(fmt("Delta lisière 2000-1990",      delta_lis_1))
cat(fmt("Delta lisière 2012-2000",      delta_lis_2))
cat(fmt("Prop. forêt D_it",           df_foret$prop_foret_alt))
cat(fmt("Delta forêt 2000-1990",        delta_for_1))
cat(fmt("Ratio prod./SAU Y_it",       df_foret$ratio_prod_surface))
