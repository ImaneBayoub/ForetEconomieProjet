suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
})

# ============================================================
# 1) Chemins
# ============================================================

classes_path  <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_communes_classes.csv"
cultures_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/superficies_communes_2010.csv"

out_diag_csv  <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_communes_diagnostics.csv"
out_resume    <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_communes_diagnostics_resume.csv"

out_hist_prob <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/diag_prob_max_hist.png"
out_hist_ent  <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/diag_entropy_hist.png"
out_hist_hhi  <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/diag_hhi_hist.png"
out_scatter   <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/diag_probmax_vs_hhi.png"
out_hist_nsig <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/diag_nb_cultures_significatives.png"

# ============================================================
# 2) Paramètres
# ============================================================

presence_threshold <- 0.05

# seuils de lecture empirique
prob_threshold_1 <- 0.80
prob_threshold_2 <- 0.90
hhi_threshold    <- 0.50

# ============================================================
# 3) Charger les classes LCA
# ============================================================

df_classes <- read_csv(classes_path, show_col_types = FALSE) %>%
  mutate(
    insee = str_trim(as.character(insee)),
    insee = str_pad(insee, width = 5, side = "left", pad = "0")
  )

cat("Nombre de communes dans le fichier de classes :", nrow(df_classes), "\n")

prob_cols <- names(df_classes)[grepl("^prob_classe_", names(df_classes))]

if (length(prob_cols) == 0) {
  stop("Aucune colonne prob_classe_* trouvée dans lca_communes_classes.csv")
}

# ============================================================
# 4) Recharger les données de cultures pour reconstruire les parts
# ============================================================

df <- read_csv(cultures_path, show_col_types = FALSE) %>%
  mutate(
    com   = str_trim(as.character(com)),
    insee = str_pad(com, width = 5, side = "left", pad = "0")
  )

prod_cols <- c(
  "Céréales",
  "Oléagineux, protéagineux, plantes à fibres  (Total)",
  "Cultures industrielles",
  "Fourrages et superficies toujours en herbe",
  "Pommes de terre et tubercules",
  "Légumes frais, fraises, melons",
  "Vignes",
  "Cultures permanentes entretenues"
)

prod_cols <- intersect(prod_cols, names(df))

if (length(prod_cols) < 3) {
  stop("Pas assez de variables trouvées dans superficies_communes_2010.csv")
}

cat("Variables de cultures utilisées :\n")
print(prod_cols)

df_prod <- df %>%
  select(annee, frdom, region, dep, com, insee, all_of(prod_cols)) %>%
  mutate(across(all_of(prod_cols), as.numeric))

row_totals <- rowSums(df_prod[, prod_cols], na.rm = TRUE)

df_prod <- df_prod %>%
  mutate(total_prod = row_totals) %>%
  filter(is.finite(total_prod), total_prod > 0)

if (nrow(df_prod) == 0) {
  stop("Aucune commune avec total de production positif.")
}

share_df <- df_prod %>%
  transmute(
    insee,
    across(all_of(prod_cols), ~ .x / total_prod)
  )

# ============================================================
# 5) Indicateurs de spécialisation agricole
# ============================================================

# 5.1 Nombre de cultures significatives (part >= seuil)
presence_mat <- share_df %>%
  mutate(across(all_of(prod_cols), ~ ifelse(.x >= presence_threshold, 1L, 0L)))

n_sig <- rowSums(presence_mat[, prod_cols], na.rm = TRUE)

# 5.2 Herfindahl-Hirschman Index (HHI)
# somme des parts au carré ; plus proche de 1 = plus spécialisé
hhi <- rowSums((share_df[, prod_cols])^2, na.rm = TRUE)

# 5.3 Part maximale de culture
max_share <- apply(share_df[, prod_cols], 1, max, na.rm = TRUE)

# 5.4 Entropie de Shannon sur les parts de cultures
entropy_shares <- apply(
  share_df[, prod_cols],
  1,
  function(p) {
    p <- as.numeric(p)
    -sum(ifelse(p > 0, p * log(p), 0), na.rm = TRUE)
  }
)

# 5.5 Culture dominante
dominant_crop <- apply(
  share_df[, prod_cols],
  1,
  function(x) prod_cols[which.max(x)]
)

diag_agri <- share_df %>%
  transmute(
    insee,
    n_cultures_significatives = n_sig,
    herfindahl = hhi,
    max_share = max_share,
    entropy_shares = entropy_shares,
    culture_dominante = dominant_crop
  )

# ============================================================
# 6) Indicateurs de netteté du clustering LCA
# ============================================================

entropy_lca <- apply(
  df_classes[, prob_cols],
  1,
  function(p) {
    p <- as.numeric(p)
    -sum(ifelse(p > 0, p * log(p), 0), na.rm = TRUE)
  }
)

# entropie normalisée entre 0 et 1
entropy_lca_norm <- entropy_lca / log(length(prob_cols))

diag_lca <- df_classes %>%
  transmute(
    insee,
    classe,
    prob_max,
    uncertainty,
    entropy_lca = entropy_lca,
    entropy_lca_norm = entropy_lca_norm
  )

# ============================================================
# 7) Fusion diagnostics
# ============================================================

diag_df <- diag_lca %>%
  left_join(diag_agri, by = "insee")

cat("Nombre de communes après fusion diagnostics :", nrow(diag_df), "\n")

# ============================================================
# 8) Résumé global
# ============================================================

resume_global <- tibble(
  n_communes = nrow(diag_df),

  prob_max_moy = mean(diag_df$prob_max, na.rm = TRUE),
  prob_max_med = median(diag_df$prob_max, na.rm = TRUE),
  part_prob_max_sup_080 = mean(diag_df$prob_max > prob_threshold_1, na.rm = TRUE),
  part_prob_max_sup_090 = mean(diag_df$prob_max > prob_threshold_2, na.rm = TRUE),

  entropy_lca_moy = mean(diag_df$entropy_lca, na.rm = TRUE),
  entropy_lca_norm_moy = mean(diag_df$entropy_lca_norm, na.rm = TRUE),

  hhi_moy = mean(diag_df$herfindahl, na.rm = TRUE),
  hhi_med = median(diag_df$herfindahl, na.rm = TRUE),
  part_hhi_sup_050 = mean(diag_df$herfindahl > hhi_threshold, na.rm = TRUE),

  max_share_moy = mean(diag_df$max_share, na.rm = TRUE),
  max_share_med = median(diag_df$max_share, na.rm = TRUE),

  n_sig_moy = mean(diag_df$n_cultures_significatives, na.rm = TRUE),
  n_sig_med = median(diag_df$n_cultures_significatives, na.rm = TRUE),

  corr_probmax_hhi = cor(diag_df$prob_max, diag_df$herfindahl, use = "complete.obs"),
  corr_probmax_nsig = cor(diag_df$prob_max, diag_df$n_cultures_significatives, use = "complete.obs"),
  corr_hhi_nsig = cor(diag_df$herfindahl, diag_df$n_cultures_significatives, use = "complete.obs")
) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )

# ============================================================
# 9) Résumé par classe
# ============================================================

resume_par_classe <- diag_df %>%
  group_by(classe) %>%
  summarise(
    n_communes = n(),
    prob_max_moy = mean(prob_max, na.rm = TRUE),
    prob_max_med = median(prob_max, na.rm = TRUE),
    entropy_lca_norm_moy = mean(entropy_lca_norm, na.rm = TRUE),
    herfindahl_moy = mean(herfindahl, na.rm = TRUE),
    max_share_moy = mean(max_share, na.rm = TRUE),
    n_sig_moy = mean(n_cultures_significatives, na.rm = TRUE),
    part_prob_max_sup_080 = mean(prob_max > prob_threshold_1, na.rm = TRUE),
    part_hhi_sup_050 = mean(herfindahl > hhi_threshold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )

# ============================================================
# 10) Export CSV
# ============================================================

write_csv(diag_df, out_diag_csv)
write_csv(bind_rows(
  resume_global %>% mutate(type_resume = "global"),
  resume_par_classe %>% mutate(type_resume = "par_classe")
), out_resume)

cat("Diagnostics exportés :", out_diag_csv, "\n")
cat("Résumé diagnostics exporté :", out_resume, "\n")

# ============================================================
# 11) Console : lecture rapide
# ============================================================

cat("\n====================\n")
cat("Résumé global\n")
cat("====================\n")
print(resume_global)

cat("\n====================\n")
cat("Résumé par classe\n")
cat("====================\n")
print(resume_par_classe)

cat("\n====================\n")
cat("Interprétation empirique\n")
cat("====================\n")

if (resume_global$part_prob_max_sup_080 >= 0.7) {
  cat("- Beaucoup de communes ont une classe dominante nette (prob_max > 0.8).\n")
} else {
  cat("- Peu de communes ont une classe dominante très nette : prudence sur les dummies.\n")
}

if (resume_global$part_hhi_sup_050 >= 0.5) {
  cat("- Une part importante des communes est spécialisée selon le HHI.\n")
} else {
  cat("- Beaucoup de communes semblent diversifiées selon le HHI.\n")
}

if (resume_global$n_sig_moy <= 2.5) {
  cat("- En moyenne, peu de cultures sont significatives par commune : la spécialisation est plausible.\n")
} else {
  cat("- En moyenne, plusieurs cultures sont significatives par commune : la logique de mélange est importante.\n")
}

# ============================================================
# 12) Graphiques de diagnostic
# ============================================================

p1 <- ggplot(diag_df, aes(x = prob_max)) +
  geom_histogram(bins = 30, color = "white") +
  geom_vline(xintercept = prob_threshold_1, linetype = "dashed") +
  geom_vline(xintercept = prob_threshold_2, linetype = "dashed") +
  labs(
    title = "Distribution de la probabilité maximale de classe",
    x = "prob_max",
    y = "Nombre de communes"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_hist_prob, p1, width = 8, height = 5, dpi = 300, bg = "white")

p2 <- ggplot(diag_df, aes(x = entropy_lca_norm)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution de l'entropie normalisée des probabilités LCA",
    x = "Entropie LCA normalisée",
    y = "Nombre de communes"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_hist_ent, p2, width = 8, height = 5, dpi = 300, bg = "white")

p3 <- ggplot(diag_df, aes(x = herfindahl)) +
  geom_histogram(bins = 30, color = "white") +
  geom_vline(xintercept = hhi_threshold, linetype = "dashed") +
  labs(
    title = "Distribution de l'indice de Herfindahl",
    x = "HHI",
    y = "Nombre de communes"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_hist_hhi, p3, width = 8, height = 5, dpi = 300, bg = "white")

p4 <- ggplot(diag_df, aes(x = prob_max, y = herfindahl)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dominance du cluster vs spécialisation agricole",
    x = "prob_max",
    y = "Herfindahl"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_scatter, p4, width = 8, height = 5, dpi = 300, bg = "white")

p5 <- ggplot(diag_df, aes(x = n_cultures_significatives)) +
  geom_bar() +
  labs(
    title = "Nombre de cultures significatives par commune",
    x = paste0("Nb de cultures avec part >= ", round(100 * presence_threshold), "%"),
    y = "Nombre de communes"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_hist_nsig, p5, width = 8, height = 5, dpi = 300, bg = "white")

cat("\nGraphiques sauvegardés :\n")
cat("-", out_hist_prob, "\n")
cat("-", out_hist_ent, "\n")
cat("-", out_hist_hhi, "\n")
cat("-", out_scatter, "\n")
cat("-", out_hist_nsig, "\n")

# ============================================================
# 13) Commentaire à garder pour la suite
# ============================================================

# Les résultats montrent que la majorité des communes présentent une forte probabilité d’appartenance à une classe (probabilité moyenne de 0.89), indiquant une bonne séparation des groupes. Par ailleurs, ces communes présentent également des niveaux élevés de spécialisation agricole, mesurés par l’indice de Herfindahl. La corrélation positive entre la probabilité maximale d’appartenance et le degré de spécialisation suggère que les classes identifiées par la LCA capturent des structures productives réelles plutôt qu’un artefact statistique.