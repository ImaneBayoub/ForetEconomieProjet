suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(poLCA)
})

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
summarise <- dplyr::summarise

# ============================================================
# 1) Chemins
# ============================================================

cultures_2000_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/superficies_communes_2000_detailees.csv"
cultures_2010_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/superficies_communes_2010_detailees.csv"

out_classes_all   <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_2000_2010_classes.csv"
out_summary       <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_2000_2010_summary.csv"
out_fit           <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_2000_2010_fit.csv"
out_transition    <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_2000_2010_transition_matrix.csv"
out_transition_pct <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_2000_2010_transition_matrix_pct.csv"
out_transition_long <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_2000_2010_transitions_communes.csv"

out_plot_heatmap  <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/lca_2000_2010_heatmap.png"
out_plot_trans    <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/lca_2000_2010_transition_heatmap.png"

# ============================================================
# 2) Paramètres
# ============================================================

set.seed(123)

presence_threshold <- 0.05
nclass_grid <- 2:6
nrep_lca <- 20

# ============================================================
# 3) Fonction de préparation
# ============================================================

prepare_year <- function(path, year_expected = NULL) {
  df <- read_csv(path, show_col_types = FALSE)

  if (!"com" %in% names(df)) {
    stop("La colonne 'com' est absente de : ", path)
  }

  df <- df %>%
    mutate(
      com = str_trim(as.character(com)),
      insee = str_pad(com, width = 5, side = "left", pad = "0")
    )

  if ("annee" %in% names(df) && !is.null(year_expected)) {
    df <- df %>% mutate(annee = as.integer(annee))
  } else if (!is.null(year_expected)) {
    df <- df %>% mutate(annee = as.integer(year_expected))
  }

  df
}

# ============================================================
# 4) Charger les deux années
# ============================================================

df_2000 <- prepare_year(cultures_2000_path, 2000)
df_2010 <- prepare_year(cultures_2010_path, 2010)

cat("Dimensions 2000 :", dim(df_2000), "\n")
cat("Dimensions 2010 :", dim(df_2010), "\n")

# ============================================================
# 5) Harmoniser les colonnes de culture
# ============================================================

id_candidates <- c("annee", "frdom", "region", "dep", "com", "insee")

cult_cols_2000 <- setdiff(names(df_2000), id_candidates)
cult_cols_2010 <- setdiff(names(df_2010), id_candidates)

common_prod_cols <- intersect(cult_cols_2000, cult_cols_2010)

if (length(common_prod_cols) < 3) {
  stop("Pas assez de catégories communes entre 2000 et 2010.")
}

cat("Nombre de catégories communes :", length(common_prod_cols), "\n")

# on enlève explicitement les variables trop agrégées si présentes
bad_patterns <- c(
  "^Superficie agricole utilisée",
  "^Surface agricole utilisée",
  "^SAU",
  "^Superficie totale",
  "^Total$",
  "^Ensemble",
  "^Exploitations"
)

keep_cols <- common_prod_cols[
  !Reduce(`|`, lapply(bad_patterns, function(p) str_detect(common_prod_cols, regex(p, ignore_case = TRUE))))
]

cat("Nombre de catégories après exclusion des agrégats :", length(keep_cols), "\n")

if (length(keep_cols) < 3) {
  stop("Trop peu de catégories restantes après exclusion des agrégats.")
}

# ============================================================
# 6) Restreindre aux communes présentes aux deux dates
# ============================================================

common_insee <- intersect(unique(df_2000$insee), unique(df_2010$insee))

df_2000 <- df_2000 %>% filter(insee %in% common_insee)
df_2010 <- df_2010 %>% filter(insee %in% common_insee)

cat("Nombre de communes communes aux deux années :", length(common_insee), "\n")

# ============================================================
# 7) Construire la base empilée
# ============================================================

df_all <- bind_rows(
  df_2000 %>% mutate(time = 0L),
  df_2010 %>% mutate(time = 1L)
) %>%
  select(any_of(c("annee", "frdom", "region", "dep", "com", "insee", "time")), all_of(keep_cols)) %>%
  mutate(across(all_of(keep_cols), as.numeric))

# ============================================================
# 8) Parts de surface
# ============================================================

row_totals <- rowSums(df_all[, keep_cols], na.rm = TRUE)

df_prod <- df_all %>%
  mutate(total_prod = row_totals) %>%
  filter(is.finite(total_prod), total_prod > 0)

if (nrow(df_prod) == 0) {
  stop("Aucune ligne avec total de production positif.")
}

# communes observées aux deux dates après total > 0
valid_both_dates <- df_prod %>%
  count(insee, time) %>%
  tidyr::pivot_wider(names_from = time, values_from = n, values_fill = 0) %>%
  filter(`0` > 0, `1` > 0) %>%
  pull(insee)

df_prod <- df_prod %>% filter(insee %in% valid_both_dates)

cat("Nombre de communes avec production positive aux deux dates :", n_distinct(df_prod$insee), "\n")
cat("Nombre de lignes dans la base empilée :", nrow(df_prod), "\n")

share_df <- df_prod %>%
  transmute(across(all_of(keep_cols), ~ .x / total_prod))

# ============================================================
# 9) Binarisation pour LCA
# ============================================================

presence_df <- share_df %>%
  mutate(across(
    everything(),
    ~ ifelse(.x >= presence_threshold, 2L, 1L)
  ))

# créer des noms courts sûrs pour poLCA
make_safe_name <- function(x) {
  x %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

safe_names <- make_safe_name(keep_cols)

# s'assurer qu'ils sont uniques
if (anyDuplicated(safe_names) > 0) {
  safe_names <- make.unique(safe_names, sep = "_")
}

rename_map <- setNames(as.list(keep_cols), safe_names)

presence_df <- presence_df %>%
  rename(!!!rename_map)

manifest_vars <- names(presence_df)

cat("Variables utilisées dans la LCA :", length(manifest_vars), "\n")
print(manifest_vars)

# ============================================================
# 10) Formule LCA
# ============================================================

f_lca <- as.formula(
  paste0("cbind(", paste(manifest_vars, collapse = ", "), ") ~ 1")
)

# ============================================================
# 11) Estimer plusieurs modèles et choisir par BIC
# ============================================================

fit_results <- list()
fit_stats <- list()

for (k in nclass_grid) {
  cat("\nEstimation LCA avec", k, "classes...\n")

  fit_k <- poLCA(
    formula = f_lca,
    data = presence_df,
    nclass = k,
    nrep = nrep_lca,
    verbose = FALSE,
    na.rm = FALSE,
    maxiter = 5000
  )

  fit_results[[as.character(k)]] <- fit_k

  fit_stats[[as.character(k)]] <- data.frame(
    nclass = k,
    logLik = fit_k$llik,
    AIC = fit_k$aic,
    BIC = fit_k$bic,
    Gsq = fit_k$Gsq,
    Chisq = fit_k$Chisq
  )
}

fit_table <- bind_rows(fit_stats) %>% arrange(BIC)
write_csv(fit_table, out_fit)

cat("\nTable des modèles exportée :", out_fit, "\n")
print(fit_table)

best_k <- fit_table$nclass[1]
best_fit <- fit_results[[as.character(best_k)]]

cat("\nNombre de classes retenu par BIC :", best_k, "\n")

# ============================================================
# 12) Affectation des classes
# ============================================================

df_classes <- df_prod %>%
  select(annee, time, frdom, region, dep, com, insee, total_prod) %>%
  mutate(
    classe = best_fit$predclass,
    prob_max = apply(best_fit$posterior, 1, max),
    uncertainty = 1 - prob_max
  )

posterior_df <- as.data.frame(best_fit$posterior)
names(posterior_df) <- paste0("prob_classe_", seq_len(ncol(posterior_df)))

df_classes <- bind_cols(df_classes, posterior_df)

write_csv(df_classes, out_classes_all)
cat("Classes exportées :", out_classes_all, "\n")

# ============================================================
# 13) Résumé des classes
# ============================================================

summary_presence <- bind_cols(
  df_classes %>% select(insee, annee, time, classe, prob_max, uncertainty),
  presence_df
) %>%
  group_by(classe) %>%
  summarise(
    n_obs = n(),
    n_communes = n_distinct(insee),
    part_obs = n() / nrow(df_classes),
    prob_max_moy = mean(prob_max, na.rm = TRUE),
    uncertainty_moy = mean(uncertainty, na.rm = TRUE),
    across(
      all_of(manifest_vars),
      ~ mean(.x == 2, na.rm = TRUE),
      .names = "presence_{.col}"
    ),
    .groups = "drop"
  )

summary_shares <- bind_cols(
  df_classes %>% select(insee, annee, time, classe),
  share_df %>% rename(!!!rename_map)
) %>%
  group_by(classe) %>%
  summarise(
    across(
      all_of(manifest_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "share_{.col}"
    ),
    .groups = "drop"
  )

df_summary <- summary_presence %>%
  left_join(summary_shares, by = "classe") %>%
  mutate(
    part_obs = round(100 * part_obs, 2),
    prob_max_moy = round(prob_max_moy, 3),
    uncertainty_moy = round(uncertainty_moy, 3)
  ) %>%
  mutate(
    across(starts_with("presence_"), ~ round(100 * .x, 1)),
    across(starts_with("share_"), ~ round(100 * .x, 1))
  ) %>%
  arrange(classe)

write_csv(df_summary, out_summary)
cat("Résumé exporté :", out_summary, "\n")

# ============================================================
# 14) Transitions 2000 -> 2010
# ============================================================

transitions <- df_classes %>%
  select(insee, annee, time, classe, prob_max, uncertainty) %>%
  select(insee, time, classe) %>%
  pivot_wider(
    id_cols = insee,
    names_from = time,
    values_from = classe,
    names_prefix = "t"
  ) %>%
  filter(!is.na(t0), !is.na(t1)) %>%
  mutate(
    same_class = t0 == t1
  )

cat("\nNombre de communes comparables dans la matrice de transition :", nrow(transitions), "\n")
cat("Part restant dans la meme classe (%): ", round(100 * mean(transitions$same_class), 2), "\n")

transition_mat <- table(transitions$t0, transitions$t1)
transition_df <- as.data.frame.matrix(transition_mat)
write.csv(transition_df, out_transition, row.names = TRUE)

transition_pct <- prop.table(transition_mat, margin = 1) * 100
transition_pct_df <- as.data.frame.matrix(round(transition_pct, 2))
write.csv(transition_pct_df, out_transition_pct, row.names = TRUE)

write_csv(transitions, out_transition_long)

cat("Matrice de transition exportée :", out_transition, "\n")
cat("Matrice de transition (%) exportée :", out_transition_pct, "\n")
cat("Transitions individuelles exportées :", out_transition_long, "\n")

print(transition_mat)
print(round(transition_pct, 2))

# ============================================================
# 15) Heatmap des classes LCA
# ============================================================

plot_df <- df_summary %>%
  select(classe, starts_with("presence_")) %>%
  pivot_longer(
    cols = starts_with("presence_"),
    names_to = "categorie",
    values_to = "pct_presence"
  ) %>%
  mutate(
    categorie = str_remove(categorie, "^presence_"),
    categorie = factor(categorie, levels = manifest_vars),
    classe = factor(classe)
  )

p_heatmap <- ggplot(plot_df, aes(x = categorie, y = classe, fill = pct_presence)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct_presence), "%")), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "#1f78b4") +
  labs(
    title = "LCA commune sur 2000 et 2010",
    subtitle = paste0(
      "Présence significative (part >= ", round(100 * presence_threshold), "%) | ",
      best_k, " classes retenues par BIC"
    ),
    x = NULL,
    y = "Classe",
    fill = "% d'observations\navec présence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(color = "black")
  )

ggsave(out_plot_heatmap, p_heatmap, width = 13, height = 6, dpi = 300, bg = "white")
cat("Heatmap LCA sauvegardée :", out_plot_heatmap, "\n")

# ============================================================
# 16) Heatmap de transition
# ============================================================

transition_plot_df <- as.data.frame(transition_pct) %>%
  rename(
    classe_2000 = Var1,
    classe_2010 = Var2,
    pct = Freq
  )

p_trans <- ggplot(transition_plot_df, aes(x = factor(classe_2010), y = factor(classe_2000), fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct), "%")), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "#238b45") +
  labs(
    title = "Transition des classes LCA entre 2000 et 2010",
    subtitle = "Lignes = classe en 2000 ; colonnes = classe en 2010",
    x = "Classe 2010",
    y = "Classe 2000",
    fill = "% ligne"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black")
  )

ggsave(out_plot_trans, p_trans, width = 7, height = 6, dpi = 300, bg = "white")
cat("Heatmap de transition sauvegardée :", out_plot_trans, "\n")

# ============================================================
# 17) Console
# ============================================================

cat("\nTaille des classes :\n")
print(table(df_classes$classe))

cat("\nPart des classes (% des observations empilees) :\n")
print(round(100 * prop.table(table(df_classes$classe)), 2))

cat("\nPart des communes restant dans la meme classe :\n")
print(round(100 * mean(transitions$same_class), 2))

cat("\nRésumé des classes :\n")
print(df_summary)

cat("\nMatrice de transition (comptes) :\n")
print(transition_mat)

cat("\nMatrice de transition (% par ligne) :\n")
print(round(transition_pct, 2))

# On a vérifié la stabilité du type de production agricole des communes avec deux approches complémentaires. D’abord, à partir de CORINE Land Cover, on a distingué les cultures annuelles et pérennes en comparant 2012 et 2018. Le résultat montre qu’il n’y a pratiquement pas d’évolution structurelle : les parts annuelles et pérennes restent globalement stables, sans bascule systématique d’un type vers l’autre. Ensuite, on a refait l’exercice avec une information beaucoup plus fine issue d’Agreste, en construisant une classification latente (LCA) des communes selon leurs catégories détaillées de culture, estimée conjointement sur 2000 et 2010 pour rendre les classes comparables dans le temps. Cette LCA retient 6 classes, et la matrice de transition montre qu’environ 75 % des communes restent dans la même classe entre 2000 et 2010 ; les transitions observées se font surtout entre classes proches, ce qui indique des ajustements internes plutôt qu’une transformation profonde des systèmes de production. Au total, les deux méthodes convergent : que l’on raisonne avec une typologie simple annuelle/pérenne issue de CLC ou avec une classification plus fine issue des données Agreste, la structure productive agricole communale apparaît fortement persistante dans le temps.