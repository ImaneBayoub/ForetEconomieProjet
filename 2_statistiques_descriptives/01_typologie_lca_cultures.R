# -----------------------------------------------------------------------------
# 01_typologie_lca_cultures.R
# Construire une typologie agricole communale par analyse en classes latentes
# à partir des superficies cultivées en 2010
# -----------------------------------------------------------------------------
# Objectif :
#   Ce script construit une typologie des communes selon la composition de leurs
#   surfaces agricoles en grandes catégories de cultures. Il utilise une analyse
#   en classes latentes (LCA) appliquée à des indicateurs binaires de présence
#   significative des cultures.
#
# Principe :
#   1. On part des superficies communales par grandes catégories de cultures.
#   2. On calcule, pour chaque commune, la part de chaque catégorie dans la
#      surface agricole retenue.
#   3. On transforme ces parts en indicateurs binaires :
#        - 1 : culture absente ou peu significative
#        - 2 : culture significativement présente
#   4. On estime plusieurs modèles LCA avec différents nombres de classes.
#   5. On retient le modèle ayant le plus faible BIC.
#   6. On attribue chaque commune à une classe agricole.
#   7. On produit des tableaux de synthèse et une heatmap d’interprétation.
#
# Sorties :
#   output/tables/lca_communes_classes.csv
#     Table communale contenant la classe LCA attribuée à chaque commune,
#     la probabilité maximale d'appartenance et l'incertitude de classification.
#
#   output/tables/lca_communes_summary.csv
#     Résumé des classes : taille, part des communes, présence moyenne des
#     différentes catégories de cultures et parts moyennes de surfaces.
#
#   output/tables/lca_communes_fit.csv
#     Comparaison des modèles LCA estimés selon le nombre de classes :
#     log-vraisemblance, AIC, BIC, G² et Chi².
#
#   output/figures/lca_communes_heatmap.png
#     Carte thermique représentant, pour chaque classe, la fréquence de présence
#     significative des catégories de cultures.
#
# Fichier d'entrée :
#   data/interim/superficies_communes_2010.parquet
#
# Remarques :
#   - La typologie obtenue est descriptive. Elle ne constitue pas directement
#     une estimation causale.
#   - Elle est ensuite utilisée comme facteur d'hétérogénéité
#     dans les estimations de l'effet de la forêt ou de la lisière.
# -----------------------------------------------------------------------------

source("R/packages.R")
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
summarise <- dplyr::summarise

# ============================================================
# 1) Chemins
# ============================================================

cultures_path <- "data/interim/superficies_communes_2010.parquet"

out_classes <- "output/tables/lca_communes_classes.csv"
out_summary <- "output/tables/lca_communes_summary.csv"
out_plot    <- "output/figures/lca_communes_heatmap.png"
out_fit     <- "output/tables/lca_communes_fit.csv"

# ============================================================
# 2) Paramètres
# ============================================================

set.seed(123)

presence_threshold <- 0.05
nclass_grid <- 2:6
nrep_lca <- 20

# ============================================================
# 3) Charger les données
# ============================================================

df <- read_parquet(cultures_path, show_col_types = FALSE) %>%
  mutate(
    com = str_trim(as.character(com)),
    insee = str_pad(com, width = 5, side = "left", pad = "0")
  )

# ============================================================
# 4) Variables retenues
# ============================================================

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
  stop("Pas assez de variables trouvées dans superficies_communes_2010.parquet")
}

cat("Variables retenues :\n")
print(prod_cols)

# ============================================================
# 5) Construire les parts
# ============================================================

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
  transmute(across(all_of(prod_cols), ~ .x / total_prod))

# ============================================================
# 6) Transformer en binaire : présence significative
# ============================================================

presence_df <- share_df %>%
  mutate(across(
    everything(),
    ~ ifelse(.x >= presence_threshold, 2L, 1L)
  ))

# poLCA attend des catégories 1,2,... et pas 0/1
# 1 = absent / non significatif
# 2 = présent significatif

# noms courts
presence_names <- c(
  cereales = "Céréales",
  ol_prot_fibres = "Oléagineux, protéagineux, plantes à fibres  (Total)",
  cult_indus = "Cultures industrielles",
  fourrages = "Fourrages et superficies toujours en herbe",
  pommes_tub = "Pommes de terre et tubercules",
  legumes_frais = "Légumes frais, fraises, melons",
  vignes = "Vignes",
  cult_perm = "Cultures permanentes entretenues"
)

# garder seulement celles présentes dans les colonnes
presence_names <- presence_names[presence_names %in% names(presence_df)]

# rename attend : nouveau_nom = ancien_nom
rename_map <- setNames(as.list(unname(presence_names)), names(presence_names))

presence_df <- presence_df %>%
  rename(!!!rename_map)

manifest_vars <- names(presence_df)

cat("\nVariables binaires utilisées dans la LCA :\n")
print(manifest_vars)

# ============================================================
# 7) Formule poLCA
# ============================================================

f_lca <- as.formula(
  paste0("cbind(", paste(manifest_vars, collapse = ", "), ") ~ 1")
)

# ============================================================
# 8) Estimer plusieurs modèles et choisir par BIC
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
# 9) Affectation des classes
# ============================================================

df_classes <- df_prod %>%
  select(annee, frdom, region, dep, com, insee, total_prod) %>%
  mutate(
    classe = best_fit$predclass,
    prob_max = apply(best_fit$posterior, 1, max),
    uncertainty = 1 - prob_max
  )

posterior_df <- as.data.frame(best_fit$posterior)
names(posterior_df) <- paste0("prob_classe_", seq_len(ncol(posterior_df)))

df_classes <- bind_cols(df_classes, posterior_df)

# ============================================================
# 10) Résumé interprétable
# ============================================================

summary_presence <- bind_cols(
  df_classes %>% select(insee, classe, prob_max, uncertainty),
  presence_df
) %>%
  group_by(classe) %>%
  summarise(
    n_communes = n(),
    part_communes = n() / nrow(df_classes),
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
  df_classes %>% select(insee, classe),
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
    part_communes = round(100 * part_communes, 2),
    prob_max_moy = round(prob_max_moy, 3),
    uncertainty_moy = round(uncertainty_moy, 3)
  ) %>%
  mutate(
    across(starts_with("presence_"), ~ round(100 * .x, 1)),
    across(starts_with("share_"), ~ round(100 * .x, 1))
  ) %>%
  arrange(classe)

# ============================================================
# 11) Export
# ============================================================

write_csv(df_classes, out_classes)
write_csv(df_summary, out_summary)

cat("\nClasses exportées :", out_classes, "\n")
cat("Résumé exporté :", out_summary, "\n")

# ============================================================
# 12) Heatmap
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
    categorie = recode(
      categorie,
      cereales = "Céréales",
      ol_prot_fibres = "Oléagineux/protéagineux/fibres",
      cult_indus = "Cultures industrielles",
      fourrages = "Fourrages/prairies",
      pommes_tub = "Pommes de terre/tubercules",
      legumes_frais = "Légumes frais",
      vignes = "Vignes",
      cult_perm = "Cultures permanentes"
    ),
    classe = factor(classe)
  )

p <- ggplot(plot_df, aes(x = categorie, y = classe, fill = pct_presence)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct_presence), "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "#1f78b4") +
  labs(
    title = "Latent class analysis des communes",
    subtitle = paste0(
      "Présence significative (part ≥ ", round(100 * presence_threshold), "%) | ",
      best_k, " classes retenues par BIC"
    ),
    x = NULL,
    y = "Classe",
    fill = "% de communes\navec présence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 30, hjust = 1),
    text = element_text(color = "black")
  )

ggsave(out_plot, p, width = 10, height = 5, dpi = 300, bg = "white")

cat("Heatmap sauvegardé :", out_plot, "\n")

# ============================================================
# 13) Console
# ============================================================

cat("\nTaille des classes :\n")
print(table(df_classes$classe))

cat("\nPart des classes (% des communes) :\n")
print(round(100 * prop.table(table(df_classes$classe)), 2))

cat("\nRésumé :\n")
print(df_summary)

# classe 1 : très céréalière + fourrages + oléagineux/protéagineux très présents
# classe 2 : très fourragère/prairies, céréales assez fréquentes
# classe 3 : céréales + cultures industrielles + oléagineux/protéagineux
# classe 5 : légumes frais et pommes de terre/tubercules beaucoup plus présents
# classe 6 : profil très marqué par les vignes
# classe 4 : plutôt mixte, avec cultures permanentes plus fréquentes

# ============================================================
# Interprétation des clusters
# ============================================================

# Les classes LCA peuvent être relues selon une opposition
# "cultures annuelles" vs "cultures pérennes".

# Règle générale :
# - Annuelles : céréales, cultures industrielles, oléagineux/protéagineux,
#   légumes frais, pommes de terre/tubercules
# - Pérennes : vignes, cultures permanentes
# - Intermédiaire : fourrages/prairies (profil à part, plutôt élevage)

# Lecture possible des 6 classes :
# - Classe 1 : plutôt annuel mixte
# - Classe 2 : intermédiaire / fourrages-prairies
# - Classe 3 : annuel dominant (grandes cultures)
# - Classe 4 : plutôt pérenne
# - Classe 5 : annuel diversifié
# - Classe 6 : pérenne dominant (vigne)

# Regroupement cohérent pour économétrie :
# - type_annuel  = classes 1, 3, 5
# - type_mixte   = classe 2
# - type_perenne = classes 4, 6

# Hypothèse de travail pour la suite :
# - les cultures pérennes devraient être plus sensibles aux effets
#   de la forêt via les services écosystémiques (pollinisation,
#   régulation des ravageurs, microclimat, etc.)
# - les cultures annuelles devraient réagir plus faiblement,
#   ou avec une dynamique différente
# - les fourrages/prairies constituent un cas intermédiaire