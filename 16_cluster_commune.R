# ============================================================
# Clustering communes (agri/forêt 1990 & 2012) + plot trajectoires
# + carte colorée par cluster
# % calculés sur pixels_total_1990 / pixels_total_2012
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(sf)
  library(cluster)
})

# ---- 0) Chemins ----
csv_path  <- "/home/imane/Documents/ensae/ForetEconomieProjet/out_communes/indicateurs_communes_clc_1990_2012.csv"

gpkg_path <- "data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"
gpkg_layer <- "COMMUNE"

k_min <- 3
k_max <- 10
set.seed(123)

# ---- 1) Charger le CSV ----
# ---- 1) Charger le CSV ----
df <- read.csv(csv_path, stringsAsFactors = FALSE) %>%
  mutate(
    insee = as.character(insee),
    # totals
    pixels_total_1990 = as.numeric(pixels_total_1990),
    pixels_total_2012 = as.numeric(pixels_total_2012),
    # stocks (il faut 1990 ET 2012 pour calculer les deltas et tracer les flèches)
    agri_1990  = as.numeric(agri_1990),
    agri_2012  = as.numeric(agri_2012),
    foret_1990 = as.numeric(foret_1990),
    foret_2012 = as.numeric(foret_2012)
  )

# ---- 2) Calcul des % + deltas ----
df_feat <- df %>%
  filter(pixels_total_1990 > 0, pixels_total_2012 > 0) %>%
  mutate(
    pct_agri_1990  = 100 * agri_1990  / pixels_total_1990,
    pct_foret_1990 = 100 * foret_1990 / pixels_total_1990,
    pct_agri_2012  = 100 * agri_2012  / pixels_total_2012,
    pct_foret_2012 = 100 * foret_2012 / pixels_total_2012,
    d_agri  = pct_agri_2012  - pct_agri_1990,
    d_foret = pct_foret_2012 - pct_foret_1990
  ) %>%
  filter(
    is.finite(pct_agri_1990), is.finite(pct_foret_1990),
    is.finite(pct_agri_2012), is.finite(pct_foret_2012),
    is.finite(d_agri), is.finite(d_foret)
  )

# ---- 3) Features pour le clustering (CE QUE TU VEUX) ----
X <- df_feat %>%
  select(pct_agri_1990, pct_foret_1990, d_agri, d_foret) %>%
  as.matrix()

X_scaled <- scale(X)

# ---- 4) Choix de k via silhouette ----
ks <- k_min:k_max
sil_scores <- map_dbl(ks, function(k){
  km <- kmeans(X_scaled, centers = k, nstart = 50, iter.max = 100)
  ss <- silhouette(km$cluster, dist(X_scaled))
  mean(ss[, 3])
})

best_k <- ks[which.max(sil_scores)]
cat("Best k (silhouette):", best_k, "\n")

# ---- 5) K-means final ----
km_final <- kmeans(X_scaled, centers = best_k, nstart = 100, iter.max = 200)

df_cluster <- df_feat %>%
  mutate(cluster = factor(km_final$cluster))

# ============================================================
# 6) Plot trajectoires MOYENNES par cluster (centroïdes + flèches)
# ============================================================

# ---- Palette sobre et contrastée ----
cluster_colors <- c(
  "1" = "#6C757D",  # gris neutre (faible forêt & agri)
  "2" = "#8b5615",  # jaune agriculture dominante
  "3" = "#E0A800",  # ocre agricole stable
  "4" = "#4CAF50",  # vert forêt importante
  "5" = "#1B5E20",  # vert foncé très forestier
  "6" = "#F57C00"   # orange transition / déprise
)

# Palette dédiée aux deux clusters (sobre + contrastée)
cluster_colors_46 <- c(
  "4" = "#4CAF50",  # vert forêt importante
  "6" = "#F57C00"   # orange transition / déprise
)

centroids <- df_cluster %>%
  group_by(cluster) %>%
  summarise(
    agri_1990  = mean(pct_agri_1990,  na.rm = TRUE),
    foret_1990 = mean(pct_foret_1990, na.rm = TRUE),
    agri_2012  = mean(pct_agri_2012,  na.rm = TRUE),
    foret_2012 = mean(pct_foret_2012, na.rm = TRUE),
    .groups = "drop"
  )

p_traj_centroids <- ggplot() +
  geom_segment(
    data = centroids,
    aes(x = agri_1990, y = foret_1990,
        xend = agri_2012, yend = foret_2012,
        color = cluster),
    arrow = arrow(length = unit(0.18, "cm")),
    linewidth = 1.2
  ) +
  geom_point(data = centroids,
             aes(agri_1990, foret_1990, color = cluster),
             size = 3) +
  geom_point(data = centroids,
             aes(agri_2012, foret_2012, color = cluster),
             size = 3, shape = 17) +
  scale_color_manual(values = cluster_colors) +
  labs(
    x = "% pixels agricoles",
    y = "% pixels forêt",
    title = "Trajectoires moyennes par cluster (1990 -> 2012)",
    color = "Cluster"
  ) +
  theme_minimal()
# ============================================================
# 6bis) (Optionnel) Ancien spaghetti plot (illisible si beaucoup de communes)
# ============================================================
# df_long <- df_cluster %>%
#   select(insee, nom,
#          pct_agri_1990, pct_foret_1990,
#          pct_agri_2012, pct_foret_2012,
#          cluster) %>%
#   pivot_longer(
#     cols = starts_with("pct_"),
#     names_to = c("var", "annee"),
#     names_pattern = "pct_(.*)_(\\d{4})"
#   ) %>%
#   pivot_wider(names_from = var, values_from = value) %>%
#   rename(pct_agri = agri, pct_foret = foret)
#
# p_traj <- ggplot(df_long, aes(x = pct_agri, y = pct_foret, group = insee)) +
#   geom_path(alpha = 0.25) +
#   geom_point(aes(color = cluster, shape = annee), size = 2) +
#   labs(
#     x = "% pixels agricoles (sur total pixels commune)",
#     y = "% pixels forêt (sur total pixels commune)",
#     color = "Cluster",
#     shape = "Année",
#     title = "Trajectoires agri–forêt des communes (1990 → 2012) + clusters"
#   ) +
#   theme_minimal()
# print(p_traj)

# ---- 7) Charger la géométrie et joindre ----
communes <- st_read(gpkg_path, layer = gpkg_layer, quiet = TRUE)

if (!("insee" %in% names(communes))) {
  if ("INSEE_COM" %in% names(communes)) {
    communes <- communes %>% mutate(insee = as.character(INSEE_COM))
  } else if ("code_insee" %in% names(communes)) {
    communes <- communes %>% mutate(insee = as.character(code_insee))
  } else if ("CODE_INSEE" %in% names(communes)) {
    communes <- communes %>% mutate(insee = as.character(CODE_INSEE))
  } else {
    stop("Impossible de trouver le code INSEE dans la couche COMMUNE.")
  }
}

communes_cl <- communes %>%
  left_join(df_cluster %>% select(insee, cluster), by = "insee")

communes_cl <- communes_cl %>% filter(!is.na(cluster))

# ---- Carte filtrée: seulement clusters 4 et 6 ----
communes_46 <- communes_cl %>%
  filter(cluster %in% c("4", "6")) %>%
  mutate(cluster = droplevels(cluster))  # enlève les niveaux inutiles

# ---- 8) Carte par cluster ----
p_map <- ggplot(communes_cl) +
  geom_sf(aes(fill = cluster), linewidth = 0.05) +
  scale_fill_manual(values = cluster_colors) +
  labs(
    fill = "Cluster",
    title = "Clusters de communes (agri/forêt)",
    subtitle = paste0("k = ", best_k, " choisi via silhouette")
  ) +
  theme_minimal()

# ---- 9) Sauvegarde ----
ggsave("plots/traj_moyennes_clusters_agri_foret.png", p_traj_centroids, width = 8, height = 6, dpi = 300)
ggsave("plots/plotmap_clusters_agri_foret_sur_total.png", p_map, width = 8, height = 8, dpi = 300)

# ============================================================
# 10) Résumé par cluster (agri/forêt) - NE PAS exporter ici
#     (on exportera à la fin après ajout de la productivité)
# ============================================================
resume_clusters <- df_cluster %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    agri_1990  = mean(pct_agri_1990, na.rm = TRUE),
    foret_1990 = mean(pct_foret_1990, na.rm = TRUE),
    agri_2012  = mean(pct_agri_2012, na.rm = TRUE),
    foret_2012 = mean(pct_foret_2012, na.rm = TRUE),
    d_agri     = mean(d_agri, na.rm = TRUE),
    d_foret    = mean(d_foret, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster)

# ============================================================
# 11) Ajouter l'évolution de productivité (1990 -> 2012)
#     depuis twfe_data.csv (id, time, ratio_prod_surface)
# ============================================================

prod_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/twfe_data.csv"

df_prod <- read.csv(prod_path, stringsAsFactors = FALSE) %>%
  mutate(
    id = as.character(id),
    time = as.numeric(time),
    ratio_prod_surface = as.numeric(ratio_prod_surface)
  )

# Wide: prod_1990 (time=1), prod_2012 (time=3) + delta
df_prod_wide <- df_prod %>%
  filter(time %in% c(1, 3)) %>%
  select(id, time, ratio_prod_surface) %>%
  distinct() %>%
  pivot_wider(
    names_from = time,
    values_from = ratio_prod_surface,
    names_prefix = "t"
  ) %>%
  rename(
    prod_1990 = t1,
    prod_2012 = t3
  ) %>%
  mutate(
    d_prod = prod_2012 - prod_1990
  )

# Joindre au niveau commune (utile si tu veux aussi faire des boxplots etc.)
df_cluster_prod <- df_cluster %>%
  left_join(df_prod_wide, by = c("insee" = "id"))

# Résumer par cluster
resume_prod_clusters <- df_cluster_prod %>%
  group_by(cluster) %>%
  summarise(
    n_prod = sum(is.finite(d_prod)),
    mean_prod_1990 = mean(prod_1990, na.rm = TRUE),
    mean_prod_2012 = mean(prod_2012, na.rm = TRUE),
    mean_d_prod    = mean(d_prod,    na.rm = TRUE),
    median_d_prod  = median(d_prod,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster)

# Fusion finale (agri/forêt + productivité)
resume_final <- resume_clusters %>%
  left_join(resume_prod_clusters, by = "cluster")

# Export final (UN SEUL FICHIER)
out_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/resume_clusters_agri_foret.csv"
write.csv(resume_final, out_path, row.names = FALSE)

cat(" Export final OK :", out_path, "\n")

# ============================================================
# 12) Plot trajectoires moyennes + productivité (labels lisibles)
# ============================================================

centroids_prod <- df_cluster_prod %>%
  group_by(cluster) %>%
  summarise(
    agri_1990  = mean(pct_agri_1990,  na.rm = TRUE),
    foret_1990 = mean(pct_foret_1990, na.rm = TRUE),
    agri_2012  = mean(pct_agri_2012,  na.rm = TRUE),
    foret_2012 = mean(pct_foret_2012, na.rm = TRUE),
    prod_1990  = mean(prod_1990, na.rm = TRUE),
    prod_2012  = mean(prod_2012, na.rm = TRUE),
    .groups = "drop"
  )

centroids_prod_long <- centroids_prod %>%
  pivot_longer(
    cols = c(agri_1990, foret_1990, prod_1990, agri_2012, foret_2012, prod_2012),
    names_to = c("var", "year"),
    names_pattern = "(agri|foret|prod)_(1990|2012)"
  ) %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(
    year = as.integer(year),
    prod_label = ifelse(is.finite(prod), paste0("P=", round(prod, 2)), "P=NA"),
    # décaler les labels: 1990 à gauche, 2012 à droite
    x_lab = ifelse(year == 1990, agri - 2.5, agri + 2.5),
    y_lab = foret + 1.2
  )

p_traj_centroids_prod <- ggplot() +
  # flèche 1990 -> 2012
  geom_segment(
    data = centroids_prod,
    aes(x = agri_1990, y = foret_1990,
        xend = agri_2012, yend = foret_2012,
        color = cluster),
    arrow = arrow(length = unit(0.18, "cm")),
    linewidth = 1.2
  ) +
  # points
  geom_point(
    data = centroids_prod_long,
    aes(x = agri, y = foret, color = cluster, shape = factor(year)),
    size = 3
  ) +
  # petit trait entre le point et le label (pour la lecture)
  geom_segment(
    data = centroids_prod_long,
    aes(x = agri, y = foret, xend = x_lab, yend = y_lab, color = cluster),
    linewidth = 0.4,
    show.legend = FALSE
  ) +
  # label lisible (fond blanc)
  geom_label(
    data = centroids_prod_long,
    aes(x = x_lab, y = y_lab, label = prod_label, color = cluster),
    fill = "white",
    label.size = 0.2,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cluster_colors) +
  scale_shape_manual(values = c("1990" = 16, "2012" = 17)) +
  coord_cartesian(clip = "off") +  # autorise labels hors cadre si besoin
  labs(
    x = "% pixels agricoles",
    y = "% pixels forêt",
    title = "Trajectoires moyennes par cluster (1990 → 2012) avec productivité",
    subtitle = "P = productivité moyenne (production/superficie) au point 1990 et 2012",
    color = "Cluster",
    shape = "Année"
  ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 30, 10, 30)) # marge pour labels

ggsave(
  "plots/traj_moyennes_clusters_agri_foret_productivite.png",
  p_traj_centroids_prod,
  width = 10, height = 6, dpi = 300
)

cat("Plot sauvegardé : plots/traj_moyennes_clusters_agri_foret_productivite.png\n")

# ============================================================
# 13) Export correspondance commune -> cluster
# ============================================================

out_clusters_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/commune_cluster.csv"

df_cluster_export <- df_cluster %>%
  select(insee, cluster)

write.csv(df_cluster_export, out_clusters_path, row.names = FALSE)

cat("Correspondance commune-cluster exportée :", out_clusters_path, "\n")