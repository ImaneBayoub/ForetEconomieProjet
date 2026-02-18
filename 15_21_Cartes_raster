setwd("C:/Users/Clément/Desktop/Work/projet_env/data")

#libraries
library(arrow)
library(data.table)
library(DIDmultiplegtDYN)
library(fixest)
library(ggalluvial)
library(janitor)
library(purrr)
library(tidyverse)
library(zoo)

library(sandwich)
library(lmtest)


## 15
library(sf)
library(dplyr)
library(terra)

# --- Lire les communes ---
dossier <- "C:/Users/Clément/Desktop/Work/projet_env/data/BDTOPO_FR_2009/BDTOPO/1_DONNEES_LIVRAISON_2022-04-00033/BDT_2-0_SHP_WGS84G_FRA-ED091/ADMINISTRATIF"

communes_sf <- st_read(file.path(dossier, "COMMUNE.shp"),
                       options = "ENCODING=UTF-8",
                       quiet = TRUE) |>
  rename_with(tolower)
# Si ta colonne INSEE n'a pas exactement le nom "code_insee", on la détecte
insee_col <- intersect(c("code_insee", "insee_com", "code_insee_commune", "insee"), names(communes_sf))[1]
if (is.na(insee_col)) stop("Je ne trouve pas la colonne INSEE (ex: code_insee / insee_com). Regarde names(communes_sf).")
communes_sf <- communes_sf |>
  mutate(code_insee = .data[[insee_col]],
         dep = substr(code_insee, 1, 2))
# Garder uniquement métropole + Corse
deps_metro_corse <- c(sprintf("%02d", 1:95), "2A", "2B")
communes_metro_corse <- communes_sf |>
  filter(dep %in% deps_metro_corse)

#metro_corse_union <- st_union(communes_metro_corse)


# Optionnel mais utile si soucis de géométries invalides
#metro_corse_union <- st_make_valid(metro_corse_union)

#metro_corse_union_sf <- st_sf(geometry = metro_corse_union)

# 3) Écrire sur disque (un seul fichier .gpkg)
#st_write(metro_corse_union_sf,
#         "metro_corse_union.gpkg",
#         layer = "metro_corse_union",
#         delete_layer = TRUE)
setwd("C:/Users/Clément/Desktop/Work/projet_env/data/")
## --- Plus tard : relire ---
sf_communes_metro <- st_read("metro_corse_union.gpkg", layer = "metro_corse_union")

# --- Lire rasters ---
r2 <- rast("C:/Users/Clément/Desktop/Work/projet_env/data/U2018_CLC2018_V2020_20u1/U2018_CLC2018_V2020_20u1/U2018_CLC2018_V2020_20u1.tif")
r1 <- rast("C:/Users/Clément/Desktop/Work/projet_env/data/U2000_CLC1990_V2020_20u1/U2000_CLC1990_V2020_20u1/U2000_CLC1990_V2020_20u1.tif")

# 1) Est-ce qu'il y a une table de catégories (code -> label) ?
#cats(r1)      # renvoie NULL si rien n’est stocké
#levels(r1)    # idem (souvent utilisé pour les rasters factor)
# 2) Est-ce qu'il y a une table de couleurs (code -> RGB) ?
#coltab(r1)    # renvoie NULL si pas de color table
# Comptage des valeurs (classes) présentes
#f <- freq(r1, digits = 0, value = TRUE)
#head(f)

# Vérif rapide
#compareGeom(r1, r2, stopOnError = FALSE)
#crs(r1); crs(r2)
#res(r1); res(r2)
#ext(r1); ext(r2)

parse_qgis_colormap <- function(path_txt) {
  x <- readLines(path_txt, warn = FALSE)
  x <- x[grepl("^\\s*\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+-\\s+", x)]
  
  m <- regexec("^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+-\\s+(.*)$", x)
  r <- regmatches(x, m)
  r <- do.call(rbind, lapply(r, function(z) z[-1]))
  
  out <- data.frame(
    value = as.integer(r[,1]),  # valeur dans le tif (1..44,48)
    R = as.integer(r[,2]),
    G = as.integer(r[,3]),
    B = as.integer(r[,4]),
    A = as.integer(r[,5]),
    clc = as.integer(r[,6]),    # vrai code CLC (111..523,999)
    label = r[,7],
    stringsAsFactors = FALSE
  )
  out
}

# chemins vers légendes
setwd("C:/Users/Clément/Desktop/Work/projet_env/data")
leg2018 <- "CLC2018_CLC2018_V2018_20_QGIS.txt"
leg1990 <- "CLC1990_CLC1990_V2018_20_QGIS.txt"
lut18 <- parse_qgis_colormap(leg2018)
lut90 <- parse_qgis_colormap(leg1990)

# rasters (déjà découpés France métro+Corse si tu veux)
r2 <- rast("CLC2018_metro_corse.tif")
r1 <- rast("CLC1990_metro_corse.tif")

# Recodage index -> CLC (et NODATA -> NA)
r2_clc <- subst(r2, lut18$value, ifelse(lut18$clc == 999, NA, lut18$clc), others = NA)
r1_clc <- subst(r1, lut90$value, ifelse(lut90$clc == 999, NA, lut90$clc), others = NA)

#
count_tif_90 = as_tibble(freq(r1_clc, digits = 0))
count_tif_18 = as_tibble(freq(r2_clc, digits = 0))







# Si compareGeom n'est pas TRUE, aligne r2 sur la grille de r1
#if (!compareGeom(r1, r2, stopOnError = FALSE)) {
#  r2 <- project(r2, r1, method = "near")   # reprojection + grille de r1
#  r2 <- resample(r2, r1, method = "near")  # au cas où (souvent redondant)
#}


# Transformer le polygone dans le CRS du raster (ici on prend celui de r1)
metro_corse_union_r1 <- st_transform(sf_communes_metro, crs(r1_clc))
# Convertir en objet terra
metro_corse_v <- vect(metro_corse_union_r1)

# Mets les communes dans le CRS du raster et en SpatVector
communes_r <- st_transform(communes_metro_corse, crs(r1_clc))
communes_r <- st_make_valid(communes_r)
communes_v <- vect(communes_r)

# ID numérique (plus rapide) + table de correspondance
communes_v$cid <- seq_len(nrow(communes_v))
cid_lut <- as.data.frame(communes_v)[, c("cid", "code_insee")]

# Raster d'ID commune (même grille que r1)
# touches=TRUE : compte les pixels touchés par le polygone (utile aux limites)
cid_r <- rasterize(communes_v, r1_clc, field = "cid", touches = TRUE)


#
# plot(cid_r, main = "Raster des ID communes (cid)")
# plot(communes_v, add = TRUE, lwd = 0.2)
# nb de cid distincts présents


# Reclasser par intervalles (rapide)
m_cat <- matrix(c(
  111, 142, 1,   # artificialisé
  211, 244, 2,   # agricole
  311, 313, 3,   # forêt
  321, 324, 4,   # semi-naturel
  331, 999, 5    # other (mostly sea)
), ncol = 3, byrow = TRUE)

cat1 <- classify(r1_clc, m_cat, others = 0, right = NA)
cat2 <- classify(r2_clc, m_cat, others = 0, right = NA)

# Option "habitat" strict : 111-112 seulement (si tu veux distinguer habitat vs autres artificialisations)
# m_hab <- matrix(c(111, 112, 1), ncol = 3, byrow = TRUE)
# hab2 <- classify(r2, m_hab, others = 0, right = TRUE)  # habitat en 2018 (1/0)

## 16
library(sf)
library(terra)
library(dplyr)

# ----------------------------
# 1) Sélection de la commune
# ----------------------------
insee <- "08239"  # 

ville_sf <- communes_r %>%
  mutate(code_insee = as.character(code_insee)) %>%
  filter(code_insee == insee)

if (nrow(ville_sf) == 0) stop("Commune INSEE introuvable : ", insee)

ville_sf <- st_make_valid(ville_sf)
ville_v  <- vect(ville_sf)  # SpatVector (terra)

# ----------------------------
# 2) Découpe des rasters (catégories agrégées)
#    cat1 = 1990 ; cat2 = 2018 (d'après ton script)
# ----------------------------
cat1_ville <- mask(crop(cat1, ville_v), ville_v)
cat2_ville <- mask(crop(cat2, ville_v), ville_v)
plot(cat1_ville)
# Option: découpe aussi en codes CLC détaillés (si tu veux)
# r1_ville <- mask(crop(r1_clc, ville_v), ville_v)  # 1990 CLC codes
# r2_ville <- mask(crop(r2_clc, ville_v), ville_v)  # 2018 CLC codes

# ----------------------------
# 3) Affichage 1990 vs 2018
# ----------------------------
labs_cat <- c(
  "0" = "Hors classe/NA",
  "1" = "Artificialisé",
  "2" = "Agricole",
  "3" = "Forêt",
  "4" = "Semi-naturel",
  "5" = "Autre (mer, etc.)"
)

make_cat_factor <- function(r, labs) {
  rf <- as.factor(r)
  levels(rf) <- data.frame(
    ID    = 0:5,
    label = unname(labs[as.character(0:5)])
  )
  rf
}

cat1_f <- make_cat_factor(cat1_ville, labs_cat)
cat2_f <- make_cat_factor(cat2_ville, labs_cat)

par(mfrow = c(1, 2), mar = c(3, 3, 3, 6))
plot(cat1_f, main = paste0(insee, " – 1990 (cat)"), axes = TRUE, plg = list(x = "right"))
plot(cat2_f, main = paste0(insee, " – 2018 (cat)"), axes = TRUE, plg = list(x = "right"))
par(mfrow = c(1, 1))




# ----------------------------
# 4) Carte des transitions 1990 -> 2018 (catégories)
#    code_transition = 10*cat1990 + cat2018
# ----------------------------
trans_ville <- cat1_ville * 10 + cat2_ville

plot(trans_ville, main = paste0(insee, " – Transitions 1990→2018 (10*a+b)"))

# Exemples utiles (binaire) :
# agricole -> artificialisé  : 21
# agricole -> forêt          : 23
agri_to_artif <- trans_ville == 21
agri_to_foret <- trans_ville == 23



library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

# --- 0) Check rapide : ton raster n'est pas vide ?
#print(global(is.na(cat1_f), "mean", na.rm = FALSE))
#print(freq(cat1_f, digits = 0) |> head(10))

# --- 1) Agrégation (optionnelle)
cat1_p <- aggregate(cat1_f, fact = 2, fun = "modal", na.rm = TRUE)
cat2_p <- aggregate(cat2_f, fact = 2, fun = "modal", na.rm = TRUE)

# --- 2) Récupérer la table de catégories (le mapping ID -> label)
lev <- levels(cat1_f)[[1]]
stopifnot(!is.null(lev))  # si ça plante ici, ton raster n'est pas factor/catégoriel

# Deviner colonnes ID + label (robuste aux noms différents)
id_col <- intersect(names(lev), c("ID","id","value","Value","layer"))[1]
if (is.na(id_col)) id_col <- names(lev)[1]

lab_col <- intersect(names(lev), c("label","Label","LABEL","class","Class","category","Category","name","Name"))[1]
if (is.na(lab_col)) lab_col <- names(lev)[2]

legend_tbl <- lev %>%
  transmute(
    cat   = as.integer(.data[[id_col]]),     # IMPORTANT: on mappe sur les VALEURS du raster (IDs)
    label = as.character(.data[[lab_col]])
  ) %>%
  distinct() %>%
  arrange(cat)

# Palette auto (modifiable)
legend_tbl$color <- c("darkgray","#e41a1c", "#ffd92f", "#4daf4a","#59FF80","#AAC8FC")


# --- 3) Raster -> df + join sur IDs
r_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x","y","cat")
  df$cat <- dplyr::case_when(
    df$cat == "Hors classe/NA"     ~ 0L,
    df$cat == "Artificialisé"      ~ 1L,
    df$cat == "Agricole"           ~ 2L,
    df$cat == "Forêt"              ~ 3L,
    df$cat == "Semi-naturel"       ~ 4L,
    df$cat == "Autre (mer, etc.)"  ~ 5L,
    TRUE                           ~ NA_integer_
  )
  # ne pas utiliser as.integer(df$cat) ==> Crée un décalage
  
  df <- df %>% left_join(legend_tbl, by = "cat")
  df$label <- factor(df$label, levels = legend_tbl$label)  # ordre légende
  df
}

df1990 <- r_to_df(cat1_p)
df2018 <- r_to_df(cat2_p)

# Check anti “tout gris” : si tout est NA ici, ta légende ne matche pas (ou raster vide)
#print(mean(is.na(df1990$label)))


fill_scale <- scale_fill_manual(
  values = setNames(legend_tbl$color, legend_tbl$label),
  limits = legend_tbl$label,   # <- clé pour afficher TOUT
  breaks = legend_tbl$label,
  drop   = FALSE,
  na.value = "grey90",
  name = "CLC"
)

make_plot <- function(df, title) {
  ggplot(df, aes(x, y, fill = label)) +
    geom_raster() +
    coord_equal(expand = FALSE) +
    labs(title = title) +
    fill_scale +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

p1 <- make_plot(df1990, paste0(insee, " – 1990"))
p2 <- make_plot(df2018, paste0(insee, " – 2018"))

(p1 + p2) + plot_layout(guides = "collect") & theme(legend.position = "right")


# ----------------------------
# 5) Résumé chiffré (hectares) par catégorie + matrice de transition
# ----------------------------
library(dplyr)
library(terra)

# aire d'une cellule en ha (OK si CRS en mètres)
cell_area_ha <- prod(res(cat1_ville)) / 10000

# fréquence 1990
f90 <- terra::freq(cat1_ville, digits = 0, value = TRUE)
f90 <- as.data.frame(f90)

# certaines versions renvoient "count", d'autres "frequency"
count_col <- intersect(c("count", "frequency"), names(f90))[1]
if (is.na(count_col)) stop("Je ne trouve pas la colonne de comptage dans freq(). Regarde names(f90).")

tab90 <- f90 %>%
  filter(!is.na(value)) %>%                 # enlève la ligne NA si présente
  transmute(
    cat = as.integer(value),
    n = .data[[count_col]],
    ha_1990 = n * cell_area_ha
  )

f18 <- terra::freq(cat2_ville, digits = 0, value = TRUE) |> as.data.frame()
count_col18 <- intersect(c("count", "frequency"), names(f18))[1]

tab18 <- f18 %>%
  filter(!is.na(value)) %>%
  transmute(
    cat = as.integer(value),
    n = .data[[count_col18]],
    ha_2018 = n * cell_area_ha
  )




## 17

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

# ----------------------------
# 0) Choix du département
# ----------------------------
dep_code <- "44"   # ex: "77", "2A", "2B", "971", ...

dep_from_insee <- function(x) {
  x <- as.character(x)
  ifelse(grepl("^(97|98)", x), substr(x, 1, 3), substr(x, 1, 2))
}

# communes_r = tes communes déjà dans le CRS du raster (comme dans ton script)
communes_dep <- communes_r %>%
  mutate(code_insee = as.character(code_insee),
         dep = dep_from_insee(code_insee)) %>%
  filter(dep == dep_code)

if (nrow(communes_dep) == 0) stop("Département introuvable : ", dep_code)

# Union -> polygone du département
dept_sf <- communes_dep %>%
  summarise(dep = first(dep), .groups = "drop") %>%
  st_make_valid()

dept_v <- vect(dept_sf)  # SpatVector terra

# ----------------------------
# 1) Découpe des rasters (cat1=1990 ; cat2=2018)
# ----------------------------
cat1_dept <- mask(crop(cat1, dept_v), dept_v)
cat2_dept <- mask(crop(cat2, dept_v), dept_v)

# (option) transitions 1990->2018
trans_dept <- cat1_dept * 10 + cat2_dept

# Exemples binaires
agri_to_artif <- trans_dept == 21
agri_to_foret <- trans_dept == 23

# ----------------------------
# 2) Affichage base (terra) avec légende
# ----------------------------
labs_cat <- c(
  "0" = "Hors classe/NA",
  "1" = "Artificialisé",
  "2" = "Agricole",
  "3" = "Forêt",
  "4" = "Semi-naturel",
  "5" = "Autre (mer, etc.)"
)

make_cat_factor <- function(r, labs) {
  rf <- as.factor(r)
  levels(rf) <- data.frame(
    ID    = 0:5,
    label = unname(labs[as.character(0:5)])
  )
  rf
}

cat1_f <- make_cat_factor(cat1_dept, labs_cat)
cat2_f <- make_cat_factor(cat2_dept, labs_cat)

#par(mfrow = c(1, 2), mar = c(3, 3, 3, 6))
#plot(cat1_f, main = paste0("Dépt ", dep_code, " – 1990"), axes = TRUE, plg = list(x = "right"))
#plot(cat2_f, main = paste0("Dépt ", dep_code, " – 2018"), axes = TRUE, plg = list(x = "right"))
#par(mfrow = c(1, 1))

#plot(trans_dept, main = paste0("Dépt ", dep_code, " – Transitions 1990→2018 (10*a+b)"))

# ----------------------------
# 3) Version ggplot (propre) — IMPORTANT:
#    agrège sur les rasters numériques (cat1_dept/cat2_dept), pas sur cat1_f
# ----------------------------
cat1_p <- aggregate(cat1_dept, fact = 2, fun = "modal", na.rm = TRUE)
cat2_p <- aggregate(cat2_dept, fact = 2, fun = "modal", na.rm = TRUE)

legend_tbl <- tibble::tibble(
  cat   = 0:5,
  label = unname(labs_cat[as.character(0:5)]),
  color = c("darkgray","#e41a1c", "#ffd92f", "#4daf4a","#59FF80","#AAC8FC")
)

r_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x","y","cat")
  df$cat <- as.integer(df$cat)
  df <- dplyr::left_join(df, legend_tbl, by = "cat")
  df$label <- factor(df$label, levels = legend_tbl$label)
  df
}

df1990 <- r_to_df(cat1_p)
df2018 <- r_to_df(cat2_p)

fill_scale <- scale_fill_manual(
  values  = setNames(legend_tbl$color, legend_tbl$label),
  limits  = legend_tbl$label,
  breaks  = legend_tbl$label,
  drop    = FALSE,
  na.value = "grey90",
  name    = "CLC"
)

make_plot <- function(df, title) {
  ggplot(df, aes(x, y, fill = label)) +
    geom_raster() +
    coord_equal(expand = FALSE) +
    labs(title = title) +
    fill_scale +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

p1 <- make_plot(df1990, paste0("Dépt ", dep_code, " – 1990"))
p2 <- make_plot(df2018, paste0("Dépt ", dep_code, " – 2018"))

(p1 + p2) + plot_layout(guides = "collect") & theme(legend.position = "right")

## 18
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tibble)

# ------------------------------------------------------------------
# 0) Polygone France métro + Corse (dans le CRS du raster)
#    - Si tu as déjà `metro_corse_union` (sf) : on l’utilise
#    - Sinon : on reconstruit l’union à partir de `communes_r`
# ------------------------------------------------------------------

france_sf = st_read("metro_corse_union.gpkg", layer = "metro_corse_union")


france_v <- vect(france_sf)  # SpatVector terra

# ------------------------------------------------------------------
# 1) Découpe des rasters (cat1=1990 ; cat2=2018)
# ------------------------------------------------------------------
# 0) convertir sf -> SpatVector


# 1) forcer le CRS identique au raster (terra gère très bien)
france_v <- project(france_v, cat1)  # ou project(france_v, crs(cat1))

# 2) re-check
#crs(france_v)
#ext(france_v)

# 3) découpe
cat1_fr <- crop(cat1, france_v)
cat1_fr <- mask(cat1_fr, france_v)

cat2_fr <- crop(cat2, france_v)
cat2_fr <- mask(cat2_fr, france_v)
# (option) transitions 1990->2018 (10*a+b)
trans_fr <- cat1_fr * 10 + cat2_fr

# Exemples binaires
#agri_to_artif <- trans_fr == 21
#agri_to_foret <- trans_fr == 23

# ------------------------------------------------------------------
# 2) Version ggplot (propre) — IMPORTANT:
#    Pour toute la France, il faut DOWNSAMPLER fort avant ggplot.
#    fact_map = 20 => ~2 km, fact_map = 10 => ~1 km (plus lourd).
# ------------------------------------------------------------------
fact_map <- 20
cat1_p <- aggregate(cat1_fr, fact = fact_map, fun = "modal", na.rm = TRUE)
cat2_p <- aggregate(cat2_fr, fact = fact_map, fun = "modal", na.rm = TRUE)

labs_cat <- c(
  "0" = "Hors classe/NA",
  "1" = "Artificialisé",
  "2" = "Agricole",
  "3" = "Forêt",
  "4" = "Semi-naturel",
  "5" = "Autre (mer, etc.)"
)

legend_tbl <- tibble(
  cat   = 0:5,
  label = unname(labs_cat[as.character(0:5)]),
  color = c("darkgray","#e41a1c","#ffd92f","#4daf4a","#59FF80","#AAC8FC")
)

r_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x","y","cat")
  df$cat <- as.integer(df$cat)
  df <- left_join(df, legend_tbl, by = "cat")
  df$label <- factor(df$label, levels = legend_tbl$label)
  df
}

df1990 <- r_to_df(cat1_p)
df2018 <- r_to_df(cat2_p)

fill_scale <- scale_fill_manual(
  values   = setNames(legend_tbl$color, legend_tbl$label),
  limits   = legend_tbl$label,
  breaks   = legend_tbl$label,
  drop     = FALSE,
  na.value = "grey90",
  name     = "CLC"
)

make_plot <- function(df, title) {
  ggplot(df, aes(x, y, fill = label)) +
    geom_raster() +
    coord_equal(expand = FALSE) +
    labs(title = title) +
    fill_scale +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

p1 <- make_plot(df1990, "France métropolitaine – 1990")
p2 <- make_plot(df2018, "France métropolitaine – 2018")

(p1 + p2) + plot_layout(guides = "collect") & theme(legend.position = "right")

## 19
setwd("C:/Users/Clément/Desktop/Work/projet_env/data/")

sf_communes_metro <- st_read("metro_corse_union.gpkg", layer = "metro_corse_union")



trans <- cat1 * 10 + cat2
# 1 : 1990, 2 : 2018
# ----------------------------
# 1) Sélection de la commune
# ----------------------------
insee <- "91477"  # 

ville_sf <- communes_r %>%
  mutate(code_insee = as.character(code_insee)) %>%
  filter(code_insee == insee)

if (nrow(ville_sf) == 0) stop("Commune INSEE introuvable : ", insee)

ville_sf <- st_make_valid(ville_sf)
ville_v  <- vect(ville_sf)  # SpatVector (terra)



cat_trans_ville <- mask(crop(trans, ville_v), ville_v)
#plot(cat1_ville)
# Option: découpe aussi en codes CLC détaillés (si tu veux)
# r1_ville <- mask(crop(r1_clc, ville_v), ville_v)  # 1990 CLC codes
# r2_ville <- mask(crop(r2_clc, ville_v), ville_v)  # 2018 CLC codes

# ----------------------------
# 3) Affichage 1990 vs 2018
# ----------------------------
labs_cat <- c(
  # "0" = "Hors classe/NA",
  
  # "1" = "Artificialisé",
  "11" = "Artificialisé",
  "12" = "Artificialisé vers Agricole",
  "13" = "Artificialisé vers Forêt",
  "14" = "Artificialisé vers Semi-naturel",
  "15" = "Artificialisé vers Autre (mer, etc.)",
  
  # "2" = "Agricole",
  "21" = "Agricole vers Artificialisé",
  "22" = "Agricole",
  "23" = "Agricole vers Forêt",
  "24" = "Agricole vers Semi-naturel",
  "25" = "Agricole vers Autre (mer, etc.)",
  
  # "3" = "Forêt",
  "31" = "Forêt vers Artificialisé",
  "32" = "Forêt vers Agricole",
  "33" = "Forêt",
  "34" = "Forêt vers Semi-naturel",
  "35" = "Forêt vers Autre (mer, etc.)",
  
  # "4" = "Semi-naturel",
  "41" = "Semi-naturel vers Artificialisé",
  "42" = "Semi-naturel vers Agricole",
  "43" = "Semi-naturel vers Forêt",
  "44" = "Semi-naturel",
  "45" = "Semi-naturel vers Autre (mer, etc.)",
  
  # "5" = "Autre (mer, etc.)",
  "51" = "Autre (mer, etc.) vers Artificialisé",
  "52" = "Autre (mer, etc.) vers Agricole",
  "53" = "Autre (mer, etc.) vers Forêt",
  "54" = "Autre (mer, etc.) vers Semi-naturel",
  "55" = "Autre (mer, etc.)"
  
)

make_cat_factor <- function(r, labs) {
  rf <- as.factor(r)
  levels(rf) <- data.frame(
    ID    = 0:55,
    label = unname(labs[as.character(0:55)])
  )
  rf
}

cat_f <- make_cat_factor(cat_trans_ville, labs_cat)

#par(mfrow = c(1, 2), mar = c(3, 3, 3, 6))
#plot(cat1_f, main = paste0(insee, " – 1990 (cat)"), axes = TRUE, plg = list(x = "right"))
#plot(cat2_f, main = paste0(insee, " – 2018 (cat)"), axes = TRUE, plg = list(x = "right"))
#par(mfrow = c(1, 1))




# ----------------------------
# 4) Carte des transitions 1990 -> 2018 (catégories)
#    code_transition = 10*cat1990 + cat2018
# ----------------------------





library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

# --- 0) Check rapide : ton raster n'est pas vide ?
print(global(is.na(cat_f), "mean", na.rm = FALSE))
print(freq(cat_f, digits = 0) |> head(10))

# --- 1) Agrégation (optionnelle)
cat_p <- aggregate(cat_f, fact = 2, fun = "modal", na.rm = TRUE)

# --- 2) Récupérer la table de catégories (le mapping ID -> label)
lev <- levels(cat_f)[[1]]
stopifnot(!is.null(lev))  # si ça plante ici, ton raster n'est pas factor/catégoriel

# Deviner colonnes ID + label (robuste aux noms différents)
id_col <- intersect(names(lev), c("ID","id","value","Value","layer"))[1]
if (is.na(id_col)) id_col <- names(lev)[1]

lab_col <- intersect(names(lev), c("label","Label","LABEL","class","Class","category","Category","name","Name"))[1]
if (is.na(lab_col)) lab_col <- names(lev)[2]

legend_tbl <- lev %>%
  transmute(
    cat   = as.integer(.data[[id_col]]),     # IMPORTANT: on mappe sur les VALEURS du raster (IDs)
    label = as.character(.data[[lab_col]])
  ) %>%
  distinct() %>%
  arrange(cat) %>%
  filter(!is.na(label))

# Palette de base
#legend_tbl$color <- c("darkgray","#e41a1c", "#ffd92f", "#4daf4a","#59FF80","#AAC8FC")

# Couleurs "originales" par grande classe (1..5)
base_cols <- c(
  "1" = "#e41a1c",  # Artificialisé
  "2" = "#ffd92f",  # Agricole
  "3" = "#4daf4a",  # Forêt
  "4" = "#59FF80",  # Semi-naturel
  "5" = "#AAC8FC"   # Autre (mer, etc.)
)

# Mélange de 2 couleurs (poids w pour la 1ère, 1-w pour la 2ème)
blend_hex <- function(c1, c2, w = 0.5) {
  r1 <- grDevices::col2rgb(c1)
  r2 <- grDevices::col2rgb(c2)
  mix <- round(w * r1 + (1 - w) * r2)
  grDevices::rgb(mix[1,], mix[2,], mix[3,], maxColorValue = 255)
}

# Décomposer cat = (origine)(destination), ex 23 = Agricole -> Forêt
orig <- legend_tbl$cat %/% 10
dest <- legend_tbl$cat %% 10

# Couleur finale : diagonale = couleur de base, transition = mélange
legend_tbl$color <- ifelse(
  orig == dest,
  unname(base_cols[as.character(orig)]),
  mapply(function(o, d) blend_hex(base_cols[as.character(o)], base_cols[as.character(d)], w = 0.5),
         orig, dest)
)



# --- 3) Raster -> df + join sur IDs
r_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x","y","cat")
  df$cat <- dplyr::case_when(
    df$cat == "Hors classe/NA" ~ 0L,
    
    df$cat == "Artificialisé"                     ~ 11L,
    df$cat == "Artificialisé vers Agricole"       ~ 12L,
    df$cat == "Artificialisé vers Forêt"          ~ 13L,
    df$cat == "Artificialisé vers Semi-naturel"   ~ 14L,
    df$cat == "Artificialisé vers Autre (mer, etc.)" ~ 15L,
    
    df$cat == "Agricole vers Artificialisé"       ~ 21L,
    df$cat == "Agricole"                          ~ 22L,
    df$cat == "Agricole vers Forêt"               ~ 23L,
    df$cat == "Agricole vers Semi-naturel"        ~ 24L,
    df$cat == "Agricole vers Autre (mer, etc.)"   ~ 25L,
    
    df$cat == "Forêt vers Artificialisé"          ~ 31L,
    df$cat == "Forêt vers Agricole"               ~ 32L,
    df$cat == "Forêt"                             ~ 33L,
    df$cat == "Forêt vers Semi-naturel"           ~ 34L,
    df$cat == "Forêt vers Autre (mer, etc.)"      ~ 35L,
    
    df$cat == "Semi-naturel vers Artificialisé"   ~ 41L,
    df$cat == "Semi-naturel vers Agricole"        ~ 42L,
    df$cat == "Semi-naturel vers Forêt"           ~ 43L,
    df$cat == "Semi-naturel"                      ~ 44L,
    df$cat == "Semi-naturel vers Autre (mer, etc.)" ~ 45L,
    
    df$cat == "Autre (mer, etc.) vers Artificialisé" ~ 51L,
    df$cat == "Autre (mer, etc.) vers Agricole"      ~ 52L,
    df$cat == "Autre (mer, etc.) vers Forêt"         ~ 53L,
    df$cat == "Autre (mer, etc.) vers Semi-naturel"  ~ 54L,
    df$cat == "Autre (mer, etc.)"                    ~ 55L,
    
    TRUE ~ NA_integer_
  )
  
  # ne pas utiliser as.integer(df$cat) ==> Crée un décalage
  
  df <- df %>% left_join(legend_tbl, by = "cat")
  df$label <- factor(df$label, levels = legend_tbl$label)  # ordre légende
  df
}

df_trans <- r_to_df(cat_p)

# Check anti “tout gris” : si tout est NA ici, ta légende ne matche pas (ou raster vide)
#print(mean(is.na(df1990$label)))


fill_scale <- scale_fill_manual(
  values = setNames(legend_tbl$color, legend_tbl$label),
  limits = legend_tbl$label,   # <- clé pour afficher TOUT
  breaks = legend_tbl$label,
  drop   = FALSE,
  na.value = "grey90",
  name = "CLC"
)

make_plot <- function(df, title) {
  ggplot(df, aes(x, y, fill = label)) +
    geom_raster() +
    coord_equal(expand = FALSE) +
    labs(title = title) +
    fill_scale +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

p1 <- make_plot(df_trans, paste0(insee, " 1990 - 2018"))

p1 + plot_layout(guides = "collect") & theme(legend.position = "right")

## 20
setwd("C:/Users/Clément/Desktop/Work/projet_env/data/")

# ------------------------------------------------------------------
# 0) Polygone France métro + Corse (dans le CRS du raster)
#    - Si tu as déjà `metro_corse_union` (sf) : on l’utilise
#    - Sinon : on reconstruit l’union à partir de `communes_r`
# ------------------------------------------------------------------

france_sf = st_read("metro_corse_union.gpkg", layer = "metro_corse_union")


france_v <- vect(france_sf)  # SpatVector terra

# ------------------------------------------------------------------
# 1) Découpe des rasters (cat1=1990 ; cat2=2018)
# ------------------------------------------------------------------
# 0) convertir sf -> SpatVector


# 1) forcer le CRS identique au raster (terra gère très bien)
france_v <- project(france_v, cat1)  # ou project(france_v, crs(cat1))

# 2) re-check
#crs(france_v)
#ext(france_v)

# 3) découpe
cat1_fr <- crop(cat1, france_v)
cat1_fr <- mask(cat1_fr, france_v)

cat2_fr <- crop(cat2, france_v)
cat2_fr <- mask(cat2_fr, france_v)
# (option) transitions 1990->2018 (10*a+b)
cat_trans_france <- cat1_fr * 10 + cat2_fr
# 1 : 1990, 2 : 2018

# ----------------------------
# 1) Sélection de la commune
# ----------------------------
#plot(cat1_ville)
# Option: découpe aussi en codes CLC détaillés (si tu veux)
# r1_ville <- mask(crop(r1_clc, ville_v), ville_v)  # 1990 CLC codes
# r2_ville <- mask(crop(r2_clc, ville_v), ville_v)  # 2018 CLC codes

# ----------------------------
# 3) Affichage 1990 vs 2018
# ----------------------------
labs_cat <- c(
  # "0" = "Hors classe/NA",
  
  # "1" = "Artificialisé",
  "11" = "Artificialisé",
  "12" = "Artificialisé vers Agricole",
  "13" = "Artificialisé vers Forêt",
  "14" = "Artificialisé vers Semi-naturel",
  "15" = "Artificialisé vers Autre (mer, etc.)",
  
  # "2" = "Agricole",
  "21" = "Agricole vers Artificialisé",
  "22" = "Agricole",
  "23" = "Agricole vers Forêt",
  "24" = "Agricole vers Semi-naturel",
  "25" = "Agricole vers Autre (mer, etc.)",
  
  # "3" = "Forêt",
  "31" = "Forêt vers Artificialisé",
  "32" = "Forêt vers Agricole",
  "33" = "Forêt",
  "34" = "Forêt vers Semi-naturel",
  "35" = "Forêt vers Autre (mer, etc.)",
  
  # "4" = "Semi-naturel",
  "41" = "Semi-naturel vers Artificialisé",
  "42" = "Semi-naturel vers Agricole",
  "43" = "Semi-naturel vers Forêt",
  "44" = "Semi-naturel",
  "45" = "Semi-naturel vers Autre (mer, etc.)",
  
  # "5" = "Autre (mer, etc.)",
  "51" = "Autre (mer, etc.) vers Artificialisé",
  "52" = "Autre (mer, etc.) vers Agricole",
  "53" = "Autre (mer, etc.) vers Forêt",
  "54" = "Autre (mer, etc.) vers Semi-naturel",
  "55" = "Autre (mer, etc.)"
  
)

make_cat_factor <- function(r, labs) {
  rf <- as.factor(r)
  levels(rf) <- data.frame(
    ID    = 0:55,
    label = unname(labs[as.character(0:55)])
  )
  rf
}

cat_f <- make_cat_factor(cat_trans_france, labs_cat)

#par(mfrow = c(1, 2), mar = c(3, 3, 3, 6))
#plot(cat1_f, main = paste0(insee, " – 1990 (cat)"), axes = TRUE, plg = list(x = "right"))
#plot(cat2_f, main = paste0(insee, " – 2018 (cat)"), axes = TRUE, plg = list(x = "right"))
#par(mfrow = c(1, 1))




# ----------------------------
# 4) Carte des transitions 1990 -> 2018 (catégories)
#    code_transition = 10*cat1990 + cat2018
# ----------------------------





library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

# --- 0) Check rapide : ton raster n'est pas vide ?
print(global(is.na(cat_f), "mean", na.rm = FALSE))
print(freq(cat_f, digits = 0) |> head(10))

# --- 1) Agrégation (optionnelle)
cat_p <- aggregate(cat_f, fact = 2, fun = "modal", na.rm = TRUE)

# --- 2) Récupérer la table de catégories (le mapping ID -> label)
lev <- levels(cat_f)[[1]]
stopifnot(!is.null(lev))  # si ça plante ici, ton raster n'est pas factor/catégoriel

# Deviner colonnes ID + label (robuste aux noms différents)
id_col <- intersect(names(lev), c("ID","id","value","Value","layer"))[1]
if (is.na(id_col)) id_col <- names(lev)[1]

lab_col <- intersect(names(lev), c("label","Label","LABEL","class","Class","category","Category","name","Name"))[1]
if (is.na(lab_col)) lab_col <- names(lev)[2]

legend_tbl <- lev %>%
  transmute(
    cat   = as.integer(.data[[id_col]]),     # IMPORTANT: on mappe sur les VALEURS du raster (IDs)
    label = as.character(.data[[lab_col]])
  ) %>%
  distinct() %>%
  arrange(cat) %>%
  filter(!is.na(label))

# Palette de base
#legend_tbl$color <- c("darkgray","#e41a1c", "#ffd92f", "#4daf4a","#59FF80","#AAC8FC")

# Couleurs "originales" par grande classe (1..5)
base_cols <- c(
  "1" = "#e41a1c",  # Artificialisé
  "2" = "#ffd92f",  # Agricole
  "3" = "#4daf4a",  # Forêt
  "4" = "#59FF80",  # Semi-naturel
  "5" = "#AAC8FC"   # Autre (mer, etc.)
)

# Mélange de 2 couleurs (poids w pour la 1ère, 1-w pour la 2ème)
blend_hex <- function(c1, c2, w = 0.5) {
  r1 <- grDevices::col2rgb(c1)
  r2 <- grDevices::col2rgb(c2)
  mix <- round(w * r1 + (1 - w) * r2)
  grDevices::rgb(mix[1,], mix[2,], mix[3,], maxColorValue = 255)
}

# Décomposer cat = (origine)(destination), ex 23 = Agricole -> Forêt
orig <- legend_tbl$cat %/% 10
dest <- legend_tbl$cat %% 10

# Couleur finale : diagonale = couleur de base, transition = mélange
legend_tbl$color <- ifelse(
  orig == dest,
  unname(base_cols[as.character(orig)]),
  mapply(function(o, d) blend_hex(base_cols[as.character(o)], base_cols[as.character(d)], w = 0.5),
         orig, dest)
)

legend_tbl$color = ifelse(legend_tbl$label == "Agricole vers Forêt" ,"#ffffff","#000000")

# --- 3) Raster -> df + join sur IDs
r_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x","y","cat")
  df$cat <- dplyr::case_when(
    df$cat == "Hors classe/NA" ~ 0L,
    
    df$cat == "Artificialisé"                     ~ 11L,
    df$cat == "Artificialisé vers Agricole"       ~ 12L,
    df$cat == "Artificialisé vers Forêt"          ~ 13L,
    df$cat == "Artificialisé vers Semi-naturel"   ~ 14L,
    df$cat == "Artificialisé vers Autre (mer, etc.)" ~ 15L,
    
    df$cat == "Agricole vers Artificialisé"       ~ 21L,
    df$cat == "Agricole"                          ~ 22L,
    df$cat == "Agricole vers Forêt"               ~ 23L,
    df$cat == "Agricole vers Semi-naturel"        ~ 24L,
    df$cat == "Agricole vers Autre (mer, etc.)"   ~ 25L,
    
    df$cat == "Forêt vers Artificialisé"          ~ 31L,
    df$cat == "Forêt vers Agricole"               ~ 32L,
    df$cat == "Forêt"                             ~ 33L,
    df$cat == "Forêt vers Semi-naturel"           ~ 34L,
    df$cat == "Forêt vers Autre (mer, etc.)"      ~ 35L,
    
    df$cat == "Semi-naturel vers Artificialisé"   ~ 41L,
    df$cat == "Semi-naturel vers Agricole"        ~ 42L,
    df$cat == "Semi-naturel vers Forêt"           ~ 43L,
    df$cat == "Semi-naturel"                      ~ 44L,
    df$cat == "Semi-naturel vers Autre (mer, etc.)" ~ 45L,
    
    df$cat == "Autre (mer, etc.) vers Artificialisé" ~ 51L,
    df$cat == "Autre (mer, etc.) vers Agricole"      ~ 52L,
    df$cat == "Autre (mer, etc.) vers Forêt"         ~ 53L,
    df$cat == "Autre (mer, etc.) vers Semi-naturel"  ~ 54L,
    df$cat == "Autre (mer, etc.)"                    ~ 55L,
    
    TRUE ~ NA_integer_
  )
  
  # ne pas utiliser as.integer(df$cat) ==> Crée un décalage
  
  df <- df %>% left_join(legend_tbl, by = "cat")
  df$label <- factor(df$label, levels = legend_tbl$label)  # ordre légende
  df
}

df_trans <- r_to_df(cat_p)

# Check anti “tout gris” : si tout est NA ici, ta légende ne matche pas (ou raster vide)
#print(mean(is.na(df1990$label)))


fill_scale <- scale_fill_manual(
  values = setNames(legend_tbl$color, legend_tbl$label),
  limits = legend_tbl$label,   # <- clé pour afficher TOUT
  breaks = legend_tbl$label,
  drop   = FALSE,
  na.value = "grey90",
  name = "CLC"
)

make_plot <- function(df, title) {
  ggplot(df, aes(x, y, fill = label)) +
    geom_raster() +
    coord_equal(expand = FALSE) +
    labs(title = title) +
    fill_scale +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

p1 <- make_plot(df_trans, paste0("France", " 1990 - 2018"))

p1 + plot_layout(guides = "collect") & theme(legend.position = "right")

## 21


#

#write_parquet("df_trans.parquet",x = df_trans)

df_trans = read_parquet("df_trans.parquet") %>%
  select(-color) 

table_trans = df_trans %>%
  tabyl(label) %>%
  mutate(percent = round(percent, 2)) %>%
  filter(percent != 0)
table_trans
