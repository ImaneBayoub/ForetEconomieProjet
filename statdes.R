library(tidyverse)
library(fixest)

## 1. Préparation des données
# Lecture des données
DATADIR <- "C://Users//fanny//Desktop//ENSAE//Cours//Projet-ESSD//data//Forêts"
df <- read.csv(file.path(DATADIR, "clc_cha_com_n1.csv"),
               skip = 3,  # Ignorer les 3 premières lignes
               sep = ";",
               header = TRUE)  # La 4ème ligne contient les noms de colonnes


surf_commune <- read.csv(file.path(DATADIR, "clc_etat_com_n1.csv"),
               skip = 3,  # Ignorer les 3 premières lignes
               sep = ";",
               header = TRUE)  # La 4ème ligne contient les noms de colonnes

surf_commune <- surf_commune %>%
  rename(
    commune = NUM_COM,
    annee = ANNEE,
    clc1 = CLC_1,
    clc2 = CLC_2,
    clc3 = CLC_3,
    clc4 = CLC_4,
    clc5 = CLC_5
  ) %>%
  mutate(
    surface_totale_commune = clc1 + clc2 + clc3 + clc4 + clc5,
    surface_naturelle_totale = clc3 + clc4 + clc5
  ) %>%
  select(commune, annee, surface_totale_commune, surface_naturelle_totale)



# Harmonisation des noms
df <- df %>%
  rename(
    commune = NUM_COM,
    annee_debut = ANNEE_DEBUT,
    annee_fin = ANNEE_FIN,
    code_debut = CODE_DEBUT,
    code_fin = CODE_FIN,
    area_ha = AREA_HA
  )

# Définition des zones naturelles
df <- df %>%
  mutate(
    naturel_debut = ifelse(code_debut %in% c(3, 4, 5), 1, 0),
    naturel_fin   = ifelse(code_fin   %in% c(3, 4, 5), 1, 0)
  )

## 2. Statistiques descriptives
# Calcul de l'évolution des surfaces naturelles
evol_nat <- df %>%
  group_by(annee_debut, annee_fin) %>%
  summarise(
    gain_naturel = sum(area_ha[naturel_debut == 0 & naturel_fin == 1], na.rm = TRUE),
    perte_naturel = sum(area_ha[naturel_debut == 1 & naturel_fin == 0], na.rm = TRUE),
    solde_naturel = gain_naturel - perte_naturel
  )

print(evol_nat)

# Détail des pertes de surfaces naturelles par destination
pertes_nat <- df %>%
  filter(naturel_debut == 1 & naturel_fin == 0) %>%
  group_by(code_fin) %>%
  summarise(surface = sum(area_ha, na.rm = TRUE)) %>%
  mutate(
    destination = case_when(
      code_fin == 1 ~ "Artificialisé",
      code_fin == 2 ~ "Agricole",
      TRUE ~ "Autre"
    )
  )

print(pertes_nat)

# Calcul de la part de surface ayant changé de nature
df %>%
  mutate(changement_naturel = (naturel_debut == 1 | naturel_fin == 1)) %>%
  summarise(
    part_surface = sum(area_ha[changement_naturel], na.rm = TRUE) /
                   sum(area_ha, na.rm = TRUE)
  )

## 3. Modélisation économétrique
panel_nat <- df %>%
  rowwise() %>%
  mutate(
    duree = annee_fin - annee_debut
  ) %>%
  ungroup() %>%
  filter(duree > 0) %>%
  mutate(
    area_ha_annuelle = area_ha / duree
  ) %>%
  tidyr::uncount(duree, .id = "t") %>%
  mutate(
    annee = annee_debut + t - 1
  )

# Construction d'un indicateur de surface naturelle
nat_commune_annee <- panel_nat %>%
  mutate(
    effet_naturel = case_when(
      naturel_debut == 0 & naturel_fin == 1 ~ area_ha_annuelle,
      naturel_debut == 1 & naturel_fin == 0 ~ -area_ha_annuelle,
      TRUE ~ 0
    )
  ) %>%
  group_by(commune, annee) %>%
  summarise(
    surface_naturelle = sum(effet_naturel, na.rm = TRUE)
  )


# Appariement avec la productivité agricole
prod <- read_csv(
  "productivite_fermes.csv",
  col_types = cols(
    ANNREF = col_integer(),
    CODE_GEO = col_character(),
    N110d = col_character(),
    N110d_MOD = col_character(),
    PR2020_01 = col_character(),
    DIM3_MOD = col_character(),
    VALEUR_BRUTE = col_double(),
    NB_ETAB = col_double()
  )
)

prod <- prod %>%
  rename(
    annee = ANNREF,
    commune = CODE_GEO,
    productivite = VALEUR_BRUTE
  ) %>%
  filter(annee %in% c(2010, 2020))

# Agrégation de la productivité par commune et année
prod_commune <- prod %>%
  group_by(commune, annee) %>%
  summarise(
    productivite = weighted.mean(productivite, NB_ETAB, na.rm = TRUE),
    nb_etab = sum(NB_ETAB, na.rm = TRUE),
    .groups = "drop"
  )

# Fusion des données
surfaces <- nat_commune_annee %>%
  filter(annee %in% c(2012, 2018)) %>%
  mutate(
    annee = case_when(
      annee == 2012 ~ 2010,
      annee == 2018 ~ 2020
    )
  )

panel_final <- prod_commune %>%
  left_join(surfaces, by = c("commune", "annee")) %>%
  filter(!is.na(surface_naturelle))

# Estimation du modèle à effets fixes
modele_twfe <- feols(
  productivite ~ surface_naturelle |
    commune + annee,
  data = panel_final,
  cluster = "commune"
)

summary(modele_twfe)
# Effet marginal d’1 hectare supplémentaire de zone naturelle
# sur la productivité agricole moyenne de la commune,
# toutes choses égales par ailleurs et à commune donnée.

panel_final <- panel_final %>%
  mutate(
    part_naturelle = surface_naturelle / surface_totale_commune
  )

feols(
  productivite ~ part_naturelle |
    commune + annee,
  data = panel_final,
  cluster = "commune"
)
# Effet marginal d’1 point de pourcentage supplémentaire de part de zone naturelle
# sur la productivité agricole moyenne de la commune,
# toutes choses égales par ailleurs et à commune donnée.

feols(
  productivite ~ surface_naturelle * part_grandes_cultures |
    commune + annee,
  data = panel_final
)
# Interaction entre surface naturelle et part de grandes cultures
# pour capturer des effets hétérogènes selon le type d'agriculture dominante.