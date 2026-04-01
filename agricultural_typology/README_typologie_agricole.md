# README — Typologie agricole communale

## Objectif

Cette partie du projet vise à caractériser la structure de production agricole des communes françaises et à construire une typologie exploitable pour l’analyse économétrique.

L’objectif est de déterminer si le type de production agricole (annuel vs pérenne, ou typologie plus fine) peut être utilisé comme variable explicative pertinente, et s’il est stable dans le temps.

---

## Stratégie

Deux approches complémentaires sont mises en œuvre :

1. Approche CLC (CORINE Land Cover)  
   - Basée sur l’occupation du sol  
   - Mesure : surfaces agricoles  
   - Classification : cultures annuelles vs pérennes  

2. Approche Agreste + LCA  
   - Basée sur les données agricoles (recensement)  
   - Mesure : surfaces par type de culture (proxy production)  
   - Méthode : Latent Class Analysis (LCA) pour identifier des profils agricoles  

---

## Question centrale

Le projet cherche à répondre à deux questions :

1. Le type de production agricole est-il stable dans le temps ?
2. Les différentes sources de données (CLC vs Agreste) donnent-elles une typologie cohérente ?

---

## Résultats principaux

- Les structures agricoles communales sont globalement très persistantes dans le temps  
- Les transitions observées sont limitées et locales  
- Les classifications issues de CLC et d’Agreste ne coïncident pas  
- Cette différence s’explique par la nature des données :
  - CLC mesure des surfaces
  - Agreste reflète les systèmes de production  

Conclusion : la typologie issue d’Agreste (LCA) est retenue pour la suite car elle est plus pertinente économiquement.

---

## Organisation des scripts

### Construction des données Agreste

- 17_prep_agreste_data_2010.py  
  Préparation des données Agreste 2010 (nettoyage, agrégation, pivot)

- 21_test_binaire_annuel_perenne_agreste_2000_2010.py  
  Test de stabilité des structures agricoles entre 2000 et 2010

---

### Typologie agricole (LCA)

- 18_lca_profils_cultures.R  
  Estimation d’une LCA sur les profils agricoles communaux

- 19_diagnostics_lca.R  
  Analyse de la qualité de la classification (probabilités, entropie, spécialisation)

- 18_bis_map_lca.R  
  Cartographie des classes LCA et de l’incertitude

- 22_test_lca_stabilite_profil_cultures_2000_2010.R  
  Analyse des transitions entre classes LCA (2000–2010)

---

### Analyse des clusters

- 17_analyse_cultures_par_cluster_2010.R  
  Description des clusters (composition agricole)

- 20_correspondance_cluster_profil.R  
  Comparaison entre clusters agricoles et clusters agri/forêt  
  Résultat : absence de correspondance forte (pas d’endogénéité directe)

---

### Approche CLC

- 23_cartes_clc_evolution.R  
  Construction des indicateurs agricoles CLC et cartes

- 24_clc_test_evolution_classification_binaire.R  
  Test de stabilité de la classification annuelle/pérenne

---

### Comparaison des approches

- 25_compare_clc_vs_agreste_lca.R  
  Comparaison directe des classifications CLC et Agreste  
  Production de matrices de confusion, indicateurs d’accord et cartes

---

### Clustering agri/forêt

- 16_clusters_trajectoires_agri_foret.R  
  Clustering des communes selon la structure et l’évolution agriculture/forêt

---

## Description des fichiers clés

### lca_communes_classes.csv

Contenu :
- 1 ligne = 1 commune (année 2010)
- Variables principales :
  - insee : identifiant commune
  - classe : cluster LCA attribué
  - prob_max : probabilité d’appartenance à la classe
  - uncertainty : incertitude (1 - probabilité)
  - prob_classe_X : probabilités pour chaque classe

Utilité :
- Variable principale de typologie agricole
- Permet de construire :
  - des dummies de clusters
  - une typologie agrégée (annuel / mixte / pérenne)



### lca_communes_summary.csv

Contenu :
- 1 ligne = 1 classe LCA
- Variables :
  - part de communes dans la classe
  - présence moyenne de chaque culture
  - parts moyennes (share_*)

Utilité :
- Interprétation économique des clusters
- Permet d’identifier le type agricole de chaque classe (ex : céréales, vigne, etc.)

---

### lca_2000_2010_transition_matrix.csv

Contenu :
- Matrice de transition :
  - lignes = classe en 2000
  - colonnes = classe en 2010
  - valeurs = nombre de communes

Utilité :
- Analyse des transitions entre types agricoles
- Mesure de la stabilité :
  - diagonale forte = forte stabilité
  - hors diagonale = changements

---

### evolution_cultures_detaillees_2000_2010.csv

Contenu :
- 1 ligne = 1 commune
- Variables :
  - top_culture_1, top_culture_2 : culture dominante
  - l1_distance : changement global de structure
  - corr_parts : similarité entre structures
  - delta_annual_share
  - delta_perennial_share
  - shannon : diversification

Utilité :
- Analyse fine et continue des évolutions
- Permet de mesurer :
  - l’intensité des changements
  - leur direction (plus annuel vs plus pérenne)

---

### carte_france_surface_agricole_clc2018_communes.csv

Contenu :
- 1 ligne = 1 commune
- Variables :
  - surf_agri_ha
  - surf_annual_ha
  - surf_perennial_ha
  - part_agri_commune
  - share_annual, share_perennial
  - score_type
  - commune_agricole (binaire)

Utilité :
- Construction de la typologie CLC (occupation du sol)
- Permet :
  - classification annuel / pérenne
  - cartographie
  - comparaison avec Agreste

Remarque :
- Basé sur des surfaces (pas sur la production)

---

### compare_labels_clc_vs_agreste.csv

Contenu :
- 1 ligne = 1 commune
- Variables :
  - label_clc_3 / label_clc_2
  - label_agreste_3 / label_agreste_2
  - agree_3, agree_2
  - compare_3, compare_2

Utilité :
- Comparaison directe entre CLC et Agreste
- Identification :
  - des accords
  - des désaccords
  - des cas indéterminés

---

## Lecture globale

Ces fichiers couvrent trois dimensions complémentaires :

1. Typologie (structure)
- lca_communes_classes.csv
- lca_communes_summary.csv

2. Dynamique (évolution)
- lca_2000_2010_classes.csv
- lca_2000_2010_transition_matrix.csv
- evolution_cultures_detaillees_2000_2010.csv

3. Validation / comparaison
- carte_france_surface_agricole_clc2018_communes.csv
- compare_labels_clc_vs_agreste.csv



