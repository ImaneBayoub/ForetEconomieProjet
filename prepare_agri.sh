#!/bin/bash

echo " Nettoyage des données 2010..."

# 1) Garder les colonnes utiles
xan select ANNREF,CODE_GEO,N110d,N110d_MOD,RA2020_001_DIM3_MOD,VALEUR_BRUTE,NB_ETAB data/data_commune_2010.csv \
    > data/clean_2010_step1.csv

# 2) Enlever "Toutes exploitations"
xan filter 'ne(N110d_MOD, "Toutes exploitations")' data/clean_2010_step1.csv \
    > data/clean_2010_step2.csv

# 3) Garder les lignes "Production brute standard ("
# (on utilise contains() car le texte contient une parenthèse, ce qui casse eq())
xan filter 'contains(RA2020_001_DIM3_MOD, "Production brute standard")' data/clean_2010_step2.csv \
    > data/clean_2010_step3.csv

# 4) Enlever les valeurs vides ET aussi VALEUR_BRUTE = 0
xan filter 'and(ne(VALEUR_BRUTE, ""), ne(VALEUR_BRUTE, "0"))' data/clean_2010_step3.csv \
    > data/agri_2010_clean.csv


echo " Nettoyage des données 2020..."

xan select ANNREF,CODE_GEO,N110d,N110d_MOD,RA2020_001_DIM3_MOD,VALEUR_BRUTE,NB_ETAB data/data_commune_2020.csv \
    > data/clean_2020_step1.csv

xan filter 'ne(N110d_MOD, "Toutes exploitations")' data/clean_2020_step1.csv \
    > data/clean_2020_step2.csv

# Même filtre : nominal complet 2020 contient aussi "("
xan filter 'contains(RA2020_001_DIM3_MOD, "Production brute standard")' data/clean_2020_step2.csv \
    > data/clean_2020_step3.csv

xan filter 'and(ne(VALEUR_BRUTE, ""), ne(VALEUR_BRUTE, "0"))' data/clean_2020_step3.csv \
    > data/agri_2020_clean.csv


echo " Fusion des données 2010 + 2020..."

xan merge data/agri_2010_clean.csv data/agri_2020_clean.csv \
    > data/agri_all_clean.csv

echo " Terminé ! Fichier final : data/agri_all_clean.csv"
