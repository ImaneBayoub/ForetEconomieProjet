#!/bin/bash

echo " Nettoyage des données 2010..."

# Dossier source des fichiers bruts
DATADIR="/c/Users/fanny/Desktop/ENSAE/Cours/Projet-ESSD/data/Fermes"

# 1) Garder les colonnes utiles
xan select ANNREF,CODE_GEO,N110d,N110d_MOD,RA2020_001_DIM3_MOD,VALEUR_BRUTE,NB_ETAB \
    "$DATADIR/FDS_RA2020_001_2010.csv" \
    --output "$DATADIR/clean_2010_step1.csv"

# 2) Enlever "Toutes exploitations"
xan filter 'ne(N110d_MOD, "Toutes exploitations")' \
    "$DATADIR/clean_2010_step1.csv" \
    --output "$DATADIR/clean_2010_step2.csv"

# 3) Garder les lignes "Production brute standard ("
xan filter 'contains(RA2020_001_DIM3_MOD, "Production brute standard")' \
    "$DATADIR/clean_2010_step2.csv" \
    --output "$DATADIR/clean_2010_step3.csv"

# 4) Enlever les valeurs vides ET VALEUR_BRUTE = 0
xan filter 'and(ne(VALEUR_BRUTE, ""), ne(VALEUR_BRUTE, "0"))' \
    "$DATADIR/clean_2010_step3.csv" \
    --output "$DATADIR/agri_2010_clean.csv"

echo " Nettoyage des données 2020..."

xan select ANNREF,CODE_GEO,N110d,N110d_MOD,RA2020_001_DIM3_MOD,VALEUR_BRUTE,NB_ETAB \
    "$DATADIR/FDS_RA2020_001_2020.csv" \
    --output "$DATADIR/clean_2020_step1.csv"

xan filter 'ne(N110d_MOD, "Toutes exploitations")' \
    "$DATADIR/clean_2020_step1.csv" \
    --output "$DATADIR/clean_2020_step2.csv"

xan filter 'contains(RA2020_001_DIM3_MOD, "Production brute standard")' \
    "$DATADIR/clean_2020_step2.csv" \
    --output "$DATADIR/clean_2020_step3.csv"
xan filter 'and(ne(VALEUR_BRUTE, ""), ne(VALEUR_BRUTE, "0"))' \
    "$DATADIR/clean_2020_step3.csv" \
    --output "$DATADIR/agri_2020_clean.csv"

echo " Fusion des données 2010 + 2020..."

xan merge "$DATADIR/agri_2010_clean.csv" "$DATADIR/agri_2020_clean.csv" \
    --output "$DATADIR/agri_all_clean.csv"

echo " Terminé ! Fichier final : $DATADIR/agri_all_clean.csv"