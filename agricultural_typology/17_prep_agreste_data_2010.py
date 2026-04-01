import pandas as pd

# ============================================================
# 0) Chemins
# ============================================================

input_file = "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/FDS_G_1013_2010.txt"
output_file = "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/superficies_communes_2010.csv"

# ============================================================
# 1) Charger le fichier
# ============================================================

df = pd.read_csv(input_file, sep=";", encoding="utf-8")

# ============================================================
# 2) Garder uniquement le niveau commune
# ============================================================

df = df[df["COM"] != "............"].copy()

# ============================================================
# 3) Garder uniquement les superficies en hectares
# ============================================================

df = df[df["G_1013_LIB_DIM4"] == "Superficie correspondante (hectares)"].copy()

# ============================================================
# 4) Garder les colonnes utiles
# ============================================================

df = df[[
    "ANNREF",
    "FRDOM",
    "REGION",
    "DEP",
    "COM",
    "G_1013_MOD_DIM2",
    "G_1013_LIB_DIM2",
    "VALEUR"
]].copy()

# ============================================================
# 5) Renommer les colonnes
# ============================================================

df = df.rename(columns={
    "ANNREF": "annee",
    "FRDOM": "frdom",
    "REGION": "region",
    "DEP": "dep",
    "COM": "com",
    "G_1013_MOD_DIM2": "code_culture",
    "G_1013_LIB_DIM2": "culture",
    "VALEUR": "surface_ha"
})

# ============================================================
# 6) Nettoyage des types
# ============================================================

df["com"] = df["com"].astype(str).str.strip().str.zfill(5)
df["culture"] = df["culture"].astype(str).str.strip()
df["surface_ha"] = pd.to_numeric(df["surface_ha"], errors="coerce")

# ============================================================
# 7) Garder uniquement les grandes catégories
# ============================================================

# Grandes catégories que tu veux conserver
great_categories = [
    "Cultures industrielles",
    "Fourrages et superficies toujours en herbe",
    "Pommes de terre et tubercules",
    "Légumes frais, fraises, melons",
    "Cultures permanentes entretenues",
    "Vignes",
    "Céréales",
    "Oléagineux, protéagineux, plantes à fibres  (Total)",
    "Légumes secs",
    "Fleurs et plantes ornementales",
    "Jachères"
]

df = df[df["culture"].isin(great_categories)].copy()

# ============================================================
# 8) Vérifications
# ============================================================

print("Cultures conservées :")
print(sorted(df["culture"].unique()))
print()

print("Nombre de lignes après filtrage :", len(df))
print("Nombre de communes :", df["com"].nunique())
print()

# Vérifie qu'il n'y a plus de sous-catégories détaillées
bad_rows = df[df["culture"].str.startswith("_", na=False)]
print("Nb lignes commençant par '_' :", len(bad_rows))
print()

# ============================================================
# 9) Pivot : une ligne par commune, une colonne par grande catégorie
# ============================================================

df_wide = df.pivot_table(
    index=["annee", "frdom", "region", "dep", "com"],
    columns="culture",
    values="surface_ha",
    aggfunc="sum",
    fill_value=0
).reset_index()

df_wide.columns.name = None

# ============================================================
# 10) Sauvegarder le résultat
# ============================================================

df_wide.to_csv(output_file, index=False, encoding="utf-8")

# ============================================================
# 11) Aperçu
# ============================================================

print("Table créée avec succès :", output_file)
print("Dimensions :", df_wide.shape)
print(df_wide.head())