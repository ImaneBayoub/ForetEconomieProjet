import pandas as pd
import numpy as np

# ============================================================
# 0) Chemins
# ============================================================

input_file_1 = "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/FDS_G_1013_2000.txt"
input_file_2 = "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/FDS_G_1013_2010.txt"

out_wide_1 = "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/superficies_communes_2000_detailees.csv"
out_wide_2 = "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/superficies_communes_2010_detailees.csv"

out_compare = "/home/imane/Documents/ensae/ForetEconomieProjet/data/evolution_cultures_detaillees_2000_2010.csv"
out_summary = "/home/imane/Documents/ensae/ForetEconomieProjet/data/evolution_cultures_detaillees_2000_2010_resume.csv"

# ============================================================
# 1) Fonction de préparation d'une année
# ============================================================

def prepare_year(input_file: str, output_file: str) -> pd.DataFrame:
    df = pd.read_csv(
        input_file,
        sep=";",
        encoding="utf-8",
        low_memory=False
    )

    # niveau commune
    df = df[df["COM"] != "............"].copy()

    # superficies en hectares
    df = df[df["G_1013_LIB_DIM4"] == "Superficie correspondante (hectares)"].copy()

    # colonnes utiles
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

    # renommage
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

    # nettoyage
    df["com"] = df["com"].astype(str).str.strip().str.zfill(5)
    df["culture"] = df["culture"].astype(str).str.strip()
    df["surface_ha"] = pd.to_numeric(df["surface_ha"], errors="coerce").fillna(0)

    # retirer lignes vides / parasites
    df = df[df["culture"] != ""].copy()
    df = df[~df["culture"].str.startswith("_", na=False)].copy()

    # pivot
    df_wide = df.pivot_table(
        index=["annee", "frdom", "region", "dep", "com"],
        columns="culture",
        values="surface_ha",
        aggfunc="sum",
        fill_value=0
    ).reset_index()

    df_wide.columns.name = None
    df_wide.to_csv(output_file, index=False, encoding="utf-8")

    return df_wide

# ============================================================
# 2) Préparer les deux années
# ============================================================

df1 = prepare_year(input_file_1, out_wide_1)
df2 = prepare_year(input_file_2, out_wide_2)

print("Année 1 OK :", out_wide_1, df1.shape)
print("Année 2 OK :", out_wide_2, df2.shape)

# ============================================================
# 3) Harmoniser les catégories communes aux deux années
# ============================================================

id_cols = ["annee", "frdom", "region", "dep", "com"]

cult_cols_1 = [c for c in df1.columns if c not in id_cols]
cult_cols_2 = [c for c in df2.columns if c not in id_cols]

common_cult_cols = sorted(set(cult_cols_1).intersection(cult_cols_2))

if len(common_cult_cols) == 0:
    raise ValueError("Aucune catégorie de culture commune entre les deux années.")

print(f"Nombre de catégories communes : {len(common_cult_cols)}")

df1 = df1[id_cols + common_cult_cols].copy()
df2 = df2[id_cols + common_cult_cols].copy()

# garder uniquement les communes communes aux deux années
common_com = sorted(set(df1["com"]).intersection(df2["com"]))
df1 = df1[df1["com"].isin(common_com)].copy()
df2 = df2[df2["com"].isin(common_com)].copy()

print(f"Nombre de communes communes aux deux années : {len(common_com)}")

# sécurité : une seule ligne par commune
dup1 = df1["com"].duplicated().sum()
dup2 = df2["com"].duplicated().sum()
print("Doublons année 1 sur com :", dup1)
print("Doublons année 2 sur com :", dup2)

if dup1 > 0:
    df1 = df1.groupby("com", as_index=False)[common_cult_cols].sum()
    df1["annee"] = 2000

if dup2 > 0:
    df2 = df2.groupby("com", as_index=False)[common_cult_cols].sum()
    df2["annee"] = 2010

# ============================================================
# 4) Passer en parts
# ============================================================

def to_shares(df: pd.DataFrame, culture_cols: list[str]) -> pd.DataFrame:
    out = df.copy()
    out["total_surface"] = out[culture_cols].sum(axis=1)
    out = out[out["total_surface"] > 0].copy()

    for c in culture_cols:
        out[c] = out[c] / out["total_surface"]

    return out

share1 = to_shares(df1, common_cult_cols)
share2 = to_shares(df2, common_cult_cols)

# garder communes présentes avec total > 0 dans les deux années
common_com_2 = sorted(set(share1["com"]).intersection(share2["com"]))
share1 = share1[share1["com"].isin(common_com_2)].copy()
share2 = share2[share2["com"].isin(common_com_2)].copy()

print(f"Nombre de communes après filtre total_surface>0 dans les deux années : {len(common_com_2)}")

# ============================================================
# 5) Fonctions diagnostics
# ============================================================

def top_category(row: pd.Series, cols: list[str]) -> str:
    return row[cols].idxmax()

def shannon_index(arr: np.ndarray) -> float:
    arr = arr[arr > 0]
    if len(arr) == 0:
        return np.nan
    return float(-(arr * np.log(arr)).sum())

def l1_distance(a: np.ndarray, b: np.ndarray) -> float:
    return float(np.abs(a - b).sum())

def corr_safe(a: np.ndarray, b: np.ndarray) -> float:
    if np.allclose(a, a[0]) or np.allclose(b, b[0]):
        return np.nan
    return float(np.corrcoef(a, b)[0, 1])

annual_keywords = [
    "Céréales",
    "Oléagineux", "protéagineux", "plantes à fibres",
    "Cultures industrielles",
    "Pommes de terre", "tubercules",
    "Légumes frais", "fraises", "melons",
    "Légumes secs",
    "Fleurs", "ornementales",
    "Jachères"
]

perennial_keywords = [
    "Vignes",
    "Cultures permanentes"
]

def classify_group(colname: str) -> str:
    if any(k.lower() in colname.lower() for k in perennial_keywords):
        return "perenne"
    if any(k.lower() in colname.lower() for k in annual_keywords):
        return "annuelle"
    return "autre"

group_map = {c: classify_group(c) for c in common_cult_cols}

annual_cols = [c for c in common_cult_cols if group_map[c] == "annuelle"]
perennial_cols = [c for c in common_cult_cols if group_map[c] == "perenne"]
other_cols = [c for c in common_cult_cols if group_map[c] == "autre"]

print("Colonnes annuelles :", len(annual_cols))
print("Colonnes pérennes  :", len(perennial_cols))
print("Colonnes autres    :", len(other_cols))

# ============================================================
# 6) Jointure explicite des deux années
# ============================================================

share1_small = share1[["com", "annee", "total_surface"] + common_cult_cols].copy()
share2_small = share2[["com", "annee", "total_surface"] + common_cult_cols].copy()

merged = share1_small.merge(
    share2_small,
    on="com",
    how="inner",
    suffixes=("_1", "_2")
)

print("Nombre de communes après merge explicite :", len(merged))

# ============================================================
# 7) Construire les indicateurs d'évolution
# ============================================================

rows = []

for _, row in merged.iterrows():
    vec1 = row[[f"{c}_1" for c in common_cult_cols]].to_numpy(dtype=float)
    vec2 = row[[f"{c}_2" for c in common_cult_cols]].to_numpy(dtype=float)

    # pour retrouver les noms de catégories dominantes
    top1 = common_cult_cols[int(np.argmax(vec1))]
    top2 = common_cult_cols[int(np.argmax(vec2))]

    annual_share_1 = float(row[[f"{c}_1" for c in annual_cols]].sum()) if annual_cols else np.nan
    annual_share_2 = float(row[[f"{c}_2" for c in annual_cols]].sum()) if annual_cols else np.nan

    perennial_share_1 = float(row[[f"{c}_1" for c in perennial_cols]].sum()) if perennial_cols else np.nan
    perennial_share_2 = float(row[[f"{c}_2" for c in perennial_cols]].sum()) if perennial_cols else np.nan

    rows.append({
        "com": row["com"],
        "annee_1": int(row["annee_1"]),
        "annee_2": int(row["annee_2"]),
        "top_culture_1": top1,
        "top_culture_2": top2,
        "top_changed": top1 != top2,
        "l1_distance": l1_distance(vec1, vec2),
        "corr_parts": corr_safe(vec1, vec2),
        "shannon_1": shannon_index(vec1),
        "shannon_2": shannon_index(vec2),
        "delta_shannon": shannon_index(vec2) - shannon_index(vec1),
        "annual_share_1": annual_share_1,
        "annual_share_2": annual_share_2,
        "delta_annual_share": annual_share_2 - annual_share_1,
        "perennial_share_1": perennial_share_1,
        "perennial_share_2": perennial_share_2 - perennial_share_1 + perennial_share_1,  # overwritten next line
        "delta_perennial_share": perennial_share_2 - perennial_share_1
    })

evol = pd.DataFrame(rows)
evol["perennial_share_2"] = merged[[f"{c}_2" for c in perennial_cols]].sum(axis=1).to_numpy(dtype=float)

# ============================================================
# 8) Diagnostics simples de stabilité
# ============================================================

evol["stable_l1_5pts"] = evol["l1_distance"] < 0.05
evol["stable_l1_10pts"] = evol["l1_distance"] < 0.10

evol["direction_cycle"] = np.select(
    [
        evol["delta_annual_share"] > 0.05,
        evol["delta_perennial_share"] > 0.05
    ],
    [
        "plus_annuelle",
        "plus_perenne"
    ],
    default="stable_ou_mixte"
)

# ============================================================
# 9) Sauvegardes
# ============================================================

evol.to_csv(out_compare, index=False, encoding="utf-8")

summary_df = pd.DataFrame({
    "n_communes_comparees": [len(evol)],
    "part_top_changed_pct": [100 * evol["top_changed"].mean()],
    "l1_distance_mean": [evol["l1_distance"].mean()],
    "l1_distance_median": [evol["l1_distance"].median()],
    "part_stable_l1_5pts_pct": [100 * evol["stable_l1_5pts"].mean()],
    "part_stable_l1_10pts_pct": [100 * evol["stable_l1_10pts"].mean()],
    "delta_annual_share_mean": [evol["delta_annual_share"].mean()],
    "delta_perennial_share_mean": [evol["delta_perennial_share"].mean()]
})

summary_df.to_csv(out_summary, index=False, encoding="utf-8")

print("\nRésumé global")
print(summary_df.T)

print("\nTop 20 des communes qui changent le plus")
print(
    evol.sort_values("l1_distance", ascending=False)
        [["com", "top_culture_1", "top_culture_2", "l1_distance",
          "delta_annual_share", "delta_perennial_share"]]
        .head(20)
)

print("\nFichiers exportés :")
print(out_compare)
print(out_summary)