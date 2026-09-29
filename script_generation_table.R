# Indicateurs électoraux par commune
# Source : « Données des élections agrégées » (data.gouv.fr, jeu 6481e741d4cf002ec0efec9d)
#
#  1. Mélenchon, présidentielle 2022 T1
#  2. Liste LFI (Manon Aubry), européennes 2024
#  3. Nouveau Front populaire (nuance UG), législatives 2024 T1
#  4. Plus d'une liste au 1er tour des municipales 2026 ? (+ nombre d'inscrits)
#
# Les résultats sont au bureau de vote : on somme voix et exprimés par commune.

library(arrow)
library(dplyr)
library(tidyr)

options(timeout = 3600)

url_base <- "https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/"
fichiers <- c("candidats_results.parquet", "general_results.parquet") # ~160 Mo + ~70 Mo

for (f in fichiers) {
  if (!file.exists(f)) download.file(paste0(url_base, f), f, mode = "wb")
}

candidats <- open_dataset("candidats_results.parquet")
generaux  <- open_dataset("general_results.parquet")

elections <- c("2022_pres_t1", "2024_euro_t1", "2024_legi_t1")

# Exprimés par commune et par élection
exprimes <- generaux |>
  filter(id_election %in% elections) |>
  group_by(id_election, code_commune) |>
  summarise(exprimes = sum(exprimes, na.rm = TRUE)) |>
  collect()

# Voix des trois cibles, repérées de façon robuste (accents, casse)
voix <- candidats |>
  filter(id_election %in% elections) |>
  select(id_election, code_commune, nom, prenom, nuance, voix) |>
  collect() |>
  mutate(
    indicateur = case_when(
      id_election == "2022_pres_t1" & prenom == "Jean-Luc" &
        grepl("^M.LENCHON$", nom)                          ~ "jlm_2022",
      id_election == "2024_euro_t1" & nuance == "LFI"      ~ "lfi_euro_2024",
      id_election == "2024_legi_t1" & nuance == "UG"       ~ "nfp_legi_2024",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(indicateur)) |>
  group_by(id_election, indicateur, code_commune) |>
  summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop")

resultats <- voix |>
  left_join(exprimes, by = c("id_election", "code_commune")) |>
  mutate(pct = round(100 * voix / exprimes, 2)) |>
  select(code_commune, indicateur, voix, pct) |>
  pivot_wider(
    names_from  = indicateur,
    values_from = c(voix, pct),
    names_glue  = "{indicateur}_{.value}"
  )

# 4. Nombre de listes au 1er tour des municipales 2026
#    (quelques lignes n'ont pas de numéro de panneau : on se rabat sur le libellé)
listes_2026 <- candidats |>
  filter(id_election == "2026_muni_t1") |>
  select(code_commune, no_panneau, libelle_abrege_liste) |>
  collect() |>
  mutate(id_liste = coalesce(as.character(no_panneau), libelle_abrege_liste)) |>
  group_by(code_commune) |>
  summarise(nb_listes_muni_2026 = n_distinct(id_liste)) |>
  mutate(plusieurs_listes_muni_2026 = nb_listes_muni_2026 > 1)

# Inscrits au 1er tour des municipales 2026
inscrits_2026 <- generaux |>
  filter(id_election == "2026_muni_t1") |>
  group_by(code_commune) |>
  summarise(inscrits_muni_2026 = sum(inscrits, na.rm = TRUE)) |>
  collect()

# Libellés des communes (dernier libellé connu)
communes <- generaux |>
  filter(id_election == "2026_muni_t1" | id_election %in% elections) |>
  distinct(code_commune, libelle_commune, code_departement) |>
  collect() |>
  distinct(code_commune, .keep_all = TRUE)

tableau <- communes |>
  left_join(resultats, by = "code_commune") |>
  left_join(listes_2026, by = "code_commune") |>
  left_join(inscrits_2026, by = "code_commune") |>
  arrange(code_commune)

# Résumé national (pourcentages sur l'ensemble des exprimés)
national <- voix |>
  group_by(indicateur, id_election) |>
  summarise(voix = sum(voix), .groups = "drop") |>
  left_join(
    exprimes |> group_by(id_election) |> summarise(exprimes = sum(exprimes)),
    by = "id_election"
  ) |>
  mutate(pct = round(100 * voix / exprimes, 2))

print(national)
cat("\nCommunes avec plus d'une liste aux municipales 2026 (T1) :",
    sum(listes_2026$plusieurs_listes_muni_2026), "sur", nrow(listes_2026), "\n")

write.csv2(tableau, "indicateurs_communes.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Exemple pour une commune :
# tableau |> filter(code_commune == "59350")  # Lille
