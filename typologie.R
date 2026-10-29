# ---- PARAMÈTRES ------------------------------------------------------------------
DEPARTEMENT     <- NULL                   # NULL = France entière ; ou par ex. "21"
DOSSIER_DONNEES <- "donnees_elections"    # cache partagé avec le script des cartes
FICHIER_SORTIE  <- "resultats_par_bureau.csv"
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(FactoMineR)
})

options(timeout = 3600)

# ---- 1. Téléchargement (une seule fois) --------------------------------------
dir.create(DOSSIER_DONNEES, showWarnings = FALSE)
sources <- c(
  "candidats_results.parquet" = "https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/candidats_results.parquet",
  "general_results.parquet"   = "https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/general_results.parquet"
)
chemin <- function(f) file.path(DOSSIER_DONNEES, f)
for (f in names(sources)) {
  if (!file.exists(chemin(f))) {
    message("Téléchargement de ", f, "…")
    download.file(sources[[f]], chemin(f), mode = "wb")
  }
}

candidats <- open_dataset(chemin("candidats_results.parquet"))
generaux  <- open_dataset(chemin("general_results.parquet"))

filtre_dep <- function(ds) {
  if (is.null(DEPARTEMENT)) ds else filter(ds, code_departement == DEPARTEMENT)
}

# ---- 2. Blocs de la présidentielle 2022 --------------------------------------------
# Rattachement par numéro de panneau ; le nom sert de contrôle.
BLOCS_2022 <- tribble(
  ~no_panneau, ~nom_attendu,     ~bloc,
  1L,         "ARTHAUD",        "extreme_gauche",
  2L,         "ROUSSEL",        "gauche_hors_lfi",
  3L,         "MACRON",         "centre",
  4L,         "LASSALLE",       "divers",
  5L,         "LE PEN",         "extreme_droite",
  6L,         "ZEMMOUR",        "extreme_droite",
  7L,         "LENCHON",        "melenchon",
  8L,         "HIDALGO",        "gauche_hors_lfi",
  9L,         "JADOT",          "gauche_hors_lfi",
  10L,         "CRESSE",         "droite",
  11L,         "POUTOU",         "extreme_gauche",
  12L,         "DUPONT-AIGNAN",  "extreme_droite"
)

message("Présidentielle 2022…")
cand_2022 <- candidats |>
  filter(id_election == "2022_pres_t1") |>
  filtre_dep() |>
  select(id_brut_miom, no_panneau, nom, voix) |>
  collect()

# Contrôle : chaque panneau correspond bien au candidat attendu
controle <- cand_2022 |>
  distinct(no_panneau, nom) |>
  inner_join(BLOCS_2022, by = "no_panneau") |>
  filter(!mapply(grepl, nom_attendu, nom, fixed = TRUE))
if (nrow(controle)) stop("Panneaux inattendus à la présidentielle 2022 : ", paste(controle$nom, collapse = ", "))

blocs_2022 <- cand_2022 |>
  inner_join(BLOCS_2022, by = "no_panneau") |>
  group_by(id_brut_miom, bloc) |>
  summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = bloc, values_from = voix, values_fill = 0L)

gen_2022 <- generaux |>
  filter(id_election == "2022_pres_t1") |>
  filtre_dep() |>
  select(id_brut_miom, code_departement, code_commune, libelle_commune, code_bv,
         inscrits, votants, exprimes) |>
  collect()

pct <- function(voix, base) ifelse(base > 0, round(100 * voix / base, 2), NA_real_)

pres_2022 <- gen_2022 |>
  left_join(blocs_2022, by = "id_brut_miom") |>
  transmute(
    id_bureau = id_brut_miom, code_departement, code_commune, libelle_commune, code_bv,
    vote_melenchon_2022        = pct(melenchon, exprimes),
    taux_abstention_2022       = pct(inscrits - votants, inscrits),
    vote_gauche_hors_lfi_2022  = pct(gauche_hors_lfi, exprimes),
    vote_extreme_gauche_2022   = pct(extreme_gauche, exprimes),
    vote_centre_2022           = pct(centre, exprimes),
    vote_droite_2022           = pct(droite, exprimes),
    vote_extreme_droite_2022   = pct(extreme_droite, exprimes),
    vote_divers_2022           = pct(divers, exprimes),
    inscrits_2022 = inscrits, exprimes_2022 = exprimes
  )

# ---- 3. LFI aux européennes 2024 et NFP aux législatives 2024 ------------------------
score_2024 <- function(election, nuance_visee, col_vote, col_exprimes) {
  voix <- candidats |>
    filter(id_election == election, nuance == nuance_visee) |>
    filtre_dep() |>
    select(id_brut_miom, voix) |>
    collect() |>
    group_by(id_brut_miom) |>
    summarise(voix = sum(voix, na.rm = TRUE))
  generaux |>
    filter(id_election == election) |>
    filtre_dep() |>
    select(id_brut_miom, code_departement, code_commune, libelle_commune, code_bv, inscrits, exprimes) |>
    collect() |>
    left_join(voix, by = "id_brut_miom") |>
    # Pas de ligne pour la nuance = pas de candidat dans ce bureau -> NA (et non 0)
    transmute(id_bureau = id_brut_miom, code_departement, code_commune, libelle_commune, code_bv,
              !!col_vote := pct(voix, exprimes),
              !!col_exprimes := exprimes)
}

message("Européennes et législatives 2024…")
euro_2024 <- score_2024("2024_euro_t1", "LFI", "vote_lfi_euro_2024", "exprimes_euro_2024")
legi_2024 <- score_2024("2024_legi_t1", "UG",  "vote_nfp_legi_2024", "exprimes_legi_2024")

# ---- 4. Assemblage --------------------------------------------------------------------
# Jointure complète : un bureau présent à un seul scrutin garde sa ligne (NA ailleurs).
cles <- c("id_bureau", "code_departement", "code_commune", "libelle_commune", "code_bv")
table_bv <- pres_2022 |>
  full_join(select(euro_2024, -c(code_departement, code_commune, libelle_commune, code_bv)), by = "id_bureau") |>
  full_join(select(legi_2024, -c(code_departement, code_commune, libelle_commune, code_bv)), by = "id_bureau")

# Identifiants des bureaux absents de 2022 : on les reprend des scrutins 2024
infos_2024 <- bind_rows(legi_2024, euro_2024) |> select(all_of(cles)) |> distinct(id_bureau, .keep_all = TRUE)
table_bv <- table_bv |>
  rows_patch(infos_2024, by = "id_bureau", unmatched = "ignore") |>
  select(id_bureau, code_departement, code_commune, libelle_commune, code_bv,
         vote_melenchon_2022, vote_lfi_euro_2024, vote_nfp_legi_2024,
         taux_abstention_2022, vote_gauche_hors_lfi_2022, vote_extreme_gauche_2022,
         vote_centre_2022, vote_droite_2022, vote_extreme_droite_2022, vote_divers_2022,
         inscrits_2022, exprimes_2022, exprimes_euro_2024, exprimes_legi_2024) |>
  arrange(id_bureau)

# ---- 5. Contrôles et écriture -------------------------------------------------------------
somme_2022 <- with(table_bv, vote_melenchon_2022 + vote_gauche_hors_lfi_2022 + vote_extreme_gauche_2022 +
                     vote_centre_2022 + vote_droite_2022 + vote_extreme_droite_2022 + vote_divers_2022)
message(nrow(table_bv), " bureaux de vote. Somme des blocs 2022 : entre ",
        round(min(somme_2022, na.rm = TRUE), 1), " et ", round(max(somme_2022, na.rm = TRUE), 1), " %.")
message("Bureaux sans résultat : présidentielle 2022 ", sum(is.na(table_bv$exprimes_2022)),
        " ; européennes 2024 ", sum(is.na(table_bv$exprimes_euro_2024)),
        " ; législatives 2024 ", sum(is.na(table_bv$exprimes_legi_2024)),
        " (dont sans candidat NFP : ", sum(!is.na(table_bv$exprimes_legi_2024) & is.na(table_bv$vote_nfp_legi_2024)), ").")

# Point-virgule et virgule décimale, avec BOM : s'ouvre correctement dans Excel
fwrite(table_bv, FICHIER_SORTIE, sep = ";", dec = ",", bom = TRUE, na = "")
message("Table écrite : ", normalizePath(FICHIER_SORTIE))

df <- table_bv |>
  filter(exprimes_2022 >= 10) |>                
  select(id_bureau, exprimes_2022, taux_abstention_2022, starts_with("vote")) |>
  na.omit()
rownames(df) <- df$id_bureau
df_t <- df |> select(-id_bureau)

acp <- PCA(df_t |> dplyr::select(-exprimes_2022), scale.unit = FALSE, ncp = 5,
           row.w = df$exprimes_2022, graph = FALSE)
acp$eig                                       # choisir le nombre d'axes

coord <- acp$ind$coord[, 1:5]     # axes retenus
w     <- df$exprimes_2022

# 1. Pré-partition k-means en 1000 groupes
set.seed(1)
km <- kmeans(coord, centers = 1000, iter.max = 100, algorithm = "MacQueen")

# Centres pondérés par les inscrits et masses des groupes
m    <- tapply(w, km$cluster, sum)
cent <- apply(coord, 2, function(x) tapply(x * w, km$cluster, sum)) / as.vector(m)

# 2. Ward sur les 1000 centres, en tenant compte de leurs masses
d2    <- as.matrix(dist(cent))^2
delta <- outer(m, m) / outer(m, m, "+") * d2     # critère de Ward entre centres
arbre <- hclust(as.dist(delta), method = "ward.D", members = m)

plot(rev(arbre$height)[1:20], type = "h",
     xlab = "Nombre de classes", ylab = "Gain d'inertie")   # choix de k

# 3. Coupure puis consolidation k-means initialisée sur les centres de classes
k       <- 6
classe0 <- cutree(arbre, k)[km$cluster]
init    <- apply(coord, 2, function(x) tapply(x * w, classe0, sum)) /
  as.vector(tapply(w, classe0, sum))

km2 <- kmeans(coord, centers = init, iter.max = 100)
df$classe <- factor(km2$cluster)

# 4. Description des classes (linéaire en n, pas de souci mémoire)
vars <- rownames(acp$var$coord)
desc <- catdes(df[, c(vars, "classe")],
               num.var = length(vars) + 1, row.w = df$exprimes_2022)
desc$quanti


classes_description <- c(
  "Gauche faible et peu LFI, très forte participation"            = 1,
  "Gauche très faible et en recul en 2024, participation moyenne" = 2,
  "Gauche moyenne portée par la gauche hors LFI, forte participation" = 3,
  "Gauche moyenne, participation moyenne"                         = 4,
  "Gauche très forte et quasi exclusivement LFI, très faible participation" = 5,
  "Gauche forte à dominante LFI, faible participation"            = 6
)

df$classe_lib <- factor(df$classe,
                        levels = classes_description,
                        labels = names(classes_description))

# ---- 6. Export pour les cartes ------------------------------------------------------------
# typologie_bureaux.csv : une ligne par bureau classé (les bureaux écartés par na.omit
#   ou le seuil d'exprimés n'y figurent pas : « non classés » sur les cartes).
# typologie_classes.csv : libellé et profil moyen de chaque type (pondéré par les exprimés).
classes_bv <- df |>
  transmute(id_bureau, classe = as.integer(as.character(classe)), exprimes_2022) |>
  left_join(select(table_bv, id_bureau, code_commune, code_bv), by = "id_bureau")
fwrite(classes_bv, "typologie_bureaux.csv", sep = ";", bom = TRUE)

profils <- df |>
  mutate(classe = as.integer(as.character(classe))) |>
  group_by(classe) |>
  summarise(across(all_of(vars), ~ round(weighted.mean(.x, exprimes_2022), 1)),
            part_exprimes = sum(exprimes_2022)) |>
  mutate(part_exprimes = round(100 * part_exprimes / sum(part_exprimes), 1),
         libelle = names(classes_description)[match(classe, classes_description)]) |>
  relocate(classe, libelle, part_exprimes)
if (anyNA(profils$libelle)) stop("Une classe n'a pas de libellé dans classes_description.")
fwrite(profils, "typologie_classes.csv", sep = ";", dec = ",", bom = TRUE)
message("Typologie exportée : typologie_bureaux.csv (", nrow(classes_bv), " bureaux) et typologie_classes.csv")
