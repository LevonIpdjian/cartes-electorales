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
    inscrits_2022 = inscrits, exprimes_2022 = exprimes,
    voix_melenchon_2022 = melenchon,
    # Les mêmes votes en % des inscrits (variables de la typologie)
    melenchon22       = pct(melenchon, inscrits),
    gauche_hors_lfi22 = pct(gauche_hors_lfi, inscrits)
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

# ---- 3 bis. Européennes 2024 : LFI et gauche hors LFI, en % des inscrits ---------------------
# Gauche hors LFI = PS-Place publique (LUG), Les Écologistes (LVEC), PCF « Gauche unie » (LCOM),
# divers gauche (LDVG). Extrême gauche et listes écologistes non classées à gauche exclues.
GAUCHE_HORS_LFI_EURO <- c("LUG", "LVEC", "LCOM", "LDVG")

voix_euro <- candidats |>
  filter(id_election == "2024_euro_t1", nuance %in% c("LFI", GAUCHE_HORS_LFI_EURO)) |>
  filtre_dep() |>
  select(id_brut_miom, nuance, voix) |>
  collect() |>
  mutate(groupe = ifelse(nuance == "LFI", "voix_lfi_euro", "voix_gauche_hors_lfi_euro")) |>
  group_by(id_brut_miom, groupe) |>
  summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = groupe, values_from = voix, values_fill = 0L)

euro_inscrits <- generaux |>
  filter(id_election == "2024_euro_t1") |>
  filtre_dep() |>
  select(id_bureau = id_brut_miom, inscrits_euro_2024 = inscrits) |>
  collect() |>
  left_join(rename(voix_euro, id_bureau = id_brut_miom), by = "id_bureau")

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
  left_join(euro_inscrits, by = "id_bureau") |>
  mutate(
    # Écart entre Mélenchon 2022 et LFI européennes 2024, en points de % des inscrits
    # (différence de taux : insensible aux variations du nombre d'inscrits entre 2022 et 2024)
    ecart_melenchon22_lfi24 = pct(voix_melenchon_2022, inscrits_2022) - pct(voix_lfi_euro, inscrits_euro_2024),
    # Le même écart en nombre de voix (pour des cercles proportionnels)
    ecart_melenchon22_lfi24_voix = voix_melenchon_2022 - voix_lfi_euro,
    # Vote LFI et vote à gauche hors LFI aux européennes 2024, en % des inscrits
    lfi_euro24 = pct(voix_lfi_euro, inscrits_euro_2024),
    gauche_hors_lfi_euro24 = pct(voix_gauche_hors_lfi_euro, inscrits_euro_2024)
  ) |>
  select(id_bureau, code_departement, code_commune, libelle_commune, code_bv,
         vote_melenchon_2022, vote_lfi_euro_2024, vote_nfp_legi_2024,
         taux_abstention_2022, vote_gauche_hors_lfi_2022, vote_extreme_gauche_2022,
         vote_centre_2022, vote_droite_2022, vote_extreme_droite_2022, vote_divers_2022,
         melenchon22, gauche_hors_lfi22,
         lfi_euro24, ecart_melenchon22_lfi24, ecart_melenchon22_lfi24_voix, gauche_hors_lfi_euro24,
         inscrits_2022, exprimes_2022, inscrits_euro_2024, exprimes_euro_2024, exprimes_legi_2024) |>
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

pond <- function(x, w) round(sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE), 2)
message("France (pondéré par les inscrits) : écart Mélenchon 2022 - LFI 2024 = ",
        pond(table_bv$ecart_melenchon22_lfi24, table_bv$inscrits_2022), " points (",
        format(sum(table_bv$ecart_melenchon22_lfi24_voix, na.rm = TRUE), big.mark = " "), " voix) ; ",
        "gauche hors LFI européennes 2024 = ", pond(table_bv$gauche_hors_lfi_euro24, table_bv$inscrits_euro_2024),
        " % des inscrits.")

# Point-virgule et virgule décimale, avec BOM : s'ouvre correctement dans Excel
fwrite(table_bv, FICHIER_SORTIE, sep = ";", dec = ",", bom = TRUE, na = "")
message("Table écrite : ", normalizePath(FICHIER_SORTIE))

# ---- 6. Typologie du potentiel de vote LFI ------------------------------------------------
# Trois variables de la présidentielle 2022 (1er tour), toutes en % des inscrits :
#   melenchon22           vote Mélenchon
#   gauche_hors_lfi22     vote Roussel + Hidalgo + Jadot
#   taux_abstention_2022  abstention
# Les européennes 2024, scrutin trop particulier (faible participation, listes de gauche
# séparées), ne sont pas utilisées.
# ACP normée (chaque variable pèse autant), bureaux pondérés par leurs inscrits.
# Construite sur la France métropolitaine (Corse comprise) : l'outre-mer et les Français
# de l'étranger, à l'abstention très forte, accaparaient sinon des types entiers.
# Ils restent « non classés ».
VARS_TYPO   <- c("melenchon22", "gauche_hors_lfi22", "taux_abstention_2022")
SEUIL_EXPRIMES <- 10   # bureaux plus petits : non classés (résultats trop instables)

df <- table_bv |>
  filter(exprimes_2022 >= SEUIL_EXPRIMES,
         !(substr(code_departement, 1, 1) == "Z" | substr(code_commune, 1, 2) %in% c("97", "98"))) |>
  select(id_bureau, inscrits_2022, exprimes_2022, all_of(VARS_TYPO)) |>
  na.omit() |>
  as.data.frame()
w <- df$inscrits_2022

acp <- PCA(df[, VARS_TYPO], scale.unit = TRUE, ncp = length(VARS_TYPO), row.w = w, graph = FALSE)
acp$eig                                       # tous les axes conservés : toute l'information
coord <- acp$ind$coord

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

# Barre n°k : inertie perdue en passant de k+1 à k classes
plot(rev(arbre$height)[1:20], type = "h",
     xlab = "Passage de k+1 à k classes", ylab = "Gain d'inertie")

# 3. Coupure puis consolidation k-means initialisée sur les centres de classes
k       <- 6
classe0 <- cutree(arbre, k)[km$cluster]
init    <- apply(coord, 2, function(x) tapply(x * w, classe0, sum)) /
  as.vector(tapply(w, classe0, sum))
km2 <- kmeans(coord, centers = init, iter.max = 100)

# 4. Numérotation par profil (et non par l'ordre arbitraire du k-means), pour que
#    chaque numéro garde le même sens d'une exécution à l'autre :
#    5 = plus fort vote Mélenchon ; puis parmi le reste : 3 = plus forte gauche hors LFI ;
#    6 = plus forte abstention ; 4 = plus fort vote Mélenchon ;
#    1 et 2 = les deux derniers (1 = le plus faible vote Mélenchon).
#    Couleurs des cartes par paires : 1-2 bleus, 3-4 roses, 5-6 violets.
if (k != 6) stop("La numérotation par profil est écrite pour 6 types : l'adapter si k change.")
moy <- as.data.frame(sapply(VARS_TYPO, function(v) tapply(df[[v]] * w, km2$cluster, sum) / tapply(w, km2$cluster, sum)))
moy$brut <- as.integer(rownames(moy))
choisir <- function(candidats, critere, plus_grand = TRUE) {
  x <- moy[moy$brut %in% candidats, ]
  x$brut[if (plus_grand) which.max(x[[critere]]) else which.min(x[[critere]])]
}
reste <- moy$brut
t5 <- choisir(reste, "melenchon22");                         reste <- setdiff(reste, t5)
t3 <- choisir(reste, "gauche_hors_lfi22");                   reste <- setdiff(reste, t3)
t6 <- choisir(reste, "taux_abstention_2022");                reste <- setdiff(reste, t6)
t4 <- choisir(reste, "melenchon22");                         reste <- setdiff(reste, t4)
t1 <- choisir(reste, "melenchon22", plus_grand = FALSE)
t2 <- setdiff(reste, t1)
correspondance <- setNames(1:6, c(t1, t2, t3, t4, t5, t6))
df$classe <- factor(correspondance[as.character(km2$cluster)], levels = 1:6)

classes_description <- c(
  "Gauche très faible"                                             = 1,
  "Gauche faible, très forte participation"                        = 2,
  "Gauche forte, avec une forte gauche hors LFI"                   = 3,
  "Profil moyen"                                                   = 4,
  "Vote Mélenchon très fort, forte abstention"                     = 5,
  "Forte abstention, gauche faible"                                = 6
)
df$classe_lib <- factor(df$classe, levels = classes_description, labels = names(classes_description))

# 5. Description des classes
desc <- catdes(df[, c(VARS_TYPO, "classe")], num.var = length(VARS_TYPO) + 1, row.w = w)
desc$quanti

# ---- 7. Export pour les cartes ------------------------------------------------------------
# typologie_bureaux.csv : une ligne par bureau classé (les bureaux écartés par na.omit
#   ou le seuil d'exprimés n'y figurent pas : « non classés » sur les cartes).
#   poids = inscrits 2022, pour calculer la part de chaque type dans une zone.
# typologie_classes.csv : libellé et profil moyen de chaque type (pondéré par les inscrits).
classes_bv <- df |>
  transmute(id_bureau, classe = as.integer(as.character(classe)), poids = inscrits_2022) |>
  left_join(select(table_bv, id_bureau, code_commune, code_bv), by = "id_bureau")
fwrite(classes_bv, "typologie_bureaux.csv", sep = ";", bom = TRUE)

profils <- df |>
  mutate(classe = as.integer(as.character(classe))) |>
  group_by(classe) |>
  summarise(across(all_of(VARS_TYPO), ~ round(weighted.mean(.x, inscrits_2022), 1)),
            part_inscrits = sum(inscrits_2022)) |>
  mutate(part_inscrits = round(100 * part_inscrits / sum(part_inscrits), 1),
         libelle = names(classes_description)[match(classe, classes_description)]) |>
  relocate(classe, libelle, part_inscrits)
if (anyNA(profils$libelle)) stop("Une classe n'a pas de libellé dans classes_description.")
print(as.data.frame(profils))
fwrite(profils, "typologie_classes.csv", sep = ";", dec = ",", bom = TRUE)
message("Typologie exportée : typologie_bureaux.csv (", nrow(classes_bv), " bureaux) et typologie_classes.csv")
