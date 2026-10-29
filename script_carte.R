# =============================================================================
# Cartes électorales interactives de toutes les circonscriptions d'un département
# -----------------------------------------------------------------------------
# Produit un petit site (dossier de fichiers HTML autonomes) :
#   - index.html    : vue d'ensemble du département, une zone par circonscription ;
#                     un clic sur une circonscription ouvre sa carte détaillée
#   - circo-1.html, circo-2.html… : une carte par circonscription, commune par commune
# Une barre de navigation en haut de chaque page permet de passer de l'une à l'autre
# en gardant le scrutin affiché.
#
# Sur chaque carte, 5 boutons :
#   - Présidentielle 2022 : score de Jean-Luc Mélenchon (1er tour)
#   - Européennes 2024    : score de la liste LFI (Manon Aubry)
#   - Législatives 2024   : score du NFP (nuance UG, 1er tour)
#   - Municipales 2026    : nombre de listes au 1er tour
#   - Inscrits 2026       : inscrits au 1er tour des municipales
# + la couche « abstentionnistes » (cercles proportionnels).
#
# Sources (téléchargées automatiquement au premier lancement, ~330 Mo) :
#   - Résultats : « Données des élections agrégées », ministère de l'Intérieur
#     https://www.data.gouv.fr/fr/datasets/6481e741d4cf002ec0efec9d/
#   - Contours et circonscription de chaque bureau de vote : « Proposition de
#     contours des bureaux de vote selon la méthode de l'INSEE »
#     https://www.data.gouv.fr/fr/datasets/667af5823c780f46db9adf59/
#
# Paquets : arrow, dplyr, sf, jsonlite
# =============================================================================

# ---- PARAMÈTRES : à modifier -------------------------------------------------
DEPARTEMENT <- "21"     # code du département : "21" Côte-d'Or, "44" Loire-Atlantique, "2A"…
CIRCOS      <- NULL     # NULL = toutes les circonscriptions ; ou par ex. c(1, 4)
SEUIL_BUREAUX <- 5      # communes d'au moins 5 bureaux : une zone par bureau de vote
# (1 = tout au bureau de vote ; Inf = tout à la commune)
DOSSIER_DONNEES <- "donnees_elections"                # cache des fichiers téléchargés
# Typologie des bureaux, produite par typologie.R (bouton « Typologie » absent si fichiers absents)
FICHIER_TYPO_BUREAUX <- "typologie_bureaux.csv"
FICHIER_TYPO_CLASSES <- "typologie_classes.csv"
DOSSIER_SORTIE  <- "C:/Users/leoni/Documents/CoteDor" # où écrire le site
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(sf)
  library(jsonlite)
  library(data.table)
})

options(timeout = 3600)
ordinal <- function(n) ifelse(n == 1, "1re", paste0(n, "e"))
fichier_circo <- function(n) sprintf("circo-%d.html", as.integer(n))

# ---- 1. Téléchargement (une seule fois) --------------------------------------
dir.create(DOSSIER_DONNEES, showWarnings = FALSE)
sources <- c(
  "candidats_results.parquet" = "https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/candidats_results.parquet",
  "general_results.parquet"   = "https://data-pipeline-open.s3.sbg.io.cloud.ovh.net/elections/general_results.parquet",
  "contours_bureaux_vote.json" = "https://static.data.gouv.fr/resources/proposition-de-contours-des-bureaux-de-vote-selon-la-methode-de-linsee/20240711-174028/contours-bureaux-vote.json"
)
chemin <- function(f) file.path(DOSSIER_DONNEES, f)
for (f in names(sources)) {
  if (!file.exists(chemin(f))) {
    message("Téléchargement de ", f, "…")
    download.file(sources[[f]], chemin(f), mode = "wb")
  }
}

# ---- 2. Bureaux de vote du département ---------------------------------------
message("Lecture des contours des bureaux de vote…")
bv_tous <- st_read(chemin("contours_bureaux_vote.json"), quiet = TRUE) |>
  filter(codeDepartement == DEPARTEMENT) |>
  st_set_crs(4326)

if (nrow(bv_tous) == 0) stop("Département inconnu : ", DEPARTEMENT)

nom_departement <- bv_tous$nomDepartement[1]
circos_dispo <- sort(unique(na.omit(as.integer(bv_tous$codeCirco))))
if (is.null(CIRCOS)) CIRCOS <- circos_dispo
inconnues <- setdiff(CIRCOS, circos_dispo)
if (length(inconnues)) {
  stop("Circonscription(s) ", paste(inconnues, collapse = ", "), " absente(s) du département ",
       DEPARTEMENT, ". Circonscriptions disponibles : ", paste(circos_dispo, collapse = ", "))
}

bv_par_commune <- st_drop_geometry(bv_tous) |> count(codeCommune, name = "nb_bv_commune")

candidats <- open_dataset(chemin("candidats_results.parquet"))
generaux  <- open_dataset(chemin("general_results.parquet"))

# Contrôle : dans les communes coupées entre plusieurs circos ou découpées par bureau,
# un bureau absent du fichier de contours ne peut être placé sur la carte : ses voix
# ne sont pas comptées. Même chose si un bureau a été renuméroté entre deux élections.
communes_coupees <- st_drop_geometry(bv_tous) |>
  filter(!is.na(codeCirco)) |>
  group_by(codeCommune) |>
  filter(n_distinct(codeCirco) > 1) |>
  distinct(codeCommune) |>
  pull()
communes_par_bureau <- bv_par_commune$codeCommune[bv_par_commune$nb_bv_commune >= SEUIL_BUREAUX]
for (election in c("2022_pres_t1", "2024_euro_t1", "2024_legi_t1", "2026_muni_t1")) {
  orphelins <- generaux |>
    filter(id_election == election,
           code_commune %in% union(communes_coupees, communes_par_bureau)) |>
    select(code_commune, libelle_commune, code_bv, exprimes) |>
    collect() |>
    anti_join(st_drop_geometry(bv_tous), by = c("code_commune" = "codeCommune", "code_bv" = "codeBureauVote"))
  if (nrow(orphelins)) {
    message("À noter (", election, ") : bureau(x) absent(s) des contours, non compté(s) : ",
            paste0(orphelins$libelle_commune, " n°", orphelins$code_bv, " (", orphelins$exprimes, " exprimés)",
                   collapse = ", "))
  }
}

# ---- Typologie des bureaux (facultative) ---------------------------------------
# Trois familles, un clair et un foncé chacune : types 1-2 bleus (gauche faible),
# 3-4 roses (gauche moyenne), 5-6 violets (gauche forte, dominante LFI).
# Toutes les paires sont bien distinctes en vision normale ; pour les daltoniens
# (deutéranopie), bleu et violet se rapprochent : le numéro du type est donc
# toujours écrit à côté de la couleur.
COULEURS_TYPO <- c("#39c3d8", "#0f5f8c", "#f19bc1", "#fde4ee", "#6f2a8e", "#9f88ec")
TYPO <- NULL
if (file.exists(FICHIER_TYPO_BUREAUX) && file.exists(FICHIER_TYPO_CLASSES)) {
  typo_classes <- fread(FICHIER_TYPO_CLASSES, sep = ";", dec = ",", encoding = "UTF-8")
  if (nrow(typo_classes) > length(COULEURS_TYPO)) {
    stop(nrow(typo_classes), " types dans la typologie : ajouter des couleurs à COULEURS_TYPO.")
  }
  typo_bv <- fread(FICHIER_TYPO_BUREAUX, sep = ";", encoding = "UTF-8",
                   colClasses = list(character = c("id_bureau", "code_commune", "code_bv"))) |>
    as_tibble() |>
    filter(code_commune %in% bv_par_commune$codeCommune)
  TYPO <- list(
    bv = typo_bv,
    classes = lapply(seq_len(nrow(typo_classes)), function(i) {
      r <- typo_classes[i, ]
      list(num = r$classe, libelle = r$libelle, couleur = COULEURS_TYPO[r$classe],
           part_france = r$part_exprimes,
           profil = list(abstention = r$taux_abstention_2022, melenchon = r$vote_melenchon_2022,
                         lfi_euro = r$vote_lfi_euro_2024, nfp = r$vote_nfp_legi_2024,
                         gauche_hors_lfi = r$vote_gauche_hors_lfi_2022, centre = r$vote_centre_2022,
                         extreme_droite = r$vote_extreme_droite_2022))
    })
  )
  message("Typologie chargée : ", nrow(typo_classes), " types, ", nrow(typo_bv), " bureaux classés dans le département.")
} else {
  message("Pas de typologie (", FICHIER_TYPO_BUREAUX, " absent) : lancer typologie.R pour l'ajouter aux cartes.")
}

# Type dominant d'une zone et part de chaque type, en % des exprimés de la présidentielle 2022
# (le reste correspond aux bureaux non classés).
typo_de <- function(lignes, denom) {
  if (is.null(lignes) || !nrow(lignes) || is.na(denom) || denom == 0) {
    return(list(classe = NULL, parts = setNames(list(), character(0))))
  }
  lignes <- arrange(lignes, desc(e))
  list(classe = lignes$classe[1],
       parts = as.list(setNames(round(100 * lignes$e / denom, 1), lignes$classe)))
}

# Point garanti à l'intérieur de chaque zone (le centroïde peut tomber dehors) :
# emplacement des cercles proportionnels.
centres_de <- function(geo) {
  centres <- geo |> st_set_agr("constant") |> st_transform(2154) |> st_point_on_surface() |> st_transform(4326)
  xy <- st_coordinates(centres)
  setNames(lapply(seq_len(nrow(centres)), function(i) round(c(xy[i, "Y"], xy[i, "X"]), 5)), centres$zone)
}

# Garde uniquement les polygones : la réparation des contours (st_make_valid) laisse
# parfois des lignes ou des points parasites dans une « GeometryCollection »,
# que la carte affiche mal (zoom et contours faux).
polygones <- function(x) {
  if (all(st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"))) x
  else st_collection_extract(x, "POLYGON")
}

# Fusionne les bureaux de vote par zone, simplifie légèrement les contours
fusionner <- function(bv, tolerance) {
  bv |>
    st_transform(2154) |>
    st_make_valid() |>
    polygones() |>
    group_by(zone) |>
    summarise(.groups = "drop") |>
    st_simplify(dTolerance = tolerance, preserveTopology = TRUE) |>
    polygones() |>
    st_cast("MULTIPOLYGON", warn = FALSE) |>
    st_transform(4326)
}

total_score <- function(tab) {
  list(voix = sum(tab$voix), exprimes = sum(tab$exprimes),
       pct = round(100 * sum(tab$voix) / sum(tab$exprimes), 2),
       inscrits = sum(tab$inscrits), abstentions = sum(tab$abstentions),
       pct_abst = round(100 * sum(tab$abstentions) / sum(tab$inscrits), 2))
}

# ---- 3. Calcul d'une circonscription -----------------------------------------
preparer_circo <- function(num) {
  code_circo <- sprintf("%02d", as.integer(num))
  bv_circo <- bv_tous |> filter(codeCirco == code_circo)
  
  # Une commune est « entière » si tous ses bureaux sont dans la circonscription,
  # « partielle » sinon (grandes villes découpées entre plusieurs circonscriptions).
  # Les communes d'au moins SEUIL_BUREAUX bureaux sont « par bureau » : une zone par
  # bureau de vote, pour voir les écarts entre quartiers.
  communes <- st_drop_geometry(bv_circo) |>
    count(codeCommune, nomCommune, name = "nb_bv_circo") |>
    left_join(bv_par_commune, by = "codeCommune") |>
    mutate(
      partielle = nb_bv_circo < nb_bv_commune,
      par_bureau = nb_bv_commune >= SEUIL_BUREAUX
    )
  
  # Une ligne par zone de la carte. `bv_zone` donne la zone de chaque bureau de la circo.
  bv_zone <- st_drop_geometry(bv_circo) |>
    select(code_commune = codeCommune, code_bv = codeBureauVote) |>
    left_join(communes, by = c("code_commune" = "codeCommune")) |>
    mutate(zone = case_when(
      par_bureau ~ paste0(code_commune, "-", code_bv),
      partielle  ~ paste0(code_commune, "P"),
      TRUE       ~ code_commune
    ))
  zones_tab <- bv_zone |>
    mutate(code_bv = ifelse(par_bureau, code_bv, NA_character_)) |>
    distinct(zone, code_commune, code_bv, nomCommune, nb_bv_circo, partielle, par_bureau) |>
    mutate(
      nom = case_when(
        par_bureau ~ paste0(nomCommune, " – bureau n°", as.integer(code_bv)),
        partielle  ~ paste0(nomCommune, " (partie, ", nb_bv_circo, " bureaux)"),
        TRUE       ~ nomCommune
      ),
      nom_court = case_when(
        par_bureau ~ paste0(nomCommune, " ", as.integer(code_bv)),
        partielle  ~ paste0(nomCommune, " (partie)"),
        TRUE       ~ nomCommune
      )
    ) |>
    arrange(zone)
  
  message(ordinal(num), " circonscription : ", nrow(communes), " communes (",
          sum(communes$partielle), " partielle(s), ", sum(communes$par_bureau), " découpée(s) par bureau) -> ",
          nrow(zones_tab), " zones.")
  
  geo <- bv_circo |>
    left_join(select(bv_zone, code_commune, code_bv, zone),
              by = c("codeCommune" = "code_commune", "codeBureauVote" = "code_bv")) |>
    fusionner(tolerance = 15)
  
  codes_communes <- communes$codeCommune
  
  # Rattache chaque ligne de résultat (bureau de vote) à une zone de la carte :
  # commune entière -> tous ses bureaux (même renumérotés entre deux élections) ;
  # commune partielle ou découpée par bureau -> bureau par bureau.
  communes_entieres <- communes |> filter(!partielle, !par_bureau) |> select(code_commune = codeCommune)
  vers_zone <- function(df) {
    entieres <- df |>
      semi_join(communes_entieres, by = "code_commune") |>
      mutate(zone = code_commune)
    au_bureau <- df |>
      anti_join(communes_entieres, by = "code_commune") |>
      inner_join(select(bv_zone, code_commune, code_bv, zone), by = c("code_commune", "code_bv"))
    bind_rows(entieres, au_bureau)
  }
  
  lire_generaux <- function(election) {
    generaux |>
      filter(id_election == election, code_commune %in% codes_communes) |>
      select(code_commune, code_bv, inscrits, votants, exprimes) |>
      collect() |>
      vers_zone() |>
      group_by(zone) |>
      summarise(inscrits = sum(inscrits, na.rm = TRUE),
                votants  = sum(votants,  na.rm = TRUE),
                exprimes = sum(exprimes, na.rm = TRUE)) |>
      # Abstentionnistes = inscrits qui n'ont pas voté (blancs et nuls exclus : ce sont des votants)
      mutate(abstentions = inscrits - votants)
  }
  
  lire_candidats <- function(election) {
    candidats |>
      filter(id_election == election, code_commune %in% codes_communes) |>
      select(code_commune, code_bv, no_panneau, nuance, nom, prenom, libelle_abrege_liste, voix) |>
      collect()
  }
  
  score <- function(election, filtre) {
    voix <- lire_candidats(election) |>
      filter({{ filtre }}) |>
      vers_zone() |>
      group_by(zone) |>
      summarise(voix = sum(voix, na.rm = TRUE))
    lire_generaux(election) |>
      left_join(voix, by = "zone") |>
      mutate(voix = coalesce(voix, 0L), pct = round(100 * voix / exprimes, 2))
  }
  
  scores <- list(
    pres2022 = score("2022_pres_t1", prenom == "Jean-Luc" & grepl("LENCHON", nom)),
    euro2024 = score("2024_euro_t1", nuance == "LFI"),
    legi2024 = score("2024_legi_t1", nuance %in% "UG")
  )
  
  # Municipales 2026 : listes (à l'échelle de la commune entière) et inscrits (de la zone)
  cand_2026 <- lire_candidats("2026_muni_t1") |>
    mutate(id_liste = coalesce(as.character(no_panneau), libelle_abrege_liste))
  listes_2026 <- cand_2026 |>
    distinct(code_commune, id_liste, libelle_abrege_liste) |>
    group_by(code_commune) |>
    summarise(nb_listes = n_distinct(id_liste),
              listes = list(sort(unique(na.omit(libelle_abrege_liste)))))
  inscrits_2026 <- lire_generaux("2026_muni_t1") |> select(zone, inscrits)
  
  # Typologie : exprimés 2022 de chaque type, par zone (mêmes règles de rattachement)
  typo_zone <- NULL
  if (!is.null(TYPO)) {
    typo_zone <- TYPO$bv |>
      filter(code_commune %in% codes_communes) |>
      vers_zone() |>
      group_by(zone, classe) |>
      summarise(e = sum(exprimes_2022), .groups = "drop")
  }
  typo_zone_de <- function(z) {
    if (is.null(typo_zone)) return(NULL)
    typo_de(filter(typo_zone, zone == z), scores$pres2022$exprimes[scores$pres2022$zone == z][1])
  }
  
  ligne_score <- function(tab, z) {
    r <- tab[tab$zone == z, ]
    if (nrow(r) == 0 || r$exprimes == 0) {
      return(list(voix = NULL, exprimes = NULL, pct = NULL,
                  inscrits = NULL, abstentions = NULL, pct_abst = NULL))
    }
    list(voix = r$voix, exprimes = r$exprimes, pct = r$pct,
         inscrits = r$inscrits, abstentions = r$abstentions,
         pct_abst = round(100 * r$abstentions / r$inscrits, 2))
  }
  
  centre_zone <- centres_de(geo)
  zones <- list()
  for (i in seq_len(nrow(zones_tab))) {
    zt <- zones_tab[i, ]
    z <- zt$zone
    l <- listes_2026[listes_2026$code_commune == zt$code_commune, ]
    ins <- inscrits_2026$inscrits[inscrits_2026$zone == z]
    zones[[z]] <- list(
      nom = zt$nom, nom_court = zt$nom_court, partielle = zt$partielle, bureau = zt$par_bureau,
      centre = centre_zone[[z]],
      pres2022 = ligne_score(scores$pres2022, z),
      euro2024 = ligne_score(scores$euro2024, z),
      legi2024 = ligne_score(scores$legi2024, z),
      muni2026 = list(
        nb_listes = if (nrow(l)) l$nb_listes else NULL,
        # I() : garder un tableau JSON même avec une seule liste (sinon auto_unbox en fait un texte)
        listes = I(if (nrow(l)) l$listes[[1]] else character(0)),
        inscrits = if (length(ins)) ins else NULL
      ),
      typo = typo_zone_de(z)
    )
  }
  
  # Compté par commune (et non par zone) : Dijon découpée en 46 bureaux reste une commune
  nb_listes_commune <- listes_2026$nb_listes[listes_2026$code_commune %in% codes_communes]
  total <- list(
    pres2022 = total_score(scores$pres2022),
    euro2024 = total_score(scores$euro2024),
    legi2024 = total_score(scores$legi2024),
    muni2026 = list(
      inscrits = sum(inscrits_2026$inscrits),
      communes_plusieurs = sum(nb_listes_commune > 1),
      communes = nrow(communes)
    ),
    typo = if (is.null(typo_zone)) NULL else
      typo_de(typo_zone |> group_by(classe) |> summarise(e = sum(e)), sum(scores$pres2022$exprimes))
  )
  
  # Contrôle : zones dont les bureaux n'ont pas été retrouvés
  # (numérotation des bureaux changée entre deux élections).
  for (k in names(scores)) {
    manquantes <- setdiff(zones_tab$zone, scores[[k]]$zone)
    if (length(manquantes)) {
      warning(ordinal(num), " circo, ", k, " : pas de résultat pour ", paste(manquantes, collapse = ", "),
              " (bureaux renumérotés ?) — affiché « n.d. » sur la carte.")
    }
  }
  
  list(
    num = num, zones = zones, total = total, geo = geo, scores = scores, typo_zone = typo_zone,
    codes_communes = codes_communes,
    titre = sprintf("%s circonscription – %s (%s)", ordinal(num), nom_departement, DEPARTEMENT),
    # I() : garder un tableau JSON même avec une seule commune partielle
    partielles = I(paste0(communes$nomCommune, " (", communes$nb_bv_circo, " bureaux)")[communes$partielle]),
    etiquettes = nrow(zones_tab) <= 25,
    # nombre de listes par commune entière, pour le total du département
    listes_communes = setNames(listes_2026$nb_listes, listes_2026$code_commune)
  )
}

# ---- 4. Écriture d'une page ------------------------------------------------------
navigation <- function(actif) {
  c(list(list(label = paste0("Vue d'ensemble – ", nom_departement), href = "index.html",
              actif = identical(actif, "index"))),
    lapply(CIRCOS, function(n) list(label = paste(ordinal(n), "circo"), href = fichier_circo(n),
                                    actif = identical(actif, n))))
}

ecrire_page <- function(meta, zones, total, geo, fichier) {
  fichier_geo <- tempfile(fileext = ".geojson")
  # Pas d'option COORDINATE_PRECISION : en arrondissant, GDAL « répare » les formes
  # et y glisse des lignes et des points parasites. On raccourcit les coordonnées
  # nous-mêmes dans le texte : 5 décimales, soit environ 1 m.
  st_write(geo, fichier_geo, driver = "GeoJSON", quiet = TRUE)
  texte_geo <- paste(readLines(fichier_geo, encoding = "UTF-8", warn = FALSE), collapse = "")
  texte_geo <- gsub("(-?\\d+\\.\\d{5})\\d+", "\\1", texte_geo)
  json <- paste0(
    '{"meta":', toJSON(meta, auto_unbox = TRUE),
    ',"zones":', toJSON(zones, auto_unbox = TRUE, null = "null", digits = NA),
    ',"total":', toJSON(total, auto_unbox = TRUE, digits = NA),
    ',"geo":', texte_geo, "}"
  )
  html <- sub("__DATA__", json, GABARIT, fixed = TRUE)
  html <- sub("__TITRE__", meta$titre, html, fixed = TRUE)
  writeLines(html, file.path(DOSSIER_SORTIE, fichier), useBytes = TRUE)
}

# ---- 5. Gabarit HTML ---------------------------------------------------------------
GABARIT <- r"---[<!doctype html>
<html lang="fr">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>__TITRE__</title>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.css">
<script src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.js"></script>
<style>
  :root { --page:#f9f9f7; --surface:#fcfcfb; --ink:#0b0b0b; --ink-2:#52514e; --line:#e4e3df;
          --accent:#2a78d6; --accent-ink:#fff; --hover:#eef4fc; }
  * { box-sizing: border-box; }
  body { margin:0; background:var(--page); color:var(--ink); font:15px/1.45 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif; }
  header { padding:20px 20px 8px; max-width:1240px; margin:0 auto; }
  h1 { font-size:22px; margin:0 0 4px; }
  .sub { color:var(--ink-2); margin:0; }
  .circos { display:flex; flex-wrap:wrap; gap:6px; margin:0 0 14px; }
  .circos a { padding:5px 12px; border-radius:999px; border:1px solid var(--line); background:var(--surface);
              color:var(--ink); text-decoration:none; font-weight:600; font-size:14px; }
  .circos a:hover { border-color:var(--accent); }
  .circos a[aria-current="page"] { background:var(--ink); border-color:var(--ink); color:#fff; }
  .filtres { display:flex; flex-wrap:wrap; gap:8px; padding:12px 20px; max-width:1240px; margin:0 auto; }
  .filtres button { font:inherit; font-weight:600; padding:9px 14px; border-radius:12px; cursor:pointer; text-align:left;
                    border:1px solid var(--line); background:var(--surface); color:var(--ink); }
  .filtres button:hover { border-color:var(--accent); }
  .filtres button[aria-pressed="true"] { background:var(--accent); border-color:var(--accent); color:var(--accent-ink); }
  .filtres button small { display:block; font-weight:400; font-size:12px; opacity:.85; }
  .filtres button[hidden] { display:none; }
  .types { display:flex; flex-direction:column; gap:10px; margin-top:12px; font-size:13px; }
  .type { display:flex; gap:8px; align-items:flex-start; }
  .type .p { flex:none; margin-top:2px; }
  .type > div { flex:1; min-width:0; }
  .type small { display:block; color:var(--ink-2); font-size:12px; font-variant-numeric:tabular-nums; }
  .barre { height:6px; border-radius:3px; background:var(--line); margin:4px 0 2px; overflow:hidden; }
  .barre span { display:block; height:100%; border-radius:3px; }
  main { display:grid; grid-template-columns:minmax(0,1fr) 360px; gap:16px; padding:8px 20px 20px; max-width:1240px; margin:0 auto; }
  #carte { height:680px; border-radius:12px; border:1px solid var(--line); background:var(--surface); }
  aside { display:flex; flex-direction:column; gap:12px; min-width:0; }
  .bloc { background:var(--surface); border:1px solid var(--line); border-radius:12px; padding:14px 16px; }
  .bloc h2 { font-size:15px; margin:0 0 2px; }
  .total { font-size:30px; font-weight:700; line-height:1.1; margin:8px 0 2px; font-variant-numeric:tabular-nums; }
  .legende-barre { height:12px; border-radius:6px; margin:12px 0 4px; }
  .legende-bornes { display:flex; justify-content:space-between; color:var(--ink-2); font-size:13px; font-variant-numeric:tabular-nums; }
  .legende-cases { display:flex; flex-direction:column; gap:4px; margin-top:10px; font-size:13px; }
  .p { display:inline-block; width:14px; height:14px; border-radius:4px; vertical-align:-2px; margin-right:6px; border:1px solid rgba(0,0,0,.15); }
  .tableau { max-height:440px; overflow:auto; padding-top:0; }
  table { width:100%; border-collapse:collapse; font-size:14px; font-variant-numeric:tabular-nums; }
  th { position:sticky; top:0; background:var(--surface); text-align:left; color:var(--ink-2); font-weight:600; font-size:12px;
       padding:12px 4px 6px; border-bottom:1px solid var(--line); }
  td { padding:6px 4px; border-bottom:1px solid var(--line); }
  .n { text-align:right; white-space:nowrap; }
  tr.ligne { cursor:pointer; }
  tr.ligne:hover, tr.ligne.actif { background:var(--hover); }
  .pastille { display:inline-block; width:10px; height:10px; border-radius:3px; margin-right:6px; border:1px solid rgba(0,0,0,.15); }
  #recherche { width:100%; font:inherit; padding:7px 10px; border:1px solid var(--line); border-radius:8px; margin:12px 0 4px; }
  .note { color:var(--ink-2); font-size:12.5px; max-width:1240px; margin:0 auto; padding:0 20px 24px; }
  .leaflet-tooltip.etiquette { background:rgba(252,252,251,.88); border:0; box-shadow:none; border-radius:6px; padding:2px 6px;
                               font:600 12px/1.25 system-ui,sans-serif; color:#0b0b0b; text-align:center; }
  .leaflet-tooltip.etiquette::before { display:none; }
  .etiquette b { font-size:14px; }
  .leaflet-popup-content { font:14px/1.5 system-ui,sans-serif; margin:12px 14px; }
  .leaflet-popup-content h3 { margin:0 0 6px; font-size:15px; }
  .leaflet-popup-content ul { margin:4px 0 0; padding-left:18px; font-size:13px; }
  .leaflet-popup-content a.ouvrir { display:inline-block; margin-top:8px; padding:6px 12px; border-radius:8px;
                                    background:var(--accent); color:#fff; font-weight:600; text-decoration:none; }
  .filtres button.bascule { display:flex; align-items:center; gap:10px; border-style:dashed; }
  .filtres button.bascule[aria-pressed="true"] { background:#52514e; border-color:#52514e; border-style:solid; color:#fff; }
  .filtres button.bascule:disabled { opacity:.45; cursor:not-allowed; border-color:var(--line); }
  .coche { width:16px; height:16px; flex:none; border-radius:4px; border:1.5px solid currentColor; position:relative; }
  .bascule[aria-pressed="true"] .coche::after { content:""; position:absolute; left:4px; top:0; width:4px; height:9px;
                                                border:solid currentColor; border-width:0 2px 2px 0; transform:rotate(45deg); }
  .colonne-carte { display:flex; flex-direction:column; gap:12px; min-width:0; }
  .legende-abst { background:var(--surface); border:1px solid var(--line); border-radius:12px; padding:12px 16px;
                  display:flex; flex-wrap:wrap; align-items:center; gap:10px 28px; font-size:13px; }
  .legende-abst[hidden] { display:none; }
  .legende-abst h3 { font-size:13px; margin:0; flex-basis:100%; }
  .legende-abst svg { display:block; }
  .classes-abst { display:flex; flex-wrap:wrap; gap:4px 12px; }
  .legende-abst .sub { font-size:12px; flex-basis:100%; }
  @media (max-width:860px) {
    main { grid-template-columns:1fr; padding:8px 16px 16px; }
    header, .filtres, .note { padding-left:16px; padding-right:16px; }
    #carte { height:460px; }
  }
</style>
</head>
<body>
<header>
  <nav class="circos" id="circos" aria-label="Circonscriptions"></nav>
  <h1 id="titre"></h1>
  <p class="sub" id="consigne"></p>
</header>
<nav class="filtres" aria-label="Choix du scrutin">
  <button data-ind="pres2022">Présidentielle 2022<small>Mélenchon, 1er tour</small></button>
  <button data-ind="euro2024">Européennes 2024<small>LFI – Manon Aubry</small></button>
  <button data-ind="legi2024">Législatives 2024<small>NFP, 1er tour</small></button>
  <button data-ind="muni2026">Municipales 2026<small id="sous-titre-muni">Nombre de listes, 1er tour</small></button>
  <button data-ind="inscrits2026">Inscrits 2026<small>Municipales, 1er tour</small></button>
  <button data-ind="typo" hidden>Typologie<small>types de bureaux</small></button>
  <button type="button" class="bascule" id="voir-abst" aria-pressed="false">
    <span class="coche" aria-hidden="true"></span>
    <span>Afficher les abstentionnistes<small>cercles proportionnels</small></span>
  </button>
</nav>
<main>
  <div class="colonne-carte">
    <div id="carte" role="region" aria-label="Carte"></div>
    <section class="legende-abst" id="legende-abst" hidden></section>
  </div>
  <aside>
    <section class="bloc" id="resume"></section>
    <section class="bloc tableau">
      <input id="recherche" type="search">
      <table><thead id="thead"></thead><tbody id="tbody"></tbody></table>
    </section>
  </aside>
</main>
<p class="note" id="note"></p>

<script>
const DONNEES = __DATA__;
const META = DONNEES.meta;
const VUE = META.vue_ensemble;   // true sur la page du département (une zone = une circonscription)

// Dégradé violet, du clair au foncé (une seule teinte, clarté régulière)
const RAMPE = ["#e4defb", "#c7bbf5", "#a896ee", "#8a72e3", "#6c51d1", "#5238ad", "#3a2580"];
const SANS_DONNEE = "#d9d8d4";
const CLASSES_LISTES = [
  { min: 0, max: 1, couleur: "#e4e3df", texte: "1 liste (pas de choix)" },
  { min: 2, max: 2, couleur: "#a896ee", texte: "2 listes" },
  { min: 3, max: 3, couleur: "#6c51d1", texte: "3 listes" },
  { min: 4, max: 9999, couleur: "#3a2580", texte: "4 listes ou plus" },
];
const INDICATEURS = {
  pres2022: { titre: "Mélenchon - présidentielle 2022 (1er tour)", type: "pct", cle: "pres2022" },
  euro2024: { titre: "LFI (Manon Aubry) - européennes 2024", type: "pct", cle: "euro2024" },
  legi2024: { titre: "NFP – législatives 2024 (1er tour)", type: "pct", cle: "legi2024" },
  muni2026: { titre: "Nombre de listes - municipales 2026 (1er tour)", type: "listes" },
  inscrits2026: { titre: "Inscrits - municipales 2026 (1er tour)", type: "inscrits" },
};
// Vue d'ensemble : une circonscription n'a pas « un » nombre de listes,
// on affiche la part de ses communes qui en ont eu plusieurs.
if (VUE) {
  INDICATEURS.muni2026 = {
    titre: "Communes avec plus d'une liste - municipales 2026 (1er tour)", type: "pct", cle: "muni2026",
    entete2: "Communes", detail: d => `${d.muni2026.communes_plusieurs} / ${d.muni2026.communes}`,
    resume: t => `${t.communes_plusieurs} communes sur ${t.communes} ont eu plus d'une liste`,
  };
  document.getElementById("sous-titre-muni").textContent = "Communes à plusieurs listes";
}
// Typologie des bureaux (si typologie.R a été lancé) : une couleur par type, numéro toujours écrit
const TYPES = META.typo && META.typo.length ? Object.fromEntries(META.typo.map(t => [t.num, t])) : null;
if (TYPES) {
  INDICATEURS.typo = { titre: "Typologie des bureaux de vote", type: "typo" };
  const b = document.querySelector('.filtres button[data-ind="typo"]');
  b.hidden = false;
  b.querySelector("small").textContent = `${META.typo.length} types de bureaux`;
}
const typeDe = d => (d.typo && d.typo.classe != null) ? d.typo.classe : null;
const partDe = (d, k) => (d.typo && d.typo.parts && d.typo.parts[k] != null) ? d.typo.parts[k] : 0;

const Z = DONNEES.zones;
const fmtNb = n => n == null ? "n.d." : n.toLocaleString("fr-FR");
const fmtPct = p => p == null ? "n.d." : p.toLocaleString("fr-FR", { minimumFractionDigits: 1, maximumFractionDigits: 1 }) + " %";

document.getElementById("titre").textContent = META.titre;
document.title = META.titre;
document.getElementById("consigne").textContent = VUE
  ? "Cliquez sur un scrutin pour colorer la carte : plus c'est foncé, plus c'est élevé. Cliquez sur une circonscription pour ouvrir sa carte commune par commune."
  : "Cliquez sur un scrutin pour colorer la carte : plus c'est foncé, plus c'est élevé. Cliquez sur une commune (ou une ligne du tableau) pour tout son détail.";
const recherche = document.getElementById("recherche");
recherche.placeholder = VUE ? "Rechercher une circonscription…" : "Rechercher une commune…";
recherche.setAttribute("aria-label", recherche.placeholder);
document.getElementById("note").innerHTML =
  "Source : ministère de l'Intérieur, « Données des élections agrégées » (data.gouv.fr), résultats par bureau de vote additionnés par " +
  (VUE ? "circonscription" : Object.values(Z).some(d => d.bureau) ? "commune (ou gardés par bureau, voir plus bas)" : "commune") + " ; " +
  "contours et rattachement des bureaux aux circonscriptions : INSEE / data.gouv.fr (2024). Pourcentages calculés sur les suffrages exprimés. " +
  "NFP = candidats de nuance « Union de la gauche » (UG). Le dégradé va du plus bas au plus haut " +
  (VUE ? "des circonscriptions du département" : "de la circonscription") + " pour le scrutin affiché. " +
  "Abstentionnistes = inscrits moins votants (les blancs et nuls sont des votants) ; les cercles sont placés à l'intérieur de chaque zone. " +
  (!VUE && Object.values(Z).some(d => d.bureau)
    ? `Les communes d'au moins ${META.seuil_bureaux} bureaux de vote sont découpées par bureau (contours INSEE, approximatifs : un bureau regroupe des adresses, pas un quartier aux limites officielles) ; ` +
      "aux municipales, chaque bureau affiche le nombre de listes de toute sa commune. "
    : "") +
  (TYPES
    ? "Typologie : classification de tous les bureaux de vote de France selon leur profil (Mélenchon, blocs et abstention à la présidentielle 2022, LFI aux européennes 2024, NFP aux législatives 2024), " +
      "par analyse en composantes principales puis classification ascendante hiérarchique. Une zone de plusieurs bureaux prend le type qui y regroupe le plus d'exprimés ; " +
      "« non classé » : bureau de moins de 10 exprimés ou sans candidat NFP. "
    : "") +
  (META.partielles.length
    ? "Communes coupées entre plusieurs circonscriptions : seuls leurs bureaux de la circonscription sont comptés et dessinés (" +
      META.partielles.join(", ") + ") ; pour les municipales, leur nombre de listes est celui de toute la commune."
    : "");

// ---- Navigation entre circonscriptions --------------------------------------
// Le scrutin choisi et la couche abstentionnistes sont gardés dans l'adresse (#ind=…&abst=1),
// et transmis aux liens : on change de circonscription sans perdre ce qu'on regardait.
document.getElementById("circos").innerHTML = META.nav.map(n =>
  `<a data-href="${n.href}" href="${n.href}"${n.actif ? ' aria-current="page"' : ""}>${n.label}</a>`).join("");
const etatInitial = new URLSearchParams(location.hash.slice(1));
const suffixe = () => `#ind=${indicateur}${voirAbst ? "&abst=1" : ""}`;
function majLiens() {
  try { history.replaceState(null, "", suffixe()); } catch (e) { /* certains navigateurs refusent en local */ }
  document.querySelectorAll("#circos a").forEach(a => { a.href = a.dataset.href + suffixe(); });
}

function hexVersRgb(h) { const n = parseInt(h.slice(1), 16); return [n >> 16, (n >> 8) & 255, n & 255]; }
function couleurSequentielle(t) {
  t = Math.max(0, Math.min(1, t));
  const x = t * (RAMPE.length - 1), i = Math.min(Math.floor(x), RAMPE.length - 2), f = x - i;
  const a = hexVersRgb(RAMPE[i]), b = hexVersRgb(RAMPE[i + 1]);
  return "rgb(" + a.map((v, k) => Math.round(v + (b[k] - v) * f)).join(",") + ")";
}
function valeur(ind, z) {
  const d = Z[z], def = INDICATEURS[ind];
  if (def.type === "pct") return d[def.cle].pct;
  if (def.type === "listes") return d.muni2026.nb_listes;
  if (def.type === "typo") return typeDe(d);
  return d.muni2026.inscrits;
}
function texteValeur(ind, v) {
  const t = INDICATEURS[ind].type;
  if (t === "typo") return v == null ? "non classé" : "Type " + v;
  if (v == null) return "n.d.";
  if (t === "pct") return fmtPct(v);
  if (t === "listes") return v + (v > 1 ? " listes" : " liste");
  return fmtNb(v);
}
function couleurPour(ind, v, min, max) {
  if (v == null) return SANS_DONNEE;
  if (INDICATEURS[ind].type === "typo") return TYPES[v].couleur;
  if (INDICATEURS[ind].type === "listes") return CLASSES_LISTES.find(c => v >= c.min && v <= c.max).couleur;
  return couleurSequentielle(max > min ? (v - min) / (max - min) : 0.5);
}

const carte = L.map("carte", { zoomSnap: 0.25, scrollWheelZoom: false });
// Fond de carte IGN (Géoplateforme) : gratuit, sans clé, fonctionne en ouvrant le fichier depuis l'ordinateur
L.tileLayer("https://data.geopf.fr/wmts?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}", {
  attribution: '&copy; <a href="https://geoservices.ign.fr/">IGN – Plan IGN</a>', maxZoom: 18, opacity: 0.6,
}).addTo(carte);

let indicateur = INDICATEURS[etatInitial.get("ind")] ? etatInitial.get("ind") : "pres2022";
let zoneActive = null;
const couches = {};
const STYLE_BORD = { weight: META.etiquettes ? 2 : 1, color: "#fcfcfb" };
const STYLE_ACTIF = { weight: 3, color: "#0b0b0b" };

const calque = L.geoJSON(DONNEES.geo, {
  style: () => ({ ...STYLE_BORD, fillOpacity: 0.82 }),
  onEachFeature: (f, couche) => {
    const z = f.properties.zone;
    couches[z] = couche;
    couche.bindPopup(() => popup(z), { maxWidth: 340 });
    couche.on("click", () => selectionner(z, false));
    couche.on("mouseover", () => couche.setStyle(STYLE_ACTIF).bringToFront());
    couche.on("mouseout", () => couche.setStyle(z === zoneActive ? STYLE_ACTIF : STYLE_BORD));
  },
}).addTo(carte);
carte.fitBounds(calque.getBounds(), { padding: [10, 10] });

// ---- Abstentionnistes : cercles proportionnels ------------------------------
// Surface du cercle proportionnelle au nombre d'abstentionnistes (rayon ∝ racine),
// même échelle pour les trois scrutins afin qu'ils restent comparables.
// Teinte grise (neutre, pas une couleur de camp) selon le taux d'abstention.
// Les classes sont propres à chaque scrutin (quintiles des zones, bornes
// arrondies au point entier) : l'abstention n'a pas le même niveau à la
// présidentielle et aux européennes, des seuils communs écraseraient les contrastes.
const SCRUTINS_ABST = ["pres2022", "euro2024", "legi2024"];
const RAYON_MAX = 30;
const GRIS_ABST = ["#dcdbd7", "#b5b3ae", "#8a8883", "#5e5c58", "#33322f"];
const fmtBorne = b => b.toLocaleString("fr-FR") + " %";

function classesAbst(ind) {
  const v = Object.values(Z).map(d => d[ind].pct_abst).filter(p => p != null).sort((a, b) => a - b);
  if (!v.length) return [];
  const quantile = q => v[Math.min(v.length - 1, Math.floor(q * v.length))];
  // Bornes intérieures uniques, arrondies : moins de 5 classes si les valeurs se tassent
  const bornes = [...new Set([0.2, 0.4, 0.6, 0.8].map(q => Math.round(quantile(q))))]
    .filter(b => b > v[0] && b <= v[v.length - 1]);
  const lim = [-Infinity, ...bornes, Infinity];
  // Répartir les gris sur toute la gamme même s'il y a moins de 5 classes
  const n = lim.length - 1;
  return lim.slice(0, -1).map((min, i) => ({
    min, max: lim[i + 1],
    couleur: GRIS_ABST[n === 1 ? 2 : Math.round(i * (GRIS_ABST.length - 1) / (n - 1))],
    texte: n === 1 ? "toutes les zones" : i === 0 ? `< ${fmtBorne(lim[1])}` : i === n - 1 ? `${fmtBorne(min)} ou plus` : `${fmtBorne(min)} à ${fmtBorne(lim[i + 1])}`,
  }));
}
const CLASSES_ABST = Object.fromEntries(SCRUTINS_ABST.map(s => [s, classesAbst(s)]));
const couleurAbst = (ind, p) => p == null ? SANS_DONNEE : CLASSES_ABST[ind].find(c => p >= c.min && p < c.max).couleur;
const ABST_MAX = Math.max(1, ...Object.values(Z).flatMap(d => SCRUTINS_ABST.map(s => d[s].abstentions || 0)));
const rayon = n => RAYON_MAX * Math.sqrt(n / ABST_MAX);

carte.createPane("cercles");
carte.getPane("cercles").style.zIndex = 450; // au-dessus des aplats (400), sous les étiquettes (650)
const calqueAbst = L.layerGroup();
let voirAbst = etatInitial.get("abst") === "1";
if (voirAbst) {
  calqueAbst.addTo(carte);
  document.getElementById("voir-abst").setAttribute("aria-pressed", "true");
}

function dessinerCercles(ind) {
  calqueAbst.clearLayers();
  if (!SCRUTINS_ABST.includes(ind)) return;
  // Les plus gros d'abord, pour que les petits restent visibles par-dessus
  Object.keys(Z)
    .filter(z => Z[z].centre && Z[z][ind].abstentions != null)
    .sort((a, b) => Z[b][ind].abstentions - Z[a][ind].abstentions)
    .forEach(z => {
      const d = Z[z][ind];
      L.circleMarker(Z[z].centre, {
        pane: "cercles", radius: Math.max(rayon(d.abstentions), 2),
        fillColor: couleurAbst(ind, d.pct_abst), fillOpacity: 0.85, color: "#fcfcfb", weight: 1.2,
      })
        .bindTooltip(`${Z[z].nom_court}<br><b>${fmtNb(d.abstentions)}</b> abstentionnistes (${fmtPct(d.pct_abst)})`,
                     { sticky: true, className: "etiquette" })
        .on("click", () => { selectionner(z, false); couches[z].openPopup(); })
        .addTo(calqueAbst);
    });
}

function legendeAbst(ind) {
  // Cercles emboîtés : trois valeurs arrondies de la même échelle que la carte
  // Arrondi vers le bas à 1, 2 ou 5 × 10^n, pour ne jamais dépasser le maximum réel
  const arrondi = v => {
    const p = Math.pow(10, Math.floor(Math.log10(Math.max(v, 1))));
    return [5, 2, 1].map(k => k * p).find(x => x <= v) || p;
  };
  const valeurs = [...new Set([ABST_MAX, ABST_MAX / 5, ABST_MAX / 25].map(arrondi))]
    // pas deux étiquettes à moins de 13 px l'une de l'autre
    .filter((v, i, t) => i === 0 || 2 * (rayon(t[i - 1]) - rayon(v)) >= 13);
  const h = 2 * RAYON_MAX + 4, base = h - 2, cx = RAYON_MAX + 2;
  const cercles = valeurs.map(v => {
    const r = rayon(v);
    return `<circle cx="${cx}" cy="${base - r}" r="${r}" fill="none" stroke="#52514e"/>
            <line x1="${cx}" y1="${base - 2 * r}" x2="${2 * RAYON_MAX + 14}" y2="${base - 2 * r}" stroke="#52514e" stroke-dasharray="2 2"/>
            <text x="${2 * RAYON_MAX + 18}" y="${base - 2 * r + 4}" font-size="12" fill="currentColor">${fmtNb(v)}</text>`;
  }).join("");
  const NOMS_SCRUTINS = { pres2022: "présidentielle 2022 (1er tour)", euro2024: "européennes 2024", legi2024: "législatives 2024 (1er tour)" };
  return `<h3>Abstentionnistes – ${NOMS_SCRUTINS[ind]}</h3>
    <svg width="170" height="${h}" role="img" aria-label="Taille des cercles">${cercles}</svg>
    <div class="classes-abst">${CLASSES_ABST[ind].map(c => `<span><span class="p" style="background:${c.couleur}"></span>${c.texte}</span>`).join("")}</div>
    <p class="sub">Taille : nombre d'abstentionnistes. Même échelle de taille pour les trois scrutins. Gris : part des inscrits, en cinq classes d'effectifs égaux propres à ce scrutin.</p>`;
}

document.getElementById("voir-abst").addEventListener("click", e => {
  voirAbst = !voirAbst;
  e.currentTarget.setAttribute("aria-pressed", voirAbst);
  if (voirAbst) calqueAbst.addTo(carte); else carte.removeLayer(calqueAbst);
  afficher(indicateur);
});

function popup(z) {
  const d = Z[z];
  const abst = s => d[s].abstentions == null ? "" :
    ` <span style="color:#52514e">· abst. ${fmtPct(d[s].pct_abst)}</span>`;
  const l = [
    `<h3>${d.nom}</h3>`,
    `Mélenchon 2022 : <b>${fmtPct(d.pres2022.pct)}</b> (${fmtNb(d.pres2022.voix)} voix)${abst("pres2022")}<br>`,
    `LFI européennes 2024 : <b>${fmtPct(d.euro2024.pct)}</b> (${fmtNb(d.euro2024.voix)} voix)${abst("euro2024")}<br>`,
    `NFP législatives 2024 : <b>${fmtPct(d.legi2024.pct)}</b> (${fmtNb(d.legi2024.voix)} voix)${abst("legi2024")}<br>`,
    `Inscrits municipales 2026 : <b>${fmtNb(d.muni2026.inscrits)}</b><br>`,
  ];
  if (TYPES) {
    const k = typeDe(d);
    if (k == null) {
      l.push(`Typologie : <b>non classé</b><br>`);
    } else {
      const parts = Object.entries(d.typo.parts).sort((a, b) => b[1] - a[1]);
      l.push(`Typologie : <span class="p" style="background:${TYPES[k].couleur}"></span><b>Type ${k}</b> – ${TYPES[k].libelle}<br>`);
      // Zone de plusieurs bureaux de types différents : on montre la répartition
      if (parts.length > 1 || parts[0][1] < 99.5) {
        l.push(`<span style="color:#52514e;font-size:13px">Répartition des exprimés : ` +
          parts.map(([n, p]) => `type ${n} ${fmtPct(p)}`).join(" · ") +
          (parts.reduce((s, [, p]) => s + p, 0) < 99.5 ? " · reste non classé" : "") + `</span><br>`);
      }
    }
  }
  if (VUE) {
    l.push(`Municipales 2026 : <b>${d.muni2026.communes_plusieurs} communes sur ${d.muni2026.communes}</b> avec plus d'une liste<br>`);
    l.push(`<a class="ouvrir" href="${d.lien}${suffixe()}">Ouvrir la carte de la circo →</a>`);
    return l.join("");
  }
  l.push(`Municipales 2026 : <b>${texteValeur("muni2026", d.muni2026.nb_listes)}</b>${d.partielle || d.bureau ? " (toute la commune)" : ""}`);
  const listes = [].concat(d.muni2026.listes || []);
  if (listes.length) l.push("<ul>" + listes.map(x => `<li>${x}</li>`).join("") + "</ul>");
  return l.join("");
}

function selectionner(z, depuisTableau) {
  if (zoneActive && couches[zoneActive]) couches[zoneActive].setStyle(STYLE_BORD);
  zoneActive = z;
  couches[z].setStyle(STYLE_ACTIF).bringToFront();
  if (depuisTableau) {
    carte.fitBounds(couches[z].getBounds(), { maxZoom: 13, padding: [40, 40] });
    couches[z].openPopup();
  }
  document.querySelectorAll("tr.ligne").forEach(tr => tr.classList.toggle("actif", tr.dataset.zone === z));
}

function afficher(ind) {
  indicateur = ind;
  document.querySelectorAll(".filtres button[data-ind]").forEach(b => b.setAttribute("aria-pressed", b.dataset.ind === ind));
  const def = INDICATEURS[ind];
  const zones = Object.keys(Z);
  const vals = zones.map(z => valeur(ind, z)).filter(v => v != null);
  const min = Math.min(...vals), max = Math.max(...vals);

  zones.forEach(z => {
    const v = valeur(ind, z), c = couches[z];
    if (!c) return;
    c.setStyle({ fillColor: couleurPour(ind, v, min, max) });
    const contenu = `${Z[z].nom_court}<br><b>${def.type === "listes" && v != null ? v : texteValeur(ind, v)}</b>`;
    if (c.getTooltip()) c.setTooltipContent(contenu);
    else c.bindTooltip(contenu, META.etiquettes
      ? { permanent: true, direction: "center", className: "etiquette" }
      : { sticky: true, className: "etiquette" });
  });

  const T = DONNEES.total;
  const perimetre = VUE ? "tout le département" : "toute la circonscription";
  let html = `<h2>${def.titre}</h2>`;
  if (def.type === "pct") {
    const t = T[def.cle];
    html += `<div class="total">${fmtPct(t.pct)}</div><p class="sub">${def.resume ? def.resume(t) : `sur ${perimetre} · ${fmtNb(t.voix)} voix sur ${fmtNb(t.exprimes)} exprimés`}</p>`;
  } else if (def.type === "listes") {
    html += `<div class="total">${T.muni2026.communes_plusieurs} communes sur ${T.muni2026.communes}</div><p class="sub">ont eu plus d'une liste</p>`;
  } else if (def.type === "typo") {
    const k = typeDe(T);
    html += `<p class="sub" style="margin-top:6px">Chaque zone prend la couleur du type qui y regroupe le plus d'électeurs (exprimés 2022). ` +
      `Barre : part des exprimés de ${perimetre} dans des bureaux de ce type${k != null ? ` ; type le plus présent ici : <b>type ${k}</b>` : ""}.</p>`;
  } else {
    html += `<div class="total">${fmtNb(T.muni2026.inscrits)}</div><p class="sub">inscrits sur ${perimetre}</p>`;
  }
  if (def.type === "listes") {
    html += `<div class="legende-cases">` + CLASSES_LISTES.map(c => `<div><span class="p" style="background:${c.couleur}"></span>${c.texte}</div>`).join("") + `</div>`;
  } else if (def.type === "typo") {
    // Légende = liste des types avec leur profil moyen national, et leur poids ici
    html += `<div class="types">` + META.typo.map(t => {
      const ici = partDe(T, t.num), p = t.profil;
      return `<div class="type"><span class="p" style="background:${t.couleur}"></span><div>
        <b>Type ${t.num}</b> – ${t.libelle}
        <div class="barre"><span style="width:${Math.min(ici, 100)}%;background:${t.couleur}"></span></div>
        <small>${fmtPct(ici)} ici · ${fmtPct(t.part_france)} en France</small>
        <small>Profil moyen : Mélenchon ${fmtPct(p.melenchon)}, LFI euro ${fmtPct(p.lfi_euro)}, NFP ${fmtPct(p.nfp)}, gauche hors LFI ${fmtPct(p.gauche_hors_lfi)}, centre ${fmtPct(p.centre)}, ext. droite ${fmtPct(p.extreme_droite)}, abstention ${fmtPct(p.abstention)}</small>
      </div></div>`;
    }).join("") + `</div>`;
  } else {
    html += `<div class="legende-barre" style="background:linear-gradient(90deg,${RAMPE.join(",")})"></div>
      <div class="legende-bornes"><span>${texteValeur(ind, min)}</span><span>${texteValeur(ind, max)}</span></div>`;
  }
  document.getElementById("resume").innerHTML = html;

  // Cercles d'abstentionnistes : pour les trois scrutins, et pour la typologie
  // (abstention de la présidentielle 2022, celle qui entre dans la typologie)
  const scrutinAbst = ind === "typo" ? "pres2022" : ind;
  const abstPossible = SCRUTINS_ABST.includes(scrutinAbst);
  const avecAbst = voirAbst && abstPossible;
  document.getElementById("voir-abst").disabled = !abstPossible;
  dessinerCercles(avecAbst ? scrutinAbst : null);
  const legende = document.getElementById("legende-abst");
  legende.hidden = !avecAbst;
  legende.innerHTML = avecAbst ? legendeAbst(scrutinAbst) : "";
  if (avecAbst) {
    const t = DONNEES.total[scrutinAbst];
    document.getElementById("resume").insertAdjacentHTML("beforeend",
      `<p class="sub" style="margin-top:10px">${fmtNb(t.abstentions)} abstentionnistes sur ${fmtNb(t.inscrits)} inscrits (<b>${fmtPct(t.pct_abst)}</b>)</p>`);
  }

  const colonne = { pct: "Score", listes: "Listes", typo: "Type" }[def.type] || "Inscrits";
  document.getElementById("thead").innerHTML =
    `<tr><th>${VUE ? "Circonscription" : "Commune"}</th><th class="n">${colonne}</th>${def.type === "pct" ? `<th class="n">${def.entete2 || "Voix"}</th>` : ""}${def.type === "typo" ? '<th class="n">Part</th>' : ""}${avecAbst ? '<th class="n">Abst.</th>' : ""}</tr>`;
  // Typologie : tri par type, puis par part du type dans la zone ; sinon valeur décroissante
  const tri = def.type === "typo"
    ? (a, b) => (a.v ?? 99) - (b.v ?? 99) || partDe(Z[b.z], b.v) - partDe(Z[a.z], a.v)
    : (a, b) => (b.v ?? -1) - (a.v ?? -1);
  const lignes = zones.map(z => ({ z, v: valeur(ind, z) })).sort(tri);
  document.getElementById("tbody").innerHTML = lignes.map(({ z, v }) =>
    `<tr class="ligne${z === zoneActive ? " actif" : ""}" data-zone="${z}" data-nom="${Z[z].nom.toLowerCase()}">
      <td><span class="pastille" style="background:${couleurPour(ind, v, min, max)}"></span>${Z[z].nom_court}</td>
      <td class="n">${texteValeur(ind, v)}</td>
      ${def.type === "pct" ? `<td class="n">${def.detail ? def.detail(Z[z]) : fmtNb(Z[z][def.cle].voix)}</td>` : ""}
      ${def.type === "typo" ? `<td class="n">${v == null ? "" : fmtPct(partDe(Z[z], v))}</td>` : ""}
      ${avecAbst ? `<td class="n">${fmtPct(Z[z][scrutinAbst].pct_abst)}</td>` : ""}
    </tr>`).join("");
  document.querySelectorAll("tr.ligne").forEach(tr => tr.addEventListener("click", () => selectionner(tr.dataset.zone, true)));
  filtrer();
  majLiens();
}

function filtrer() {
  const q = recherche.value.trim().toLowerCase();
  document.querySelectorAll("tr.ligne").forEach(tr => { tr.style.display = !q || tr.dataset.nom.includes(q) ? "" : "none"; });
}
recherche.addEventListener("input", filtrer);
document.querySelectorAll(".filtres button[data-ind]").forEach(b => b.addEventListener("click", () => afficher(b.dataset.ind)));
afficher(indicateur);
</script>
</body>
</html>
]---"

# ---- 6. Génération du site ---------------------------------------------------------
dir.create(DOSSIER_SORTIE, showWarnings = FALSE, recursive = TRUE)

message("Calcul des circonscriptions…")
circos <- lapply(CIRCOS, preparer_circo)

for (res in circos) {
  meta <- list(titre = res$titre, partielles = res$partielles, etiquettes = res$etiquettes,
               vue_ensemble = FALSE, nav = navigation(res$num), seuil_bureaux = SEUIL_BUREAUX,
               typo = TYPO$classes)
  ecrire_page(meta, res$zones, res$total, res$geo, fichier_circo(res$num))
}

# Vue d'ensemble : une zone par circonscription, chiffres = totaux de chaque circo
message("Vue d'ensemble du département…")
geo_dep <- bv_tous |>
  filter(as.integer(codeCirco) %in% CIRCOS) |>
  mutate(zone = paste0("C", as.integer(codeCirco))) |>
  fusionner(tolerance = 40)
centre_dep <- centres_de(geo_dep)

zones_dep <- list()
for (res in circos) {
  z <- paste0("C", res$num)
  m <- res$total$muni2026
  zones_dep[[z]] <- list(
    nom = paste(ordinal(res$num), "circonscription"), nom_court = paste(ordinal(res$num), "circo"),
    partielle = FALSE, lien = fichier_circo(res$num), centre = centre_dep[[z]],
    pres2022 = res$total$pres2022, euro2024 = res$total$euro2024, legi2024 = res$total$legi2024,
    muni2026 = c(m, list(pct = round(100 * m$communes_plusieurs / m$communes, 2))),
    typo = res$total$typo
  )
}

# Totaux du département : somme des circonscriptions (chaque bureau n'est que dans une circo)
total_dep_score <- function(k) total_score(bind_rows(lapply(circos, function(r) r$scores[[k]])))
# Une commune coupée entre plusieurs circos ne compte qu'une fois
listes_dep <- unlist(lapply(circos, function(r) r$listes_communes))
listes_dep <- listes_dep[!duplicated(names(listes_dep))]
communes_dep <- unique(unlist(lapply(circos, function(r) r$codes_communes)))
plusieurs_dep <- sum(listes_dep[names(listes_dep) %in% communes_dep] > 1)
total_dep <- list(
  pres2022 = total_dep_score("pres2022"),
  euro2024 = total_dep_score("euro2024"),
  legi2024 = total_dep_score("legi2024"),
  muni2026 = list(
    inscrits = sum(sapply(circos, function(r) r$total$muni2026$inscrits)),
    communes_plusieurs = plusieurs_dep, communes = length(communes_dep),
    pct = round(100 * plusieurs_dep / length(communes_dep), 2)
  ),
  typo = if (is.null(TYPO)) NULL else
    typo_de(bind_rows(lapply(circos, function(r) r$typo_zone)) |> group_by(classe) |> summarise(e = sum(e)),
            total_dep_score("pres2022")$exprimes)
)

meta_dep <- list(
  titre = sprintf("%s (%s) – circonscriptions législatives", nom_departement, DEPARTEMENT),
  partielles = I(character(0)), etiquettes = TRUE, vue_ensemble = TRUE, nav = navigation("index"),
  typo = TYPO$classes
)
ecrire_page(meta_dep, zones_dep, total_dep, geo_dep, "index.html")

message("Site écrit dans ", normalizePath(DOSSIER_SORTIE), " : index.html + ",
        paste(fichier_circo(CIRCOS), collapse = ", "))
