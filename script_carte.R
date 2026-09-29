# =============================================================================
# Carte électorale interactive d'une circonscription législative
# -----------------------------------------------------------------------------
# Produit un fichier HTML autonome (à ouvrir par double-clic) avec 5 boutons :
#   - Présidentielle 2022 : score de Jean-Luc Mélenchon (1er tour)
#   - Européennes 2024    : score de la liste LFI (Manon Aubry)
#   - Législatives 2024   : score du NFP (nuance UG, 1er tour)
#   - Municipales 2026    : nombre de listes au 1er tour
#   - Inscrits 2026       : inscrits au 1er tour des municipales
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
DEPARTEMENT <- "21"   # code du département : "21" Côte-d'Or, "44" Loire-Atlantique, "2A"…
CIRCO       <- 4      # numéro de la circonscription
DOSSIER_DONNEES <- "donnees_elections"   # cache des fichiers téléchargés
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(sf)
  library(jsonlite)
})

options(timeout = 3600)
code_circo <- sprintf("%02d", as.integer(CIRCO))

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

# ---- 2. Bureaux de vote de la circonscription --------------------------------
message("Lecture des contours des bureaux de vote…")
bv_tous <- st_read(chemin("contours_bureaux_vote.json"), quiet = TRUE) |>
  filter(codeDepartement == DEPARTEMENT)

if (nrow(bv_tous) == 0) stop("Département inconnu : ", DEPARTEMENT)

bv_circo <- bv_tous |> filter(codeCirco == code_circo)
if (nrow(bv_circo) == 0) {
  stop("Pas de circonscription n°", CIRCO, " dans le département ", DEPARTEMENT,
       ". Circonscriptions disponibles : ",
       paste(sort(unique(na.omit(as.integer(bv_tous$codeCirco)))), collapse = ", "))
}

nom_departement <- bv_circo$nomDepartement[1]

# Une commune est « entière » si tous ses bureaux sont dans la circonscription,
# « partielle » sinon (grandes villes découpées entre plusieurs circonscriptions).
bv_par_commune <- st_drop_geometry(bv_tous) |> count(codeCommune, name = "nb_bv_commune")
communes <- st_drop_geometry(bv_circo) |>
  count(codeCommune, nomCommune, name = "nb_bv_circo") |>
  left_join(bv_par_commune, by = "codeCommune") |>
  mutate(
    partielle = nb_bv_circo < nb_bv_commune,
    zone = ifelse(partielle, paste0(codeCommune, "P"), codeCommune),
    nom = ifelse(partielle, paste0(nomCommune, " (partie, ", nb_bv_circo, " bureaux)"), nomCommune),
    nom_court = ifelse(partielle, paste0(nomCommune, " (partie)"), nomCommune)
  )

bv_partiels <- st_drop_geometry(bv_circo) |>
  semi_join(filter(communes, partielle), by = "codeCommune") |>
  transmute(code_commune = codeCommune, code_bv = codeBureauVote)

message(nrow(communes), " communes dans la circonscription (",
        sum(communes$partielle), " partielle(s)).")

# ---- 3. Géométries : fusion des bureaux par zone -----------------------------
geo <- bv_circo |>
  st_set_crs(4326) |>
  st_transform(2154) |>
  st_make_valid() |>
  left_join(select(communes, codeCommune, zone), by = "codeCommune") |>
  group_by(zone) |>
  summarise(.groups = "drop") |>
  st_simplify(dTolerance = 15, preserveTopology = TRUE) |>
  st_transform(4326)

# ---- 4. Résultats électoraux -------------------------------------------------
candidats <- open_dataset(chemin("candidats_results.parquet"))
generaux  <- open_dataset(chemin("general_results.parquet"))
codes_communes <- communes$codeCommune

# Rattache chaque ligne de résultat (bureau de vote) à une zone de la carte :
# commune entière -> tous ses bureaux ; commune partielle -> ses bureaux de la circo.
vers_zone <- function(df) {
  entieres <- df |>
    inner_join(filter(communes, !partielle) |> select(codeCommune, zone),
               by = c("code_commune" = "codeCommune"))
  partielles <- df |>
    semi_join(bv_partiels, by = c("code_commune", "code_bv")) |>
    mutate(zone = paste0(code_commune, "P"))
  bind_rows(entieres, partielles)
}

lire_generaux <- function(election) {
  generaux |>
    filter(id_election == election, code_commune %in% codes_communes) |>
    select(code_commune, code_bv, inscrits, exprimes) |>
    collect() |>
    vers_zone() |>
    group_by(zone) |>
    summarise(inscrits = sum(inscrits, na.rm = TRUE), exprimes = sum(exprimes, na.rm = TRUE))
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

message("Calcul des scores…")
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

# ---- 5. Assemblage des données pour la page ----------------------------------
ligne_score <- function(tab, z) {
  r <- tab[tab$zone == z, ]
  if (nrow(r) == 0 || r$exprimes == 0) return(list(voix = NULL, exprimes = NULL, pct = NULL))
  list(voix = r$voix, exprimes = r$exprimes, pct = r$pct)
}

zones <- list()
for (i in seq_len(nrow(communes))) {
  cm <- communes[i, ]
  z <- cm$zone
  l <- listes_2026[listes_2026$code_commune == cm$codeCommune, ]
  ins <- inscrits_2026$inscrits[inscrits_2026$zone == z]
  zones[[z]] <- list(
    nom = cm$nom, nom_court = cm$nom_court, partielle = cm$partielle,
    pres2022 = ligne_score(scores$pres2022, z),
    euro2024 = ligne_score(scores$euro2024, z),
    legi2024 = ligne_score(scores$legi2024, z),
    muni2026 = list(
      nb_listes = if (nrow(l)) l$nb_listes else NULL,
      listes = if (nrow(l)) l$listes[[1]] else character(0),
      inscrits = if (length(ins)) ins else NULL
    )
  )
}

total_score <- function(tab) {
  list(voix = sum(tab$voix), exprimes = sum(tab$exprimes),
       pct = round(100 * sum(tab$voix) / sum(tab$exprimes), 2))
}
nb_listes_zone <- sapply(zones, function(z) if (is.null(z$muni2026$nb_listes)) NA else z$muni2026$nb_listes)
total <- list(
  pres2022 = total_score(scores$pres2022),
  euro2024 = total_score(scores$euro2024),
  legi2024 = total_score(scores$legi2024),
  muni2026 = list(
    inscrits = sum(inscrits_2026$inscrits),
    communes_plusieurs = sum(nb_listes_zone > 1, na.rm = TRUE),
    communes = length(zones)
  )
)

# Contrôle : communes partielles dont les bureaux n'ont pas été retrouvés
# (numérotation des bureaux changée entre deux élections).
for (k in names(scores)) {
  manquantes <- setdiff(communes$zone, scores[[k]]$zone)
  if (length(manquantes)) {
    warning(k, " : pas de résultat pour ", paste(manquantes, collapse = ", "),
            " (bureaux renumérotés ?) — affiché « n.d. » sur la carte.")
  }
}

fichier_geo <- tempfile(fileext = ".geojson")
st_write(geo, fichier_geo, driver = "GeoJSON", quiet = TRUE,
         layer_options = "COORDINATE_PRECISION=5")
texte_geo <- paste(readLines(fichier_geo, encoding = "UTF-8", warn = FALSE), collapse = "")

meta <- list(
  titre = sprintf("%s circonscription – %s (%s)",
                  ifelse(CIRCO == 1, "1re", paste0(CIRCO, "e")), nom_departement, DEPARTEMENT),
  partielles = communes$nom[communes$partielle],
  etiquettes = nrow(communes) <= 25
)

json <- paste0(
  '{"meta":', toJSON(meta, auto_unbox = TRUE),
  ',"zones":', toJSON(zones, auto_unbox = TRUE, null = "null", digits = NA),
  ',"total":', toJSON(total, auto_unbox = TRUE, digits = NA),
  ',"geo":', texte_geo, "}"
)

# ---- 6. Page HTML --------------------------------------------------------------
gabarit <- r"---[<!doctype html>
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
  .filtres { display:flex; flex-wrap:wrap; gap:8px; padding:12px 20px; max-width:1240px; margin:0 auto; }
  .filtres button { font:inherit; font-weight:600; padding:9px 14px; border-radius:12px; cursor:pointer; text-align:left;
                    border:1px solid var(--line); background:var(--surface); color:var(--ink); }
  .filtres button:hover { border-color:var(--accent); }
  .filtres button[aria-pressed="true"] { background:var(--accent); border-color:var(--accent); color:var(--accent-ink); }
  .filtres button small { display:block; font-weight:400; font-size:12px; opacity:.85; }
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
  @media (max-width:860px) {
    main { grid-template-columns:1fr; padding:8px 16px 16px; }
    header, .filtres, .note { padding-left:16px; padding-right:16px; }
    #carte { height:460px; }
  }
</style>
</head>
<body>
<header>
  <h1 id="titre"></h1>
  <p class="sub">Cliquez sur un scrutin pour colorer la carte : plus c'est foncé, plus c'est élevé. Cliquez sur une commune (ou une ligne du tableau) pour tout son détail.</p>
</header>
<nav class="filtres" aria-label="Choix du scrutin">
  <button data-ind="pres2022">Présidentielle 2022<small>Mélenchon, 1er tour</small></button>
  <button data-ind="euro2024">Européennes 2024<small>LFI – Manon Aubry</small></button>
  <button data-ind="legi2024">Législatives 2024<small>NFP, 1er tour</small></button>
  <button data-ind="muni2026">Municipales 2026<small>Nombre de listes, 1er tour</small></button>
  <button data-ind="inscrits2026">Inscrits 2026<small>Municipales, 1er tour</small></button>
</nav>
<main>
  <div id="carte" role="region" aria-label="Carte de la circonscription"></div>
  <aside>
    <section class="bloc" id="resume"></section>
    <section class="bloc tableau">
      <input id="recherche" type="search" placeholder="Rechercher une commune…" aria-label="Rechercher une commune">
      <table><thead id="thead"></thead><tbody id="tbody"></tbody></table>
    </section>
  </aside>
</main>
<p class="note" id="note"></p>

<script>
const DONNEES = __DATA__;

const RAMPE = ["#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"];
const SANS_DONNEE = "#d9d8d4";
const CLASSES_LISTES = [
  { min: 0, max: 1, couleur: "#e4e3df", texte: "1 liste (pas de choix)" },
  { min: 2, max: 2, couleur: "#86b6ef", texte: "2 listes" },
  { min: 3, max: 3, couleur: "#2a78d6", texte: "3 listes" },
  { min: 4, max: 9999, couleur: "#104281", texte: "4 listes ou plus" },
];
const INDICATEURS = {
  pres2022: { titre: "Mélenchon - présidentielle 2022 (1er tour)", type: "pct", cle: "pres2022" },
  euro2024: { titre: "LFI (Manon Aubry) - européennes 2024", type: "pct", cle: "euro2024" },
  legi2024: { titre: "NFP – législatives 2024 (1er tour)", type: "pct", cle: "legi2024" },
  muni2026: { titre: "Nombre de listes - municipales 2026 (1er tour)", type: "listes" },
  inscrits2026: { titre: "Inscrits - municipales 2026 (1er tour)", type: "inscrits" },
};

const Z = DONNEES.zones;
const fmtNb = n => n == null ? "n.d." : n.toLocaleString("fr-FR");
const fmtPct = p => p == null ? "n.d." : p.toLocaleString("fr-FR", { minimumFractionDigits: 1, maximumFractionDigits: 1 }) + " %";

document.getElementById("titre").textContent = DONNEES.meta.titre;
document.title = DONNEES.meta.titre;
document.getElementById("note").innerHTML =
  "Source : ministère de l'Intérieur, « Données des élections agrégées » (data.gouv.fr), résultats par bureau de vote additionnés par commune ; " +
  "contours et rattachement des bureaux aux circonscriptions : INSEE / data.gouv.fr (2024). Pourcentages calculés sur les suffrages exprimés. " +
  "NFP = candidats de nuance « Union de la gauche » (UG). Le dégradé va du plus bas au plus haut de la circonscription pour le scrutin affiché. " +
  (DONNEES.meta.partielles.length
    ? "Communes coupées entre plusieurs circonscriptions : seuls leurs bureaux de la circonscription sont comptés et dessinés (" +
      DONNEES.meta.partielles.join(", ") + ") ; pour les municipales, leur nombre de listes est celui de toute la commune."
    : "");

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
  return d.muni2026.inscrits;
}
function texteValeur(ind, v) {
  if (v == null) return "n.d.";
  const t = INDICATEURS[ind].type;
  if (t === "pct") return fmtPct(v);
  if (t === "listes") return v + (v > 1 ? " listes" : " liste");
  return fmtNb(v);
}
function couleurPour(ind, v, min, max) {
  if (v == null) return SANS_DONNEE;
  if (INDICATEURS[ind].type === "listes") return CLASSES_LISTES.find(c => v >= c.min && v <= c.max).couleur;
  return couleurSequentielle(max > min ? (v - min) / (max - min) : 0.5);
}

const carte = L.map("carte", { zoomSnap: 0.25, scrollWheelZoom: false });
// Fond de carte IGN (Géoplateforme) : gratuit, sans clé, fonctionne en ouvrant le fichier depuis l'ordinateur
L.tileLayer("https://data.geopf.fr/wmts?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}", {
  attribution: '&copy; <a href="https://geoservices.ign.fr/">IGN – Plan IGN</a>', maxZoom: 18, opacity: 0.6,
}).addTo(carte);

let indicateur = "pres2022", zoneActive = null;
const couches = {};
const STYLE_BORD = { weight: DONNEES.meta.etiquettes ? 2 : 1, color: "#fcfcfb" };
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

function popup(z) {
  const d = Z[z];
  const l = [
    `<h3>${d.nom}</h3>`,
    `Mélenchon 2022 : <b>${fmtPct(d.pres2022.pct)}</b> (${fmtNb(d.pres2022.voix)} voix)<br>`,
    `LFI européennes 2024 : <b>${fmtPct(d.euro2024.pct)}</b> (${fmtNb(d.euro2024.voix)} voix)<br>`,
    `NFP législatives 2024 : <b>${fmtPct(d.legi2024.pct)}</b> (${fmtNb(d.legi2024.voix)} voix)<br>`,
    `Inscrits municipales 2026 : <b>${fmtNb(d.muni2026.inscrits)}</b><br>`,
    `Municipales 2026 : <b>${texteValeur("muni2026", d.muni2026.nb_listes)}</b>${d.partielle ? " (toute la commune)" : ""}`,
  ];
  if (d.muni2026.listes && d.muni2026.listes.length) l.push("<ul>" + d.muni2026.listes.map(x => `<li>${x}</li>`).join("") + "</ul>");
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
  document.querySelectorAll(".filtres button").forEach(b => b.setAttribute("aria-pressed", b.dataset.ind === ind));
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
    else c.bindTooltip(contenu, DONNEES.meta.etiquettes
      ? { permanent: true, direction: "center", className: "etiquette" }
      : { sticky: true, className: "etiquette" });
  });

  const T = DONNEES.total;
  let html = `<h2>${def.titre}</h2>`;
  if (def.type === "pct") {
    const t = T[def.cle];
    html += `<div class="total">${fmtPct(t.pct)}</div><p class="sub">sur toute la circonscription · ${fmtNb(t.voix)} voix sur ${fmtNb(t.exprimes)} exprimés</p>`;
  } else if (def.type === "listes") {
    html += `<div class="total">${T.muni2026.communes_plusieurs} communes sur ${T.muni2026.communes}</div><p class="sub">ont eu plus d'une liste</p>`;
  } else {
    html += `<div class="total">${fmtNb(T.muni2026.inscrits)}</div><p class="sub">inscrits sur toute la circonscription</p>`;
  }
  if (def.type === "listes") {
    html += `<div class="legende-cases">` + CLASSES_LISTES.map(c => `<div><span class="p" style="background:${c.couleur}"></span>${c.texte}</div>`).join("") + `</div>`;
  } else {
    html += `<div class="legende-barre" style="background:linear-gradient(90deg,${RAMPE.join(",")})"></div>
      <div class="legende-bornes"><span>${texteValeur(ind, min)}</span><span>${texteValeur(ind, max)}</span></div>`;
  }
  document.getElementById("resume").innerHTML = html;

  const colonne = def.type === "pct" ? "Score" : def.type === "listes" ? "Listes" : "Inscrits";
  document.getElementById("thead").innerHTML =
    `<tr><th>Commune</th><th class="n">${colonne}</th>${def.type === "pct" ? '<th class="n">Voix</th>' : ""}</tr>`;
  const lignes = zones.map(z => ({ z, v: valeur(ind, z) })).sort((a, b) => (b.v ?? -1) - (a.v ?? -1));
  document.getElementById("tbody").innerHTML = lignes.map(({ z, v }) =>
    `<tr class="ligne${z === zoneActive ? " actif" : ""}" data-zone="${z}" data-nom="${Z[z].nom.toLowerCase()}">
      <td><span class="pastille" style="background:${couleurPour(ind, v, min, max)}"></span>${Z[z].nom_court}</td>
      <td class="n">${texteValeur(ind, v)}</td>
      ${def.type === "pct" ? `<td class="n">${fmtNb(Z[z][def.cle].voix)}</td>` : ""}
    </tr>`).join("");
  document.querySelectorAll("tr.ligne").forEach(tr => tr.addEventListener("click", () => selectionner(tr.dataset.zone, true)));
  filtrer();
}

function filtrer() {
  const q = document.getElementById("recherche").value.trim().toLowerCase();
  document.querySelectorAll("tr.ligne").forEach(tr => { tr.style.display = !q || tr.dataset.nom.includes(q) ? "" : "none"; });
}
document.getElementById("recherche").addEventListener("input", filtrer);
document.querySelectorAll(".filtres button").forEach(b => b.addEventListener("click", () => afficher(b.dataset.ind)));
afficher("pres2022");
</script>
</body>
</html>
]---"

html <- sub("__DATA__", json, gabarit, fixed = TRUE)
html <- sub("__TITRE__", meta$titre, html, fixed = TRUE)

sortie <- sprintf("carte_circo_%s-%s.html", DEPARTEMENT, CIRCO)
writeLines(html, paste0("C:/Users/leoni/Documents/CoteDor/",sortie), useBytes = TRUE)
message("Carte écrite : ", normalizePath(sortie))
