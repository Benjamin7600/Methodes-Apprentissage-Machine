# ============================================================
# TPE MACHINE LEARNING
# 01 - PREPARATION ET VISUALISATION DES DONNEES
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library("here")
library("data.table")


# ============================================================
# 2. VERIFICATION DES DONNEES
# ============================================================

# Afficher les fichiers disponibles dans le dossier data
list.files(here::here("data"))


# ============================================================
# 3. CHEMINS DES DONNEES
# ============================================================

file_crsp <- here::here(
  "data",
  "CRSP_Monthly_Stocks_2000_2025.zip"
)

file_compustat <- here::here(
  "data",
  "Compustat_Fundamentals_Annual_2000_2025_Complete.zip"
)

file_sp500 <- here::here(
  "data",
  "SP500_Historical_Constituents_2000_2025.zip"
)


# ============================================================
# 4. VERIFICATION DU CONTENU DES FICHIERS ZIP
# ============================================================

# SUPPLEMENT
# Les données WRDS sont fournies sous forme de fichiers ZIP.
# On vérifie le contenu de chaque fichier avant l'importation.

unzip(file_crsp, list = TRUE)

unzip(file_compustat, list = TRUE)

unzip(file_sp500, list = TRUE)


# ============================================================
# 5. EXTRACTION DES FICHIERS ZIP
# ============================================================

# SUPPLEMENT
# Le cours utilise notamment load() avec des fichiers .rda.
# Nos données WRDS sont des fichiers CSV compressés en ZIP.
# Il faut donc d'abord les extraire.

unzip(
  file_crsp,
  exdir = here::here("data")
)

unzip(
  file_compustat,
  exdir = here::here("data")
)

unzip(
  file_sp500,
  exdir = here::here("data")
)


# Vérifier que les CSV ont bien été extraits
list.files(here::here("data"))


# ============================================================
# 6. IMPORTATION DE CRSP
# ============================================================

# SUPPLEMENT
# fread() est utilisé pour importer efficacement le gros
# fichier CSV issu de WRDS.

crsp <- fread(
  file = here::here("data", "adkkstnfk70e5cqb.csv")
)


# ============================================================
# 7. PREMIERS CONTROLES DE CRSP
# ============================================================

# Dimensions de la base
dim(crsp)

# Noms des variables
names(crsp)

# Premières observations
head(crsp)

# Vérifier que la base n'est pas vide
stopifnot(nrow(crsp) > 0)


# ============================================================
# 8. IMPORTATION DE COMPUSTAT
# ============================================================

# SUPPLEMENT
# Importation du fichier Compustat extrait du ZIP.

compustat <- fread(
  file = here::here("data", "qbj9jvhjabdxruhu.csv")
)


# ============================================================
# 9. PREMIERS CONTROLES DE COMPUSTAT
# ============================================================

# Dimensions
dim(compustat)

# Noms des variables
names(compustat)

# Premières observations
head(compustat)

# Vérifier que la base n'est pas vide
stopifnot(nrow(compustat) > 0)


# ============================================================
# 10. IMPORTATION DE L'HISTORIQUE S&P 500
# ============================================================

# SUPPLEMENT
# Importation du fichier historique des constituants du S&P 500.

sp500 <- fread(
  file = here::here("data", "tzevp6zuss1ko5u3.csv")
)


# ============================================================
# 11. PREMIERS CONTROLES DU S&P 500
# ============================================================

# Dimensions
dim(sp500)

# Noms des variables
names(sp500)

# Premières observations
head(sp500)

# Vérifier que la base n'est pas vide
stopifnot(nrow(sp500) > 0)


# ============================================================
# 12. REDUCTION DE L'HISTORIQUE S&P 500
# ============================================================

# SUPPLEMENT
# Le ticker peut changer au cours de la vie d'un titre.
# Pour définir l'appartenance au S&P 500, on utilise PERMNO
# et les dates d'entrée/sortie, et non le ticker.

sp500_members <- unique(
  sp500[, .(
    PERMNO,
    MbrStartDt,
    MbrEndDt
  )]
)


# Contrôles
dim(sp500_members)

head(sp500_members)

length(unique(sp500_members$PERMNO))

stopifnot(nrow(sp500_members) > 0)
# ============================================================
# 13. CONTROLES AVANT CONSTRUCTION DE L'UNIVERS S&P 500
# ============================================================

# Vérifier la période couverte par CRSP
range(crsp$MthCalDt)

# Nombre de titres CRSP
length(unique(crsp$PERMNO))

# Nombre de titres apparaissant dans l'historique S&P 500
length(unique(sp500_members$PERMNO))

# Vérifier les valeurs manquantes sur les identifiants principaux
sum(is.na(crsp$PERMNO))
sum(is.na(crsp$MthCalDt))

sum(is.na(sp500_members$PERMNO))
sum(is.na(sp500_members$MbrStartDt))
sum(is.na(sp500_members$MbrEndDt))

# Vérifier que les périodes d'appartenance sont cohérentes
stopifnot(
  all(sp500_members$MbrStartDt <= sp500_members$MbrEndDt)
)

# Vérifier les dates CRSP
stopifnot(
  !any(is.na(crsp$MthCalDt))
)

# ============================================================
# 14. CONSTRUCTION DE L'UNIVERS S&P 500 MENSUEL
# ============================================================

# SUPPLEMENT
# Certaines observations CRSP sont strictement dupliquées.
# On supprime d'abord les doublons exacts.
#
# IMPORTANT :
# L'univers d'investissement doit être défini à la date
# de formation du portefeuille.
#
# Pour chaque observation mensuelle CRSP, on utilise comme
# date de formation la date de l'observation CRSP MthCalDt.
#
# Un titre est donc considéré comme membre du S&P 500
# uniquement si sa période d'appartenance contient
# cette date de formation :
#
# MbrStartDt <= FormationDate <= MbrEndDt
#
# Cette règle évite de considérer comme disponible à la
# formation un titre qui entre dans l'indice seulement
# plus tard au cours du mois.


# ------------------------------------------------------------
# 14.1 SUPPRESSION DES DOUBLONS EXACTS CRSP
# ------------------------------------------------------------

crsp <- unique(crsp)


# Vérification des doublons PERMNO-date
duplicates_crsp <- crsp[
  ,
  .N,
  by = .(
    PERMNO,
    MthCalDt
  )
][
  N > 1
]

nrow(duplicates_crsp)


# ------------------------------------------------------------
# 14.2 DATE DE FORMATION
# ------------------------------------------------------------

# La date de formation correspond à la date mensuelle CRSP
# observée dans la base.

crsp[
  ,
  FormationDate := as.IDate(MthCalDt)
]


# ------------------------------------------------------------
# 14.3 FUSION AVEC L'HISTORIQUE DES CONSTITUANTS
# ------------------------------------------------------------

# On associe à chaque observation CRSP les différentes
# périodes historiques d'appartenance du PERMNO au S&P 500.

crsp_sp500 <- merge(
  crsp,
  sp500_members,
  by = "PERMNO",
  allow.cartesian = TRUE
)


# ------------------------------------------------------------
# 14.4 FILTRE D'APPARTENANCE A LA DATE DE FORMATION
# ------------------------------------------------------------

# Le titre doit déjà appartenir au S&P 500 à la date
# exacte utilisée pour former le portefeuille.

crsp_sp500 <- crsp_sp500[
  MbrStartDt <= FormationDate &
    MbrEndDt >= FormationDate
]


# ------------------------------------------------------------
# 14.5 TRAITEMENT DES DOUBLONS "LAST KNOWN"
# ------------------------------------------------------------

# Certains titres peuvent avoir deux observations le même mois :
# une observation normale et une observation administrative
# contenant "LAST KNOWN".
#
# On privilégie l'observation normale.

crsp_sp500[
  ,
  LastKnown := grepl(
    "LAST KNOWN",
    SecurityNm,
    fixed = TRUE
  )
]


# Trier afin de placer l'observation normale avant
# l'observation LAST KNOWN.

setorder(
  crsp_sp500,
  PERMNO,
  MthCalDt,
  LastKnown
)


# Garder une seule observation par PERMNO et par mois.

crsp_sp500 <- crsp_sp500[
  !duplicated(
    crsp_sp500,
    by = c(
      "PERMNO",
      "MthCalDt"
    )
  )
]


# Supprimer la variable temporaire.

crsp_sp500[
  ,
  LastKnown := NULL
]


# ------------------------------------------------------------
# 14.6 CONTROLES
# ------------------------------------------------------------

# Vérifier que chaque observation conservée respecte
# effectivement la période d'appartenance au S&P 500.

stopifnot(
  all(
    crsp_sp500$MbrStartDt <= crsp_sp500$FormationDate &
      crsp_sp500$MbrEndDt >= crsp_sp500$FormationDate
  )
)


# Vérifier qu'il ne reste qu'une observation
# par PERMNO et par date.

duplicates_sp500_final <- crsp_sp500[
  ,
  .N,
  by = .(
    PERMNO,
    MthCalDt
  )
][
  N > 1
]

nrow(duplicates_sp500_final)

stopifnot(
  nrow(duplicates_sp500_final) == 0
)

# ============================================================
# 15. CONTROLES DE L'UNIVERS S&P 500
# ============================================================

dim(crsp_sp500)

range(crsp_sp500$MthCalDt)

length(unique(crsp_sp500$PERMNO))


# Nombre de titres par mois
nb_stocks_month <- crsp_sp500[
  ,
  .N,
  by = MthCalDt
]

summary(nb_stocks_month$N)

head(nb_stocks_month)

tail(nb_stocks_month)


# Vérification finale des doublons action-mois
duplicates <- crsp_sp500[
  ,
  .N,
  by = .(PERMNO, MthCalDt)
][N > 1]

nrow(duplicates)


# Contrôles temporels
stopifnot(
  min(crsp_sp500$MthCalDt) >= as.IDate("2000-01-01")
)

stopifnot(
  max(crsp_sp500$MthCalDt) <= as.IDate("2025-12-31")
)


# ============================================================
# 17. PREPARATION DES DONNEES COMPUSTAT
# ============================================================

# SUPPLEMENT
# Compustat contient les informations comptables annuelles
# des entreprises.
#
# Ces variables serviront ensuite à construire les
# caractéristiques fondamentales utilisées par notre modèle.


# Vérifier la période disponible
range(compustat$datadate)


# Nombre d'entreprises différentes
length(unique(compustat$gvkey))


# Vérifier les formats
str(compustat[, .(
  gvkey,
  tic,
  cusip,
  datadate,
  fyear,
  at,
  ceq,
  che,
  dlc,
  dltt,
  ebitda,
  ni,
  sale,
  xrd,
  capx
)])


# ============================================================
# 18. SELECTION DES VARIABLES COMPUSTAT
# ============================================================

# SUPPLEMENT
# On conserve uniquement les variables nécessaires
# à la construction de nos caractéristiques fondamentales.

compustat_clean <- compustat[
  ,
  .(
    gvkey,
    tic,
    cusip,
    conm,
    datadate,
    fyear,
    gsector,
    
    # Bilan
    at,
    ceq,
    che,
    dlc,
    dltt,
    lt,
    
    # Résultats
    ebitda,
    ni,
    sale,
    
    # Investissement / innovation
    capx,
    xrd,
    
    # Actions / valeur de marché
    csho,
    mkvalt,
    prcc_f
  )
]


# ============================================================
# 19. PREMIERS CONTROLES COMPUSTAT
# ============================================================

dim(compustat_clean)

head(compustat_clean)

range(compustat_clean$datadate)

summary(compustat_clean$fyear)


# Nombre de valeurs manquantes par variable
sapply(
  compustat_clean,
  function(x) sum(is.na(x))
)


# ============================================================
# 20. PREPARATION DE LA CLE CRSP - COMPUSTAT
# ============================================================

# SUPPLEMENT
# CRSP contient un CUSIP sur 8 caractères tandis que
# Compustat contient un CUSIP sur 9 caractères.
#
# On crée donc une clé commune à partir des
# 8 premiers caractères du CUSIP Compustat.


# Clé CUSIP Compustat
compustat_clean[
  ,
  CUSIP8 := substr(cusip, 1, 8)
]


# Vérification
head(
  compustat_clean[
    ,
    .(
      gvkey,
      tic,
      cusip,
      CUSIP8,
      conm
    )
  ]
)


# ============================================================
# 21. VERIFICATION DU RACCORDEMENT AVEC CRSP
# ============================================================

# SUPPLEMENT
# Avant de fusionner les données financières,
# on vérifie combien de titres du S&P 500 peuvent être
# identifiés dans Compustat à partir du CUSIP.


# CUSIP distincts dans notre univers CRSP-S&P 500
crsp_cusip <- unique(
  crsp_sp500[
    !is.na(CUSIP) & CUSIP != "",
    .(
      PERMNO,
      CUSIP
    )
  ]
)


# CUSIP distincts dans Compustat
compustat_cusip <- unique(
  compustat_clean[
    !is.na(CUSIP8) & CUSIP8 != "",
    .(
      gvkey,
      CUSIP8
    )
  ]
)


# Nombre de PERMNO historiques dans notre univers
length(unique(crsp_cusip$PERMNO))


# Nombre de PERMNO ayant au moins une correspondance Compustat
matched_permno <- unique(
  crsp_cusip[
    compustat_cusip,
    on = .(CUSIP = CUSIP8),
    nomatch = 0
  ]$PERMNO
)

length(matched_permno)


# Pourcentage de couverture
100 * length(matched_permno) /
  length(unique(crsp_cusip$PERMNO))


# ============================================================
# 22. DIAGNOSTIC DES TITRES NON APPARIES
# ============================================================

# SUPPLEMENT
# Identifier les PERMNO du S&P 500 qui n'ont trouvé
# aucune correspondance dans Compustat avec le CUSIP.

unmatched_permno <- setdiff(
  unique(crsp_cusip$PERMNO),
  matched_permno
)

# Nombre de titres non appariés
length(unmatched_permno)


# Examiner quelques titres non appariés
unmatched_sample <- crsp_sp500[
  PERMNO %in% head(unmatched_permno, 20),
  .(
    PERMNO,
    Ticker,
    CUSIP,
    HdrCUSIP,
    SecurityNm,
    MthCalDt
  )
][
  order(PERMNO, MthCalDt)
][
  ,
  .SD[.N],
  by = PERMNO
]

unmatched_sample



# ============================================================
# 23. DEUXIEME RACCORDEMENT AVEC HdrCUSIP
# ============================================================

# SUPPLEMENT
# Certains titres ne sont pas appariés avec le CUSIP courant.
# On vérifie si le CUSIP historique principal (HdrCUSIP)
# permet de récupérer une partie des correspondances manquantes.


# Une ligne par PERMNO et HdrCUSIP
crsp_hdrcusip <- unique(
  crsp_sp500[
    !is.na(HdrCUSIP) & HdrCUSIP != "",
    .(
      PERMNO,
      HdrCUSIP
    )
  ]
)


# Recherche des correspondances Compustat
matched_hdr <- unique(
  crsp_hdrcusip[
    compustat_cusip,
    on = .(HdrCUSIP = CUSIP8),
    nomatch = 0
  ]$PERMNO
)


# Combiner les correspondances obtenues par CUSIP
# et celles obtenues par HdrCUSIP
matched_total <- union(
  matched_permno,
  matched_hdr
)


# ============================================================
# 23. DEUXIEME RACCORDEMENT AVEC HdrCUSIP
# ============================================================

# SUPPLEMENT
# Certains titres ne sont pas appariés avec le CUSIP courant.
# On vérifie si le CUSIP historique principal (HdrCUSIP)
# permet de récupérer une partie des correspondances manquantes.


# Une ligne par PERMNO et HdrCUSIP
crsp_hdrcusip <- unique(
  crsp_sp500[
    !is.na(HdrCUSIP) & HdrCUSIP != "",
    .(
      PERMNO,
      HdrCUSIP
    )
  ]
)


# Recherche des correspondances Compustat
matched_hdr <- unique(
  crsp_hdrcusip[
    compustat_cusip,
    on = .(HdrCUSIP = CUSIP8),
    nomatch = 0
  ]$PERMNO
)


# Combiner les correspondances obtenues par CUSIP
# et celles obtenues par HdrCUSIP
matched_total <- union(
  matched_permno,
  matched_hdr
)


# ============================================================
# 24. CONTROLES DU RACCORDEMENT
# ============================================================

# Correspondances obtenues avec CUSIP
length(matched_permno)

# Correspondances obtenues avec HdrCUSIP
length(matched_hdr)

# Nombre total de PERMNO récupérés
length(matched_total)

# Nouveaux PERMNO récupérés grâce à HdrCUSIP
length(setdiff(matched_hdr, matched_permno))


# Couverture sur les 1107 PERMNO de notre univers S&P 500
100 * length(matched_total) /
  length(unique(crsp_sp500$PERMNO))


# Titres toujours non appariés
unmatched_final <- setdiff(
  unique(crsp_sp500$PERMNO),
  matched_total
)

length(unmatched_final)


# ============================================================
# 25. CONSTRUCTION DU LIEN PERMNO - GVKEY
# ============================================================

# SUPPLEMENT
# PERMNO est l'identifiant permanent des titres dans CRSP.
# GVKEY est l'identifiant permanent des entreprises dans Compustat.
#
# On construit le lien entre ces deux identifiants en utilisant
# d'abord HdrCUSIP, qui donne ici la meilleure couverture.


permno_gvkey <- unique(
  crsp_hdrcusip[
    compustat_cusip,
    on = .(HdrCUSIP = CUSIP8),
    nomatch = 0,
    .(
      PERMNO,
      gvkey
    )
  ]
)


# ============================================================
# 26. CONTROLES DU LIEN PERMNO - GVKEY
# ============================================================

dim(permno_gvkey)

head(permno_gvkey)


# Nombre de PERMNO reliés
length(unique(permno_gvkey$PERMNO))


# Nombre de GVKEY reliés
length(unique(permno_gvkey$gvkey))


# Vérifier si un PERMNO correspond à plusieurs GVKEY
permno_multiple_gvkey <- permno_gvkey[
  ,
  .(
    N_GVKEY = uniqueN(gvkey)
  ),
  by = PERMNO
][
  N_GVKEY > 1
]

nrow(permno_multiple_gvkey)

head(permno_multiple_gvkey)


# Vérifier l'inverse :
# un GVKEY associé à plusieurs PERMNO
gvkey_multiple_permno <- permno_gvkey[
  ,
  .(
    N_PERMNO = uniqueN(PERMNO)
  ),
  by = gvkey
][
  N_PERMNO > 1
]

nrow(gvkey_multiple_permno)

head(gvkey_multiple_permno)


# ============================================================
# 27. DATE DE DISPONIBILITE DES DONNEES COMPUSTAT
# ============================================================

# SUPPLEMENT
# datadate correspond à la date de clôture de l'exercice
# comptable et non nécessairement à la date à laquelle
# l'information était disponible aux investisseurs.
#
# Afin d'éviter le look-ahead bias, on applique un délai
# prudent de 6 mois après la date de clôture.


# Ajouter le PERMNO aux données Compustat
compustat_linked <- merge(
  compustat_clean,
  permno_gvkey,
  by = "gvkey"
)


# Date à partir de laquelle les informations seront
# considérées comme disponibles
compustat_linked[
  ,
  AvailableDate :=
    as.IDate(
      seq(
        as.Date(datadate),
        by = "6 months",
        length.out = 2
      )[2]
    ),
  by = seq_len(nrow(compustat_linked))
]


# ============================================================
# 28. CONTROLES DES DONNEES COMPUSTAT RELIEES
# ============================================================

dim(compustat_linked)

length(unique(compustat_linked$PERMNO))

length(unique(compustat_linked$gvkey))

range(compustat_linked$datadate)

range(compustat_linked$AvailableDate)


head(
  compustat_linked[
    ,
    .(
      PERMNO,
      gvkey,
      tic,
      datadate,
      AvailableDate,
      fyear
    )
  ]
)


# ============================================================
# 29. FUSION TEMPORELLE CRSP - COMPUSTAT
# ============================================================

# SUPPLEMENT
# On conserve explicitement la date mensuelle CRSP dans
# une variable Date avant la jointure temporelle.

crsp_sp500[, Date := MthCalDt]


# Trier les données
setorder(
  compustat_linked,
  PERMNO,
  AvailableDate
)

setorder(
  crsp_sp500,
  PERMNO,
  Date
)


# Pour chaque PERMNO et chaque mois CRSP,
# récupérer la dernière information Compustat disponible.

data_ml <- compustat_linked[
  crsp_sp500,
  on = .(
    PERMNO,
    AvailableDate <= Date
  ),
  mult = "last"
]

# ============================================================
# 29. FUSION TEMPORELLE CRSP - COMPUSTAT
# ============================================================

# SUPPLEMENT
# Pour chaque observation mensuelle CRSP, on associe
# la dernière information Compustat disponible à cette date.
#
# On crée Date_CRSP afin de conserver explicitement
# la vraie date mensuelle après la jointure non-équi.


# Copie de la base CRSP-S&P 500
crsp_ml <- copy(crsp_sp500)

# Conserver explicitement la date mensuelle
crsp_ml[, Date_CRSP := MthCalDt]


# Trier les données
setorder(
  compustat_linked,
  PERMNO,
  AvailableDate
)

setorder(
  crsp_ml,
  PERMNO,
  Date_CRSP
)


# Jointure temporelle
data_ml <- compustat_linked[
  crsp_ml,
  on = .(
    PERMNO,
    AvailableDate <= Date_CRSP
  ),
  mult = "last",
  
  # On indique explicitement les variables à conserver
  .(
    PERMNO = i.PERMNO,
    
    # Données CRSP
    MthCalDt = i.MthCalDt,
    Ticker = i.Ticker,
    CUSIP = i.CUSIP,
    HdrCUSIP = i.HdrCUSIP,
    SecurityNm = i.SecurityNm,
    PrimaryExch = i.PrimaryExch,
    MthPrc = i.MthPrc,
    MthCap = i.MthCap,
    MthPrevPrc = i.MthPrevPrc,
    MthPrevCap = i.MthPrevCap,
    MthRet = i.MthRet,
    MthRetx = i.MthRetx,
    MthVol = i.MthVol,
    ShrOut = i.ShrOut,
    
    # Données Compustat
    gvkey = x.gvkey,
    tic = x.tic,
    conm = x.conm,
    datadate = x.datadate,
    fyear = x.fyear,
    gsector = x.gsector,
    
    at = x.at,
    ceq = x.ceq,
    che = x.che,
    dlc = x.dlc,
    dltt = x.dltt,
    lt = x.lt,
    ebitda = x.ebitda,
    ni = x.ni,
    sale = x.sale,
    capx = x.capx,
    xrd = x.xrd,
    csho = x.csho,
    mkvalt = x.mkvalt,
    prcc_f = x.prcc_f,
    
    # Vraie date de disponibilité Compustat
    CompustatDate = x.AvailableDate
  )
]


# ============================================================
# 30. CONTROLES DE LA FUSION
# ============================================================

dim(data_ml)

names(data_ml)

range(data_ml$MthCalDt)

length(unique(data_ml$PERMNO))


# Vérifier les doublons action-mois
duplicates_ml <- data_ml[
  ,
  .N,
  by = .(
    PERMNO,
    MthCalDt
  )
][N > 1]

nrow(duplicates_ml)


# ============================================================
# 31. CONTROLE DU LOOK-AHEAD BIAS
# ============================================================

lookahead_error <- data_ml[
  !is.na(CompustatDate) &
    CompustatDate > MthCalDt
]

nrow(lookahead_error)


# ============================================================
# 32. COUVERTURE COMPUSTAT
# ============================================================

sum(!is.na(data_ml$gvkey))

sum(is.na(data_ml$gvkey))

100 * mean(!is.na(data_ml$gvkey))


# ============================================================
# 33. APERCU
# ============================================================

head(
  data_ml[
    ,
    .(
      PERMNO,
      Ticker,
      MthCalDt,
      MthRet,
      MthCap,
      gvkey,
      tic,
      datadate,
      CompustatDate,
      at,
      ceq,
      ebitda,
      ni,
      sale
    )
  ],
  20
)


# ============================================================
# 34. CONSTRUCTION DES CARACTERISTIQUES FONDAMENTALES
# ============================================================

# SUPPLEMENT
# Les données comptables brutes dépendent fortement de la taille
# de l'entreprise. On construit donc des ratios comparables
# entre les différentes entreprises.


# ------------------------------------------------------------
# 34.1 RENTABILITE
# ------------------------------------------------------------

# ROE : résultat net / capitaux propres

data_ml[
  ,
  ROE := ni / ceq
]


# ROA : résultat net / actif total

data_ml[
  ,
  ROA := ni / at
]


# Marge EBITDA : EBITDA / ventes

data_ml[
  ,
  EBITDA_Margin := ebitda / sale
]


# ------------------------------------------------------------
# 34.2 LEVIER FINANCIER
# ------------------------------------------------------------

# Dette totale

data_ml[
  ,
  TotalDebt := dlc + dltt
]


# Dette / actif

data_ml[
  ,
  Leverage := TotalDebt / at
]


# ------------------------------------------------------------
# 34.3 LIQUIDITE
# ------------------------------------------------------------

# Cash / actif

data_ml[
  ,
  CashRatio := che / at
]


# ------------------------------------------------------------
# 34.4 INVESTISSEMENT
# ------------------------------------------------------------

# Dépenses d'investissement rapportées aux actifs

data_ml[
  ,
  CAPX_Assets := capx / at
]


# R&D rapportée aux ventes

data_ml[
  ,
  RD_Sales := xrd / sale
]


# ============================================================
# 35. CARACTERISTIQUES DE VALORISATION
# ============================================================

# SUPPLEMENT
# On utilise la capitalisation boursière CRSP pour construire
# des ratios reliant valeur comptable et valeur de marché.


# CRSP MthCap est exprimée en milliers de dollars.
# Compustat ceq est exprimé en millions de dollars.
# On remet donc MthCap en millions.

data_ml[
  ,
  MarketCap_M := MthCap / 1000
]


# Book-to-Market

data_ml[
  ,
  BookToMarket := ceq / MarketCap_M
]


# Earnings-to-Price

data_ml[
  ,
  EarningsToPrice := ni / MarketCap_M
]


# ============================================================
# 36. CONTROLE DES CARACTERISTIQUES
# ============================================================

features_fundamental <- c(
  "ROE",
  "ROA",
  "EBITDA_Margin",
  "Leverage",
  "CashRatio",
  "CAPX_Assets",
  "RD_Sales",
  "BookToMarket",
  "EarningsToPrice"
)


# Résumé statistique

summary(
  data_ml[
    ,
    ..features_fundamental
  ]
)


# Nombre de valeurs manquantes

sapply(
  data_ml[
    ,
    ..features_fundamental
  ],
  function(x) sum(is.na(x))
)


# Nombre de valeurs infinies

sapply(
  data_ml[
    ,
    ..features_fundamental
  ],
  function(x) sum(is.infinite(x))
)



# # ============================================================
# 37. CONSTRUCTION DES CARACTERISTIQUES DE MARCHE
# ============================================================

# SUPPLEMENT
# Les données CRSP permettent de construire des variables
# décrivant la dynamique récente du prix de chaque action.
#
# IMPORTANT :
# toutes les variables sont construites uniquement à partir
# d'informations disponibles à la date considérée.


# Trier correctement les observations

setorder(
  data_ml,
  PERMNO,
  MthCalDt
)


# ------------------------------------------------------------
# 37.1 MOMENTUM
# ------------------------------------------------------------

# Rendement cumulé des mois précédents en excluant
# le mois courant.
#
# Momentum 12-1 :
# on utilise les 11 rendements précédant le mois courant.

data_ml[
  ,
  Momentum_12_1 :=
    shift(
      frollapply(
        1 + MthRet,
        N = 11,
        FUN = prod,
        align = "right"
      ),
      1
    ) - 1,
  by = PERMNO
]


# Momentum court terme :
# rendement cumulé des 5 mois précédant le mois courant.

data_ml[
  ,
  Momentum_6_1 :=
    shift(
      frollapply(
        1 + MthRet,
        N = 5,
        FUN = prod,
        align = "right"
      ),
      1
    ) - 1,
  by = PERMNO
]


# ------------------------------------------------------------
# 37.2 RENDEMENT DU MOIS PRECEDENT
# ------------------------------------------------------------

# Rendement observé au mois t-1.
# Le shift permet d'éviter d'utiliser le rendement
# du mois courant comme caractéristique.

data_ml[
  ,
  Return_1M := shift(
    MthRet,
    1
  ),
  by = PERMNO
]


# ------------------------------------------------------------
# 37.3 VOLATILITE HISTORIQUE
# ------------------------------------------------------------

# Ecart-type des rendements mensuels sur les
# 12 mois précédents.
#
# Le mois courant est exclu grâce au shift.

data_ml[
  ,
  Volatility_12M :=
    shift(
      frollapply(
        MthRet,
        N = 12,
        FUN = sd,
        align = "right"
      ),
      1
    ),
  by = PERMNO
]


# ------------------------------------------------------------
# 37.4 TAILLE DE L'ENTREPRISE
# ------------------------------------------------------------

# Logarithme de la capitalisation boursière.
# La transformation logarithmique réduit l'asymétrie
# entre les petites et les très grandes entreprises.

data_ml[
  ,
  LogSize := log(MthCap)
]


# ============================================================
# 38. CONTROLE DES CARACTERISTIQUES DE MARCHE
# ============================================================

features_market <- c(
  "Momentum_12_1",
  "Momentum_6_1",
  "Return_1M",
  "Volatility_12M",
  "LogSize"
)


summary(
  data_ml[
    ,
    ..features_market
  ]
)


# Valeurs manquantes

sapply(
  data_ml[
    ,
    ..features_market
  ],
  function(x) sum(is.na(x))
)


# Valeurs infinies

sapply(
  data_ml[
    ,
    ..features_market
  ],
  function(x) sum(is.infinite(x))
)


# ============================================================
# 39. TRAITEMENT DES VALEURS EXTREMES
# ============================================================

# SUPPLEMENT
# Certaines caractéristiques présentent des valeurs extrêmes
# susceptibles d'avoir une influence excessive sur le modèle.
#
# On applique une winsorisation transversale à 1 % et 99 %
# pour chaque mois.
#
# Cette opération utilise uniquement les observations
# disponibles au même mois et ne crée donc pas de
# look-ahead bias.


# Liste des caractéristiques à traiter

features_winsor <- c(
  features_fundamental,
  features_market
)


# ------------------------------------------------------------
# 39.1 FONCTION DE WINSORISATION
# ------------------------------------------------------------

winsorize <- function(x, p = 0.01) {
  
  if (all(is.na(x))) {
    return(x)
  }
  
  q <- quantile(
    x,
    probs = c(p, 1 - p),
    na.rm = TRUE,
    names = FALSE
  )
  
  pmax(
    pmin(x, q[2]),
    q[1]
  )
}


# ------------------------------------------------------------
# 39.2 WINSORISATION PAR MOIS
# ------------------------------------------------------------

data_ml[
  ,
  (features_winsor) :=
    lapply(
      .SD,
      winsorize
    ),
  by = MthCalDt,
  .SDcols = features_winsor
]


# ============================================================
# 40. CONTROLES APRES WINSORISATION
# ============================================================

summary(
  data_ml[
    ,
    ..features_winsor
  ]
)


# Vérifier les valeurs infinies

sapply(
  data_ml[
    ,
    ..features_winsor
  ],
  function(x) sum(is.infinite(x))
)


# Vérifier les valeurs manquantes

sapply(
  data_ml[
    ,
    ..features_winsor
  ],
  function(x) sum(is.na(x))
)


# ============================================================
# 41. CONSTRUCTION DE LA VARIABLE CIBLE A PARTIR DU CRSP COMPLET
# ============================================================

# OBJECTIF :
#
# Pour chaque action appartenant à l'univers d'investissement
# au mois t, récupérer son rendement au mois calendrier t+1
# directement dans le CRSP complet.
#
# IMPORTANT :
# Le rendement futur ne doit pas être construit avec
# shift(MthRet, type = "lead") dans data_ml, car data_ml
# est déjà filtré selon l'appartenance au S&P 500.
#
# Une action présente dans l'univers à t doit conserver
# son rendement t+1 même si elle quitte ensuite le S&P 500.


# ------------------------------------------------------------
# 41.1 COPIE DU CRSP COMPLET
# ------------------------------------------------------------

crsp_returns <- copy(crsp)


# S'assurer que la date est au bon format

crsp_returns[
  ,
  MthCalDt := as.IDate(MthCalDt)
]


# ------------------------------------------------------------
# 41.2 IDENTIFIER ET TRAITER LES OBSERVATIONS "LAST KNOWN"
# ------------------------------------------------------------

# Le diagnostic a montré que certains PERMNO-mois possèdent
# deux lignes :
#
# 1. une observation normale ;
# 2. une observation administrative "LAST KNOWN".
#
# Les rendements sont identiques dans les cas diagnostiqués.
# On privilégie l'observation normale.


crsp_returns[
  ,
  LastKnown := grepl(
    "LAST KNOWN",
    SecurityNm,
    fixed = TRUE
  )
]


# Trier :
# observation normale d'abord,
# LAST KNOWN ensuite.

setorder(
  crsp_returns,
  PERMNO,
  MthCalDt,
  LastKnown
)


# ------------------------------------------------------------
# 41.3 CREER UNE CLE MENSUELLE
# ------------------------------------------------------------

crsp_returns[
  ,
  YearMonth_CRSP := format(
    MthCalDt,
    "%Y-%m"
  )
]


# ------------------------------------------------------------
# 41.4 CONSERVER UNE SEULE OBSERVATION PAR PERMNO-MOIS
# ------------------------------------------------------------

# Comme l'observation normale a été placée avant LAST KNOWN,
# duplicated() conserve l'observation normale.

crsp_returns <- crsp_returns[
  !duplicated(
    crsp_returns,
    by = c(
      "PERMNO",
      "YearMonth_CRSP"
    )
  )
]


# La variable temporaire n'est plus nécessaire.

crsp_returns[
  ,
  LastKnown := NULL
]


# ------------------------------------------------------------
# 41.5 VERIFIER L'UNICITE APRES NETTOYAGE
# ------------------------------------------------------------

duplicates_crsp_month <- crsp_returns[
  ,
  .N,
  by = .(
    PERMNO,
    YearMonth_CRSP
  )
][
  N > 1
]


cat(
  "\nDoublons PERMNO-mois après nettoyage :",
  nrow(duplicates_crsp_month),
  "\n"
)


stopifnot(
  nrow(duplicates_crsp_month) == 0
)


# ------------------------------------------------------------
# 41.6 CREER LE MOIS CIBLE t+1 DANS data_ml
# ------------------------------------------------------------

# MthCalDt correspond au mois de formation t.
#
# On transforme d'abord ce mois en premier jour du mois
# afin d'ajouter exactement un mois calendrier.

data_ml[
  ,
  FormationMonth := as.IDate(
    paste0(
      format(MthCalDt, "%Y-%m"),
      "-01"
    )
  )
]


# Fonction simple permettant d'obtenir le mois suivant
# sans dépendre du nombre de jours dans le mois.

next_month <- function(x) {
  
  year_x <- as.integer(
    format(x, "%Y")
  )
  
  month_x <- as.integer(
    format(x, "%m")
  )
  
  next_year <- ifelse(
    month_x == 12L,
    year_x + 1L,
    year_x
  )
  
  next_month_number <- ifelse(
    month_x == 12L,
    1L,
    month_x + 1L
  )
  
  sprintf(
    "%04d-%02d",
    next_year,
    next_month_number
  )
}


data_ml[
  ,
  TargetYearMonth := next_month(
    FormationMonth
  )
]


# ------------------------------------------------------------
# 41.7 CONSTRUIRE LA TABLE DES RENDEMENTS CRSP FUTURS
# ------------------------------------------------------------

future_returns <- crsp_returns[
  ,
  .(
    PERMNO,
    
    TargetYearMonth =
      YearMonth_CRSP,
    
    Date_NextObs =
      MthCalDt,
    
    Return_NextMonth =
      MthRet
  )
]


# Vérification importante :
# une seule ligne doit exister par PERMNO-mois.

future_return_duplicates <- future_returns[
  ,
  .N,
  by = .(
    PERMNO,
    TargetYearMonth
  )
][
  N > 1
]


cat(
  "Doublons dans future_returns :",
  nrow(future_return_duplicates),
  "\n"
)


stopifnot(
  nrow(future_return_duplicates) == 0
)


# ------------------------------------------------------------
# 41.8 SUPPRIMER LES ANCIENNES CIBLES SI ELLES EXISTENT
# ------------------------------------------------------------

# Cette précaution permet de réexécuter cette section
# sans conflit avec les anciennes variables.

old_target_variables <- intersect(
  c(
    "Return_NextMonth",
    "Date_NextObs",
    "MonthsToNext"
  ),
  names(data_ml)
)


if (
  length(old_target_variables) > 0
) {
  
  data_ml[
    ,
    (old_target_variables) := NULL
  ]
  
}


# ------------------------------------------------------------
# 41.9 JOINDRE LE RENDEMENT t+1 DEPUIS LE CRSP COMPLET
# ------------------------------------------------------------

data_ml[
  future_returns,
  on = .(
    PERMNO,
    TargetYearMonth
  ),
  `:=`(
    Date_NextObs =
      i.Date_NextObs,
    
    Return_NextMonth =
      i.Return_NextMonth
  )
]


# ============================================================
# 42. CONTROLES DE LA VARIABLE CIBLE
# ============================================================

# ------------------------------------------------------------
# 42.1 RESUME STATISTIQUE
# ------------------------------------------------------------

summary(
  data_ml$Return_NextMonth
)


# ------------------------------------------------------------
# 42.2 DISPONIBILITE
# ------------------------------------------------------------

cat(
  "\nNombre total d'observations :",
  nrow(data_ml),
  "\n"
)


cat(
  "Cibles disponibles :",
  sum(
    !is.na(data_ml$Return_NextMonth)
  ),
  "\n"
)


cat(
  "Cibles manquantes :",
  sum(
    is.na(data_ml$Return_NextMonth)
  ),
  "\n"
)


cat(
  "Pourcentage disponible :",
  round(
    100 *
      mean(
        !is.na(data_ml$Return_NextMonth)
      ),
    4
  ),
  "%\n"
)


# ------------------------------------------------------------
# 42.3 VALEURS INFINIES
# ------------------------------------------------------------

cat(
  "Valeurs infinies :",
  sum(
    is.infinite(
      data_ml$Return_NextMonth
    )
  ),
  "\n"
)


# ------------------------------------------------------------
# 42.4 CONTROLE VISUEL
# ------------------------------------------------------------

head(
  data_ml[
    ,
    .(
      PERMNO,
      Ticker,
      MthCalDt,
      MthRet,
      TargetYearMonth,
      Date_NextObs,
      Return_NextMonth
    )
  ],
  20
)


# ============================================================
# 43. CONTROLE TEMPOREL DE LA VARIABLE CIBLE
# ============================================================

# Toute cible disponible doit correspondre exactement
# au mois calendrier suivant.


# ------------------------------------------------------------
# 43.1 CALCUL DE L'ECART EN MOIS
# ------------------------------------------------------------

data_ml[
  !is.na(Date_NextObs),
  MonthsToNext :=
    12L *
    (
      as.integer(
        format(
          Date_NextObs,
          "%Y"
        )
      ) -
        as.integer(
          format(
            MthCalDt,
            "%Y"
          )
        )
    ) +
    (
      as.integer(
        format(
          Date_NextObs,
          "%m"
        )
      ) -
        as.integer(
          format(
            MthCalDt,
            "%m"
          )
        )
    )
]


# ------------------------------------------------------------
# 43.2 DISTRIBUTION DES ECARTS
# ------------------------------------------------------------

cat(
  "\nDistribution de MonthsToNext :\n"
)


print(
  table(
    data_ml$MonthsToNext,
    useNA = "ifany"
  )
)


# ------------------------------------------------------------
# 43.3 RECHERCHER LES ALIGNEMENTS INCORRECTS
# ------------------------------------------------------------

invalid_target_dates <- data_ml[
  !is.na(Return_NextMonth) &
    (
      is.na(MonthsToNext) |
        MonthsToNext != 1
    )
]


cat(
  "\nCibles avec mauvais alignement temporel :",
  nrow(invalid_target_dates),
  "\n"
)


stopifnot(
  nrow(invalid_target_dates) == 0
)


# ------------------------------------------------------------
# 43.4 VERIFICATION DIRECTE DES MOIS
# ------------------------------------------------------------

target_month_check <- data_ml[
  !is.na(Return_NextMonth),
  .(
    PERMNO,
    MthCalDt,
    
    TargetYearMonth,
    
    Date_NextObs,
    
    ObservedYearMonth =
      format(
        Date_NextObs,
        "%Y-%m"
      )
  )
]


target_month_errors <- target_month_check[
  TargetYearMonth != ObservedYearMonth
]


cat(
  "Erreurs TargetYearMonth :",
  nrow(target_month_errors),
  "\n"
)


stopifnot(
  nrow(target_month_errors) == 0
)


# ============================================================
# 44. CONTROLES FINAUX DE LA VARIABLE CIBLE
# ============================================================


# ------------------------------------------------------------
# 44.1 VERIFIER L'UNICITE DE data_ml
# ------------------------------------------------------------

target_duplicates <- data_ml[
  ,
  .N,
  by = .(
    PERMNO,
    MthCalDt
  )
][
  N > 1
]


cat(
  "\nDoublons PERMNO-date dans data_ml :",
  nrow(target_duplicates),
  "\n"
)


stopifnot(
  nrow(target_duplicates) == 0
)


# ------------------------------------------------------------
# 44.2 RESUME FINAL
# ------------------------------------------------------------

target_final_check <- data_ml[
  ,
  .(
    N_Observations =
      .N,
    
    N_Target_Available =
      sum(
        !is.na(Return_NextMonth)
      ),
    
    N_Target_Missing =
      sum(
        is.na(Return_NextMonth)
      ),
    
    Pct_Target_Available =
      100 *
      mean(
        !is.na(Return_NextMonth)
      ),
    
    N_Invalid_Time_Alignment =
      sum(
        !is.na(Return_NextMonth) &
          (
            is.na(MonthsToNext) |
              MonthsToNext != 1
          )
      )
  )
]


print(
  target_final_check
)


# ------------------------------------------------------------
# 44.3 EXEMPLES DES CIBLES FINALES
# ------------------------------------------------------------

head(
  data_ml[
    !is.na(Return_NextMonth),
    .(
      PERMNO,
      Ticker,
      MthCalDt,
      MthRet,
      TargetYearMonth,
      Date_NextObs,
      Return_NextMonth,
      MonthsToNext
    )
  ],
  30
)


# ------------------------------------------------------------
# 44.4 NETTOYAGE
# ------------------------------------------------------------

# FormationMonth était uniquement nécessaire pour construire
# la clé du mois suivant.
#
# TargetYearMonth, Date_NextObs et MonthsToNext sont conservés
# pour permettre les contrôles ultérieurs.

data_ml[
  ,
  FormationMonth := NULL
]


# ============================================================
# FIN DE LA CONSTRUCTION DE LA CIBLE
# ============================================================

# ============================================================
# 45. CONTROLE DE CONTINUITE DES CARACTERISTIQUES DE MARCHE
# ============================================================

# SUPPLEMENT
# Les caractéristiques de marché de la section 37 utilisent
# shift() et des fenêtres glissantes.
#
# Lorsqu'un titre quitte le S&P 500 puis y revient plus tard,
# l'observation précédente disponible pour ce PERMNO peut être
# séparée de plusieurs mois ou plusieurs années.
#
# On vérifie donc que les observations utilisées pour construire
# les caractéristiques de marché sont réellement consécutives.


# Trier les observations

setorder(
  data_ml,
  PERMNO,
  MthCalDt
)


# ------------------------------------------------------------
# 45.1 DATE DE L'OBSERVATION PRECEDENTE
# ------------------------------------------------------------

data_ml[
  ,
  Date_PrevObs := shift(
    MthCalDt,
    1
  ),
  by = PERMNO
]


# ------------------------------------------------------------
# 45.2 ECART EN MOIS AVEC L'OBSERVATION PRECEDENTE
# ------------------------------------------------------------

data_ml[
  !is.na(Date_PrevObs),
  MonthsFromPrev :=
    12L * (
      as.integer(format(MthCalDt, "%Y")) -
        as.integer(format(Date_PrevObs, "%Y"))
    ) +
    (
      as.integer(format(MthCalDt, "%m")) -
        as.integer(format(Date_PrevObs, "%m"))
    )
]


# ------------------------------------------------------------
# 45.3 DISTRIBUTION DES ECARTS
# ------------------------------------------------------------

table(
  data_ml$MonthsFromPrev,
  useNA = "ifany"
)


# Nombre d'observations précédées d'une interruption

data_ml[
  !is.na(MonthsFromPrev) &
    MonthsFromPrev != 1,
  .N
]


# Nombre de PERMNO concernés

data_ml[
  !is.na(MonthsFromPrev) &
    MonthsFromPrev != 1,
  uniqueN(PERMNO)
]


# ------------------------------------------------------------
# 45.4 EXAMINER LES CAS PROBLEMATIQUES
# ------------------------------------------------------------

head(
  data_ml[
    !is.na(MonthsFromPrev) &
      MonthsFromPrev != 1,
    .(
      PERMNO,
      Ticker,
      Date_PrevObs,
      MthCalDt,
      MonthsFromPrev,
      Return_1M,
      Momentum_6_1,
      Momentum_12_1,
      Volatility_12M
    )
  ][order(-MonthsFromPrev)],
  30
)


# ============================================================
# 46. CORRECTION DES CARACTERISTIQUES DE MARCHE
# ============================================================

# SUPPLEMENT
# Le contrôle précédent a identifié 28 interruptions dans
# les séries mensuelles de certains PERMNO.
#
# Les caractéristiques calculées avec shift() et frollapply()
# ne doivent jamais utiliser des observations situées avant
# une interruption.
#
# On découpe donc l'historique de chaque PERMNO en séquences
# de mois consécutifs ("Spell").


# Trier les observations

setorder(
  data_ml,
  PERMNO,
  MthCalDt
)


# ------------------------------------------------------------
# 46.1 IDENTIFIER LES SEQUENCES DE MOIS CONSECUTIFS
# ------------------------------------------------------------

data_ml[
  ,
  Spell := cumsum(
    is.na(MonthsFromPrev) |
      MonthsFromPrev != 1
  ),
  by = PERMNO
]


# ------------------------------------------------------------
# 46.2 RENDEMENT DU MOIS PRECEDENT
# ------------------------------------------------------------

data_ml[
  ,
  Return_1M := shift(
    MthRet,
    1
  ),
  by = .(
    PERMNO,
    Spell
  )
]


# ------------------------------------------------------------
# 46.3 MOMENTUM 12-1
# ------------------------------------------------------------

data_ml[
  ,
  Momentum_12_1 :=
    shift(
      frollapply(
        1 + MthRet,
        N = 11,
        FUN = prod,
        align = "right"
      ),
      1
    ) - 1,
  by = .(
    PERMNO,
    Spell
  )
]


# ------------------------------------------------------------
# 46.4 MOMENTUM 6-1
# ------------------------------------------------------------

data_ml[
  ,
  Momentum_6_1 :=
    shift(
      frollapply(
        1 + MthRet,
        N = 5,
        FUN = prod,
        align = "right"
      ),
      1
    ) - 1,
  by = .(
    PERMNO,
    Spell
  )
]


# ------------------------------------------------------------
# 46.5 VOLATILITE HISTORIQUE
# ------------------------------------------------------------

data_ml[
  ,
  Volatility_12M :=
    shift(
      frollapply(
        MthRet,
        N = 12,
        FUN = sd,
        align = "right"
      ),
      1
    ),
  by = .(
    PERMNO,
    Spell
  )
]


# ============================================================
# 47. CONTROLES APRES CORRECTION
# ============================================================

# La première observation de chaque séquence
# ne doit plus avoir de rendement précédent.

data_ml[
  is.na(MonthsFromPrev) |
    MonthsFromPrev != 1,
  .(
    N = .N,
    Return_1M_non_NA =
      sum(!is.na(Return_1M))
  )
]


# Nombre de valeurs manquantes après correction

sapply(
  data_ml[
    ,
    .(
      Return_1M,
      Momentum_6_1,
      Momentum_12_1,
      Volatility_12M
    )
  ],
  function(x) sum(is.na(x))
)


# Vérifier les 28 observations suivant une interruption

data_ml[
  !is.na(MonthsFromPrev) &
    MonthsFromPrev != 1,
  .(
    PERMNO,
    Ticker,
    Date_PrevObs,
    MthCalDt,
    MonthsFromPrev,
    Spell,
    Return_1M,
    Momentum_6_1,
    Momentum_12_1,
    Volatility_12M
  )
][order(-MonthsFromPrev)]


# ============================================================
# 48. NOUVELLE WINSORISATION DES CARACTERISTIQUES DE MARCHE
# ============================================================

# SUPPLEMENT
# Les caractéristiques de marché ont été recalculées après
# correction des interruptions temporelles.
#
# On applique donc de nouveau la winsorisation transversale
# à 1 % et 99 % pour chaque mois uniquement sur ces variables.


# ------------------------------------------------------------
# 48.1 VARIABLES DE MARCHE A TRAITER
# ------------------------------------------------------------

features_market <- c(
  "Momentum_12_1",
  "Momentum_6_1",
  "Return_1M",
  "Volatility_12M",
  "LogSize"
)


# ------------------------------------------------------------
# 48.2 WINSORISATION PAR MOIS
# ------------------------------------------------------------

data_ml[
  ,
  (features_market) :=
    lapply(
      .SD,
      winsorize
    ),
  by = MthCalDt,
  .SDcols = features_market
]


# ============================================================
# 49. CONTROLES APRES NOUVELLE WINSORISATION
# ============================================================

# Résumé statistique

summary(
  data_ml[
    ,
    ..features_market
  ]
)


# Valeurs manquantes

sapply(
  data_ml[
    ,
    ..features_market
  ],
  function(x) sum(is.na(x))
)


# Valeurs infinies

sapply(
  data_ml[
    ,
    ..features_market
  ],
  function(x) sum(is.infinite(x))
)


# ------------------------------------------------------------
# 49.1 VERIFICATION DES RUPTURES
# ------------------------------------------------------------

# La winsorisation ne doit évidemment pas transformer
# les NA créés lors des interruptions.

data_ml[
  !is.na(MonthsFromPrev) &
    MonthsFromPrev != 1,
  .(
    N = .N,
    Return_1M_non_NA =
      sum(!is.na(Return_1M)),
    Momentum_6_1_non_NA =
      sum(!is.na(Momentum_6_1)),
    Momentum_12_1_non_NA =
      sum(!is.na(Momentum_12_1)),
    Volatility_12M_non_NA =
      sum(!is.na(Volatility_12M))
  )
]


# ------------------------------------------------------------
# 49.2 CONTROLE FINAL DE LA CIBLE
# ------------------------------------------------------------

data_ml[
  !is.na(Return_NextMonth) &
    MonthsToNext != 1,
  .N
]

# ============================================================
# 49.3 IMPORTATION DES FACTEURS FAMA-FRENCH 5
# ============================================================

# Les facteurs Fama-French ne font PAS partie des 13
# caractéristiques utilisées dans la PCA.
#
# Ils serviront séparément à expliquer le rendement des actions
# et à construire le rendement résiduel non expliqué par FF5.

# ------------------------------------------------------------
# 49.3.1 LOCALISER LE FICHIER FAMA-FRENCH
# ------------------------------------------------------------

ff_files <- list.files(
  path = "data",
  pattern = "Fama|French|FF5|5_Factors",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

ff_files

# ------------------------------------------------------------
# 49.3.2 IMPORTATION DU FICHIER FAMA-FRENCH
# ------------------------------------------------------------

ff_path <- "data/Fama_French_5_Factors_Monthly_2000_2025.csv"

ff_raw <- fread(
  ff_path
)

# Vérification de la structure
names(ff_raw)

head(ff_raw)

str(ff_raw)

# ============================================================
# 49.4 PREPARATION DES FACTEURS FAMA-FRENCH 5
# ============================================================

# Copie de travail
ff5 <- copy(ff_raw)


# ------------------------------------------------------------
# 49.4.1 RENOMMER LE FACTEUR DE MARCHE
# ------------------------------------------------------------

setnames(
  ff5,
  old = "Mkt-RF",
  new = "MKT_RF"
)


# ------------------------------------------------------------
# 49.4.2 CONSTRUCTION DE LA DATE
# ------------------------------------------------------------

# month est sous la forme "2000-01".
# On crée directement le dernier jour du mois.

ff5[
  ,
  MthCalDt :=
    as.Date(
      paste0(
        month,
        "-01"
      )
    )
]

# Passer du premier au dernier jour du mois
ff5[
  ,
  MthCalDt :=
    seq(
      MthCalDt[1],
      by = "month",
      length.out = .N + 1
    )[2:(.N + 1)] - 1
]


# ------------------------------------------------------------
# 49.4.3 CONVERSION POURCENTAGES -> DECIMALES
# ------------------------------------------------------------

ff_cols <- c(
  "MKT_RF",
  "SMB",
  "HML",
  "RMW",
  "CMA",
  "RF"
)

ff5[
  ,
  (ff_cols) :=
    lapply(
      .SD,
      function(x) x / 100
    ),
  .SDcols = ff_cols
]


# ------------------------------------------------------------
# 49.4.4 CONSERVER LES COLONNES NECESSAIRES
# ------------------------------------------------------------

ff5 <- ff5[
  ,
  .(
    MthCalDt,
    MKT_RF,
    SMB,
    HML,
    RMW,
    CMA,
    RF
  )
]


# ------------------------------------------------------------
# 49.4.5 CONTROLES
# ------------------------------------------------------------

setorder(
  ff5,
  MthCalDt
)

head(ff5)

tail(ff5)

range(
  ff5$MthCalDt
)

stopifnot(
  !anyDuplicated(
    ff5$MthCalDt
  )
)

stopifnot(
  !anyNA(ff5)
)

# ============================================================
# 49.5 CONTROLES DES DONNEES FAMA-FRENCH
# ============================================================

head(ff5)

tail(ff5)

summary(
  ff5[
    ,
    ..ff_cols
  ]
)

range(
  ff5$MthCalDt
)

stopifnot(
  !anyDuplicated(
    ff5$MthCalDt
  )
)

stopifnot(
  !anyNA(
    ff5
  )
)


# ============================================================
# 49.6 CONTROLE DE LA CLE DE FUSION DANS data_ml
# ============================================================

"MthCalDt" %in% names(data_ml)

class(data_ml$MthCalDt)

range(
  data_ml$MthCalDt,
  na.rm = TRUE
)

head(
  data_ml[
    ,
    .(
      PERMNO,
      MthCalDt
    )
  ]
)

# ============================================================
# 49.7 PREPARATION DE LA CLE MENSUELLE FAMA-FRENCH
# ============================================================

# CRSP peut utiliser le dernier jour de NEGOCIATION du mois
# alors que Fama-French est daté au dernier jour CALENDAIRE.
#
# Exemple :
# CRSP        : 2000-04-28
# Fama-French : 2000-04-30
#
# Il s'agit économiquement du même mois.
# La fusion doit donc être effectuée sur YYYY-MM.


# ------------------------------------------------------------
# 49.7.1 CLE MENSUELLE DANS data_ml
# ------------------------------------------------------------

data_ml[
  ,
  YearMonth :=
    format(
      MthCalDt,
      "%Y-%m"
    )
]


# ------------------------------------------------------------
# 49.7.2 CLE MENSUELLE DANS ff5
# ------------------------------------------------------------

ff5[
  ,
  YearMonth :=
    format(
      MthCalDt,
      "%Y-%m"
    )
]


# ============================================================
# 49.8 PREPARATION DE LA TABLE FF5 CONTEMPORAINE
# ============================================================

ff5_current <- ff5[
  ,
  .(
    YearMonth,
    MKT_RF,
    SMB,
    HML,
    RMW,
    CMA,
    RF
  )
]


# Une seule observation FF par mois

stopifnot(
  !anyDuplicated(
    ff5_current$YearMonth
  )
)


# ============================================================
# 49.9 FUSION DES FACTEURS FF5 DU MOIS t
# ============================================================

# Supprimer d'éventuelles colonnes FF provenant
# d'une exécution antérieure de cette section.

ff_existing <- grep(
  "^(MKT_RF|SMB|HML|RMW|CMA|RF)(\\.x|\\.y)?$",
  names(data_ml),
  value = TRUE
)

if (length(ff_existing) > 0) {
  
  data_ml[
    ,
    (ff_existing) := NULL
  ]
}


# Fusion sur le MOIS et non sur le jour exact

data_ml <- merge(
  data_ml,
  ff5_current,
  by = "YearMonth",
  all.x = TRUE,
  sort = FALSE
)


# Remettre l'ordre du panel

setorder(
  data_ml,
  PERMNO,
  MthCalDt
)


# ============================================================
# 49.10 CONTROLE DE LA FUSION FF5 DU MOIS t
# ============================================================

ff_cols <- c(
  "MKT_RF",
  "SMB",
  "HML",
  "RMW",
  "CMA",
  "RF"
)


stopifnot(
  all(
    ff_cols %in%
      names(data_ml)
  )
)


ff_merge_check <- data_ml[
  ,
  .(
    N = .N,
    
    Missing_MKT_RF =
      sum(is.na(MKT_RF)),
    
    Missing_SMB =
      sum(is.na(SMB)),
    
    Missing_HML =
      sum(is.na(HML)),
    
    Missing_RMW =
      sum(is.na(RMW)),
    
    Missing_CMA =
      sum(is.na(CMA)),
    
    Missing_RF =
      sum(is.na(RF))
  )
]


ff_merge_check


# ============================================================
# 49.11 CONSTRUCTION DES FACTEURS FF5 DU MOIS t+1
# ============================================================

# Return_NextMonth représente :
#
# R_(i,t+1)
#
# Il faut donc lui associer les facteurs Fama-French
# observés pendant CE MEME mois t+1.
#
# On construit une table permettant d'associer :
#
# observation de janvier 2020
#          ↓
# facteurs FF de février 2020


ff5_next <- copy(ff5)


# ------------------------------------------------------------
# 49.11.1 CREATION DU MOIS DE RATTACHEMENT
# ------------------------------------------------------------

# Pour chaque mois FF t+1, on construit le mois t.
#
# Exemple :
#
# FF février 2020
#       ↓
# Match_YearMonth = janvier 2020


ff5_next[
  ,
  FF_Month_Start :=
    as.Date(
      paste0(
        YearMonth,
        "-01"
      )
    )
]


ff5_next[
  ,
  Match_YearMonth :=
    format(
      FF_Month_Start - 1,
      "%Y-%m"
    )
]


# ------------------------------------------------------------
# 49.11.2 TABLE FINALE FF5 t+1
# ------------------------------------------------------------

ff5_next <- ff5_next[
  ,
  .(
    YearMonth =
      Match_YearMonth,
    
    MKT_RF_NextMonth =
      MKT_RF,
    
    SMB_NextMonth =
      SMB,
    
    HML_NextMonth =
      HML,
    
    RMW_NextMonth =
      RMW,
    
    CMA_NextMonth =
      CMA,
    
    RF_NextMonth =
      RF
  )
]


# Une seule observation par mois

stopifnot(
  !anyDuplicated(
    ff5_next$YearMonth
  )
)


# ============================================================
# 49.12 FUSION DES FACTEURS FF5 DU MOIS t+1
# ============================================================

# Supprimer d'éventuelles colonnes t+1 provenant
# d'une exécution antérieure.

ff_next_existing <- grep(
  "_NextMonth(\\.x|\\.y)?$",
  names(data_ml),
  value = TRUE
)

# Attention :
# Return_NextMonth doit être conservé.

ff_next_existing <- setdiff(
  ff_next_existing,
  "Return_NextMonth"
)


if (length(ff_next_existing) > 0) {
  
  data_ml[
    ,
    (ff_next_existing) := NULL
  ]
}


# Fusion

data_ml <- merge(
  data_ml,
  ff5_next,
  by = "YearMonth",
  all.x = TRUE,
  sort = FALSE
)


# Remettre le panel dans son ordre

setorder(
  data_ml,
  PERMNO,
  MthCalDt
)


# ============================================================
# 49.13 CONTROLE DES FACTEURS FF5 t+1
# ============================================================

ff_next_cols <- c(
  "MKT_RF_NextMonth",
  "SMB_NextMonth",
  "HML_NextMonth",
  "RMW_NextMonth",
  "CMA_NextMonth",
  "RF_NextMonth"
)


stopifnot(
  all(
    ff_next_cols %in%
      names(data_ml)
  )
)


ff_next_check <- data_ml[
  ,
  .(
    N = .N,
    
    Missing_Return_NextMonth =
      sum(is.na(Return_NextMonth)),
    
    Missing_MKT_RF_NextMonth =
      sum(is.na(MKT_RF_NextMonth)),
    
    Missing_SMB_NextMonth =
      sum(is.na(SMB_NextMonth)),
    
    Missing_HML_NextMonth =
      sum(is.na(HML_NextMonth)),
    
    Missing_RMW_NextMonth =
      sum(is.na(RMW_NextMonth)),
    
    Missing_CMA_NextMonth =
      sum(is.na(CMA_NextMonth)),
    
    Missing_RF_NextMonth =
      sum(is.na(RF_NextMonth))
  )
]


ff_next_check


# ============================================================
# 49.14 CONTROLE VISUEL DE L'ALIGNEMENT t ET t+1
# ============================================================

# On vérifie que :
#
# ligne janvier t
# Return_NextMonth = rendement de février
# FF_NextMonth     = facteurs de février


data_ml[
  !is.na(Return_NextMonth) &
    !is.na(MKT_RF_NextMonth),
  .(
    PERMNO,
    MthCalDt,
    YearMonth,
    
    Return_NextMonth,
    
    MKT_RF,
    MKT_RF_NextMonth,
    
    RF,
    RF_NextMonth
  )
][1:20]


# ============================================================
# 49.15 CONSTRUCTION DU RENDEMENT EXCEDENTAIRE FUTUR
# ============================================================

# Modèle Fama-French 5 :
#
# R_(i,t+1) - RF_(t+1)
#
# =
#
# alpha
# + beta_MKT * MKT_RF_(t+1)
# + beta_SMB * SMB_(t+1)
# + beta_HML * HML_(t+1)
# + beta_RMW * RMW_(t+1)
# + beta_CMA * CMA_(t+1)
# + epsilon_(i,t+1)


data_ml[
  ,
  Excess_Return_NextMonth :=
    Return_NextMonth -
    RF_NextMonth
]


# ============================================================
# 49.16 CONTROLES DU RENDEMENT EXCEDENTAIRE
# ============================================================

summary(
  data_ml$Excess_Return_NextMonth
)


data_ml[
  !is.na(Excess_Return_NextMonth),
  .(
    PERMNO,
    MthCalDt,
    Return_NextMonth,
    RF_NextMonth,
    Excess_Return_NextMonth
  )
][1:20]


# ============================================================
# 49.17 CONTROLE FINAL FAMA-FRENCH
# ============================================================

ff_final_check <- data_ml[
  ,
  .(
    N = .N,
    
    Complete_FF_Current =
      sum(
        complete.cases(
          MKT_RF,
          SMB,
          HML,
          RMW,
          CMA,
          RF
        )
      ),
    
    Complete_FF_Next =
      sum(
        complete.cases(
          MKT_RF_NextMonth,
          SMB_NextMonth,
          HML_NextMonth,
          RMW_NextMonth,
          CMA_NextMonth,
          RF_NextMonth
        )
      ),
    
    Complete_Return_NextMonth =
      sum(
        !is.na(
          Return_NextMonth
        )
      ),
    
    Complete_Excess_Return =
      sum(
        !is.na(
          Excess_Return_NextMonth
        )
      )
  )
]


ff_final_check


# ============================================================
# 49.18 CONTROLE DE COHERENCE
# ============================================================

# Le rendement excédentaire ne peut être disponible que
# lorsque Return_NextMonth et RF_NextMonth sont disponibles.

stopifnot(
  all(
    is.na(data_ml$Excess_Return_NextMonth) |
      (
        !is.na(data_ml$Return_NextMonth) &
          !is.na(data_ml$RF_NextMonth)
      )
  )
)


# ------------------------------------------------------------
# Fin de l'intégration des données Fama-French
# ------------------------------------------------------------

# ============================================================
# 50. DIAGNOSTIC DES VALEURS MANQUANTES AVANT MODELISATION
# ============================================================

# SUPPLEMENT
# Avant de construire l'échantillon final destiné au modèle,
# on mesure précisément la disponibilité des différentes
# caractéristiques.
#
# Aucune observation n'est supprimée à cette étape.


# ------------------------------------------------------------
# 50.1 LISTE COMPLETE DES CARACTERISTIQUES
# ------------------------------------------------------------

features_all <- c(
  features_fundamental,
  features_market
)


# ------------------------------------------------------------
# 50.2 NOMBRE ET POURCENTAGE DE VALEURS MANQUANTES
# ------------------------------------------------------------

missing_features <- data.table(
  Feature = features_all,
  N_missing = sapply(
    data_ml[
      ,
      ..features_all
    ],
    function(x) sum(is.na(x))
  )
)

missing_features[
  ,
  Pct_missing :=
    100 * N_missing / nrow(data_ml)
]

missing_features[
  order(-Pct_missing)
]


# ------------------------------------------------------------
# 50.3 OBSERVATIONS COMPLETES
# ------------------------------------------------------------

# Nombre d'observations pour lesquelles toutes les
# caractéristiques sont disponibles.

complete_features <- complete.cases(
  data_ml[
    ,
    ..features_all
  ]
)

sum(complete_features)

100 * mean(complete_features)


# ------------------------------------------------------------
# 50.4 OBSERVATIONS COMPLETES AVEC CIBLE DISPONIBLE
# ------------------------------------------------------------

complete_ml <- complete_features &
  !is.na(data_ml$Return_NextMonth)

sum(complete_ml)

100 * mean(complete_ml)


# ------------------------------------------------------------
# 50.5 IMPACT DE RD_Sales
# ------------------------------------------------------------

# On vérifie combien d'observations seraient utilisables
# si RD_Sales n'était pas exigée.

features_without_rd <- setdiff(
  features_all,
  "RD_Sales"
)

complete_without_rd <- complete.cases(
  data_ml[
    ,
    ..features_without_rd
  ]
) &
  !is.na(data_ml$Return_NextMonth)

sum(complete_without_rd)

100 * mean(complete_without_rd)


# ------------------------------------------------------------
# 50.6 COUVERTURE PAR ANNEE
# ------------------------------------------------------------

data_ml[
  ,
  .(
    N = .N,
    N_target = sum(!is.na(Return_NextMonth)),
    N_complete_all = sum(
      complete.cases(.SD) &
        !is.na(Return_NextMonth)
    )
  ),
  by = .(
    Year = as.integer(format(MthCalDt, "%Y"))
  ),
  .SDcols = features_all
][
  ,
  Pct_complete_all :=
    100 * N_complete_all / N
][]


# ============================================================
# 51. DIAGNOSTIC DES VALEURS MANQUANTES DE RD_Sales
# ============================================================

# SUPPLEMENT
# RD_Sales est la caractéristique présentant le plus grand
# nombre de valeurs manquantes.
#
# Avant de décider de l'exclure ou de traiter les valeurs
# manquantes, on étudie leur répartition dans le temps
# et entre les différents secteurs.


# ------------------------------------------------------------
# 51.1 DISPONIBILITE DE RD_Sales PAR ANNEE
# ------------------------------------------------------------

rd_by_year <- data_ml[
  ,
  .(
    N = .N,
    N_RD_missing = sum(is.na(RD_Sales)),
    N_RD_available = sum(!is.na(RD_Sales))
  ),
  by = .(
    Year = as.integer(format(MthCalDt, "%Y"))
  )
]

rd_by_year[
  ,
  Pct_RD_missing :=
    100 * N_RD_missing / N
]

rd_by_year[]


# ------------------------------------------------------------
# 51.2 DISPONIBILITE DE RD_Sales PAR SECTEUR
# ------------------------------------------------------------

rd_by_sector <- data_ml[
  !is.na(gsector),
  .(
    N = .N,
    N_RD_missing = sum(is.na(RD_Sales)),
    N_RD_available = sum(!is.na(RD_Sales))
  ),
  by = gsector
]

rd_by_sector[
  ,
  Pct_RD_missing :=
    100 * N_RD_missing / N
]

rd_by_sector[
  order(-Pct_RD_missing)
]


# ------------------------------------------------------------
# 51.3 VERIFIER DIRECTEMENT LA VARIABLE xrd
# ------------------------------------------------------------

# RD_Sales = xrd / sale.
# On vérifie donc si les valeurs manquantes proviennent
# principalement de xrd.

data_ml[
  ,
  .(
    N = .N,
    XRD_missing = sum(is.na(xrd)),
    Sales_missing = sum(is.na(sale)),
    RD_missing = sum(is.na(RD_Sales))
  )
]


# ------------------------------------------------------------
# 51.4 OBSERVATIONS AVEC xrd EGAL A ZERO
# ------------------------------------------------------------

data_ml[
  !is.na(xrd),
  .(
    N_XRD_observed = .N,
    N_XRD_zero = sum(xrd == 0),
    Pct_XRD_zero = 100 * mean(xrd == 0)
  )
]




# ============================================================
# 52. SELECTION FINALE DES CARACTERISTIQUES DU MODELE
# ============================================================

# SUPPLEMENT
# Le diagnostic précédent montre que RD_Sales présente
# environ 50 % de valeurs manquantes.
#
# De plus, cette indisponibilité est fortement concentrée
# dans certains secteurs.
#
# Exiger RD_Sales réduirait fortement l'échantillon et
# modifierait la représentation sectorielle de l'univers.
#
# RD_Sales est donc conservée dans la base data_ml,
# mais exclue des caractéristiques du modèle principal.


# ------------------------------------------------------------
# 52.1 CARACTERISTIQUES FONDAMENTALES RETENUES
# ------------------------------------------------------------

features_fundamental_final <- c(
  "ROE",
  "ROA",
  "EBITDA_Margin",
  "Leverage",
  "CashRatio",
  "CAPX_Assets",
  "BookToMarket",
  "EarningsToPrice"
)


# ------------------------------------------------------------
# 52.2 CARACTERISTIQUES DE MARCHE RETENUES
# ------------------------------------------------------------

features_market_final <- c(
  "Momentum_12_1",
  "Momentum_6_1",
  "Return_1M",
  "Volatility_12M",
  "LogSize"
)


# ------------------------------------------------------------
# 52.3 LISTE FINALE DES CARACTERISTIQUES
# ------------------------------------------------------------

features_final <- c(
  features_fundamental_final,
  features_market_final
)

features_final

length(features_final)




# ============================================================
# 53. CONTROLE DE L'ECHANTILLON AVEC LES VARIABLES RETENUES
# ============================================================

# Observation utilisable si :
# 1. toutes les caractéristiques finales sont disponibles ;
# 2. le rendement du mois suivant est disponible.

usable_ml <- complete.cases(
  data_ml[
    ,
    ..features_final
  ]
) &
  !is.na(data_ml$Return_NextMonth)


# ============================================================
# DIAGNOSTIC FINAL 1
# UNIVERS DISPONIBLE A t VS CIBLE FUTURE DISPONIBLE
# ============================================================

features_available_t <- complete.cases(
  data_ml[, ..features_final]
)

diag_future_target <- data_ml[
  features_available_t == TRUE,
  .(
    N_Actions_t = .N,
    N_Target_Available = sum(!is.na(Return_NextMonth)),
    N_Target_Missing = sum(is.na(Return_NextMonth)),
    Pct_Target_Missing = 100 * mean(is.na(Return_NextMonth))
  )
]

diag_future_target[]


# Répartition des cibles futures manquantes par année

data_ml[
  features_available_t == TRUE &
    is.na(Return_NextMonth),
  .(
    N = .N
  ),
  by = .(
    Year = as.integer(
      format(MthCalDt, "%Y")
    )
  )
][order(Year)]


# ----# Impact spécifique sur la période test

data_ml[
  features_available_t == TRUE &
    MthCalDt >= as.IDate("2022-01-01"),
  .(
    N_Actions_t = .N,
    N_Target_Available =
      sum(!is.na(Return_NextMonth)),
    N_Target_Missing =
      sum(is.na(Return_NextMonth)),
    Pct_Target_Missing =
      100 * mean(is.na(Return_NextMonth))
  )
]



# Voir les observations test concernées

data_ml[
  features_available_t == TRUE &
    is.na(Return_NextMonth) &
    MthCalDt >= as.IDate("2022-01-01"),
  .(
    PERMNO,
    Ticker,
    MthCalDt,
    Return_NextMonth
  )
][order(MthCalDt)]


# ============================================================
# DIAGNOSTIC FINAL 2
# VALEURS NON FINIES DANS LES 13 CARACTERISTIQUES
# ============================================================

finite_check <- data.table(
  
  Variable = features_final,
  
  N_NA = sapply(
    data_ml[, ..features_final],
    function(x) sum(is.na(x))
  ),
  
  N_Inf = sapply(
    data_ml[, ..features_final],
    function(x) sum(is.infinite(x))
  ),
  
  N_NonFinite = sapply(
    data_ml[, ..features_final],
    function(x) sum(!is.finite(x))
  )
)

finite_check[]

finite_check_model <- data.table(
  
  Variable = features_final,
  
  N_Inf = sapply(
    data_model[, ..features_final],
    function(x) sum(is.infinite(x))
  ),
  
  N_NonFinite = sapply(
    data_model[, ..features_final],
    function(x) sum(!is.finite(x))
  )
)

finite_check_model[]


# ============================================================
# DIAGNOSTIC FINAL 3
# FRONTIERE DEVELOPPEMENT / TEST
# ============================================================

data_ml[
  MthCalDt >= as.IDate("2021-11-01") &
    MthCalDt <= as.IDate("2022-02-28"),
  .(
    N = .N,
    N_Target_Available =
      sum(!is.na(Return_NextMonth))
  ),
  by = .(
    Month = format(MthCalDt, "%Y-%m")
  )
][order(Month)]


data_ml[
  format(MthCalDt, "%Y-%m") == "2021-12" &
    features_available_t == TRUE,
  .(
    N_Actions = .N,
    N_Target_Available =
      sum(!is.na(Return_NextMonth)),
    N_Target_Missing =
      sum(is.na(Return_NextMonth))
  )
]

dev_data
test_data
lasso
cv.glmnet
glmnet
lambda



# ============================================================
# DIAGNOSTIC 3B
# DERNIER MOIS DU DEVELOPPEMENT
# ============================================================

max(lasso_development$MthCalDt)

lasso_development[
  format(MthCalDt, "%Y-%m") == "2021-12",
  .(
    N = .N,
    Mean_Target =
      mean(
        FF5_Residual_NextMonth,
        na.rm = TRUE
      )
  )
]

ls(
  pattern =
    "lasso|final|train|lambda"
)


# ============================================================
# DIAGNOSTIC FINAL 3C
# INSPECTION DES OBJETS DU REFIT FINAL
# ============================================================

cat("\n--- lasso_final ---\n")
print(lasso_final)

cat("\n--- fit_lasso ---\n")
print(fit_lasso)

cat("\n--- best_lambda ---\n")
print(best_lambda)

cat("\n--- Dimensions lasso_development ---\n")
print(dim(lasso_development))

cat("\n--- Dernier mois development ---\n")
print(max(lasso_development$MthCalDt))

cat("\n--- Nombre d'observations Decembre 2021 ---\n")
print(
  nrow(
    lasso_development[
      format(MthCalDt, "%Y-%m") == "2021-12"
    ]
  )
)



#--------------------------------------------------------
# 53.1 NOMBRE D'OBSERVATIONS UTILISABLES
# ------------------------------------------------------------

sum(usable_ml)

100 * mean(usable_ml)


# ------------------------------------------------------------
# 53.2 NOMBRE DE TITRES REPRESENTES
# ------------------------------------------------------------

uniqueN(
  data_ml[
    usable_ml,
    PERMNO
  ]
)


# ------------------------------------------------------------
# 53.3 PERIODE COUVERTE
# ------------------------------------------------------------

range(
  data_ml[
    usable_ml,
    MthCalDt
  ]
)


# ------------------------------------------------------------
# 53.4 OBSERVATIONS UTILISABLES PAR ANNEE
# ------------------------------------------------------------

# Ajouter l'indicateur directement dans data_ml
# afin qu'il soit correctement utilisé dans chaque groupe annuel.

data_ml[
  ,
  Usable := usable_ml
]


usable_by_year <- data_ml[
  ,
  .(
    N = .N,
    N_usable = sum(Usable)
  ),
  by = .(
    Year = as.integer(format(MthCalDt, "%Y"))
  )
]


usable_by_year[
  ,
  Pct_usable :=
    100 * N_usable / N
]


usable_by_year[]


# ------------------------------------------------------------
# 51.5 COUVERTURE SANS RD_Sales PAR ANNEE
# ------------------------------------------------------------

data_ml[
  ,
  .(
    N = .N,
    N_complete_without_RD = sum(
      complete.cases(.SD) &
        !is.na(Return_NextMonth)
    )
  ),
  by = .(
    Year = as.integer(format(MthCalDt, "%Y"))
  ),
  .SDcols = features_without_rd
][
  ,
  Pct_complete_without_RD :=
    100 * N_complete_without_RD / N
][]
# ------------------------------------------------------------
# 53.5 CONTROLE DE COHERENCE
# ------------------------------------------------------------

sum(usable_by_year$N_usable)

sum(usable_ml)

stopifnot(
  sum(usable_by_year$N_usable) ==
    sum(usable_ml)
)


# ============================================================
# 54. CONSTRUCTION DE L'ECHANTILLON FINAL DE MODELISATION
# ============================================================

# SUPPLEMENT
# Les contrôles précédents ont permis de définir les
# caractéristiques retenues pour le modèle principal.
#
# On construit maintenant une base contenant uniquement
# les observations utilisables :
#
# 1. toutes les caractéristiques finales sont disponibles ;
# 2. le rendement du mois suivant est disponible.
#
# La base complète data_ml est conservée intacte.


# ------------------------------------------------------------
# 54.1 CREATION DE LA BASE DE MODELISATION
# ------------------------------------------------------------

# IMPORTANT :
#
# On conserve maintenant trois notions de rendement distinctes :
#
# 1. Return_1M
#    = rendement retardé R_(t-1)
#    = caractéristique prédictive parmi les 13 features
#
# 2. MthRet
#    = rendement réalisé au mois t
#    = utilisé pour estimer correctement les expositions FF5
#
# 3. Return_NextMonth
#    = rendement réalisé au mois t+1
#    = rendement futur / variable cible
#
# Cette distinction évite tout décalage temporel dans
# l'estimation des modèles Fama-French.


data_model <- data_ml[
  Usable == TRUE,
  c(
    list(
      
      # --------------------------------------------------------
      # IDENTIFIANTS
      # --------------------------------------------------------
      
      PERMNO = PERMNO,
      Ticker = Ticker,
      MthCalDt = MthCalDt,
      gsector = gsector,
      
      
      # --------------------------------------------------------
      # RENDEMENTS
      # --------------------------------------------------------
      
      # Rendement courant au mois t.
      # Celui-ci sera utilisé dans l'estimation FF5 :
      #
      # Excess_Return_Current_t = MthRet_t - RF_t
      
      MthRet = MthRet,
      
      
      # Rendement futur au mois t+1.
      # Il provient maintenant du CRSP complet.
      
      Return_NextMonth = Return_NextMonth,
      
      
      # Rendement excédentaire futur :
      #
      # Return_NextMonth - RF_NextMonth
      
      Excess_Return_NextMonth =
        Excess_Return_NextMonth,
      
      
      # --------------------------------------------------------
      # FAMA-FRENCH AU MOIS t
      # --------------------------------------------------------
      
      # Ces facteurs sont alignés avec MthRet_t.
      # Ils serviront notamment à estimer les expositions
      # historiques FF5.
      
      MKT_RF = MKT_RF,
      SMB = SMB,
      HML = HML,
      RMW = RMW,
      CMA = CMA,
      RF = RF,
      
      
      # --------------------------------------------------------
      # FAMA-FRENCH AU MOIS t+1
      # --------------------------------------------------------
      
      # Ces facteurs sont conservés pour la décomposition
      # ex post du rendement futur.
      #
      # Ils ne doivent PAS être utilisés comme prédicteurs
      # disponibles à la date t.
      
      MKT_RF_NextMonth =
        MKT_RF_NextMonth,
      
      SMB_NextMonth =
        SMB_NextMonth,
      
      HML_NextMonth =
        HML_NextMonth,
      
      RMW_NextMonth =
        RMW_NextMonth,
      
      CMA_NextMonth =
        CMA_NextMonth,
      
      RF_NextMonth =
        RF_NextMonth
    ),
    
    # ----------------------------------------------------------
    # 13 CARACTERISTIQUES PREDICTIVES
    # ----------------------------------------------------------
    
    .SD
  ),
  
  .SDcols = features_final
]


# ------------------------------------------------------------
# TRI CHRONOLOGIQUE
# ------------------------------------------------------------

setorder(
  data_model,
  MthCalDt,
  PERMNO
)


# ------------------------------------------------------------
# CONTROLE DE LA DISTINCTION DES RENDEMENTS
# ------------------------------------------------------------

# Return_1M doit déjà faire partie de features_final.
# MthRet est maintenant conservé séparément.
# Return_NextMonth représente le rendement futur.

stopifnot(
  all(
    c(
      "Return_1M",
      "MthRet",
      "Return_NextMonth"
    ) %in%
      names(data_model)
  )
)


# Vérification visuelle

head(
  data_model[
    ,
    .(
      PERMNO,
      Ticker,
      MthCalDt,
      Return_1M,
      MthRet,
      Return_NextMonth
    )
  ],
  20
)

# ============================================================
# 55. CONTROLES DE L'ECHANTILLON FINAL
# ============================================================

# ------------------------------------------------------------
# 55.1 DIMENSIONS
# ------------------------------------------------------------

dim(data_model)


# ------------------------------------------------------------
# 55.2 NOMBRE DE TITRES
# ------------------------------------------------------------

uniqueN(
  data_model$PERMNO
)


# ------------------------------------------------------------
# 55.3 PERIODE COUVERTE
# ------------------------------------------------------------

range(
  data_model$MthCalDt
)


# ------------------------------------------------------------
# 55.4 VALEURS MANQUANTES DANS LES CARACTERISTIQUES
# ------------------------------------------------------------

sapply(
  data_model[
    ,
    ..features_final
  ],
  function(x) sum(is.na(x))
)


# ------------------------------------------------------------
# 55.5 VALEURS MANQUANTES DANS LA CIBLE
# ------------------------------------------------------------

sum(
  is.na(data_model$Return_NextMonth)
)


# ------------------------------------------------------------
# 55.6 VALEURS INFINIES
# ------------------------------------------------------------

sapply(
  data_model[
    ,
    c(features_final, "Return_NextMonth"),
    with = FALSE
  ],
  function(x) sum(is.infinite(x))
)


# ------------------------------------------------------------
# 55.7 DOUBLONS ACTION-MOIS
# ------------------------------------------------------------

duplicates_model <- data_model[
  ,
  .N,
  by = .(
    PERMNO,
    MthCalDt
  )
][
  N > 1
]

nrow(duplicates_model)


# ------------------------------------------------------------
# 55.8 APERCU DE LA BASE FINALE
# ------------------------------------------------------------

head(
  data_model,
  20
)

# ============================================================
# 56. STRUCTURE TEMPORELLE DE L'ECHANTILLON DE MODELISATION
# ============================================================

# SUPPLEMENT
# Avant de définir les périodes d'entraînement,
# de validation et de test, on examine la distribution
# temporelle de l'échantillon final.
#
# Le découpage sera effectué chronologiquement afin
# d'éviter toute fuite d'information entre le passé
# et le futur.


# ------------------------------------------------------------
# 56.1 NOMBRE D'OBSERVATIONS PAR ANNEE
# ------------------------------------------------------------

model_by_year <- data_model[
  ,
  .(
    N = .N,
    N_stocks = uniqueN(PERMNO)
  ),
  by = .(
    Year = as.integer(format(MthCalDt, "%Y"))
  )
]

model_by_year[]


# ------------------------------------------------------------
# 56.2 NOMBRE DE MOIS DISPONIBLES PAR ANNEE
# ------------------------------------------------------------

months_by_year <- data_model[
  ,
  .(
    N_months = uniqueN(
      format(MthCalDt, "%Y-%m")
    )
  ),
  by = .(
    Year = as.integer(format(MthCalDt, "%Y"))
  )
]

months_by_year[]


# ------------------------------------------------------------
# 56.3 NOMBRE TOTAL DE MOIS
# ------------------------------------------------------------

uniqueN(
  format(
    data_model$MthCalDt,
    "%Y-%m"
  )
)


# ------------------------------------------------------------
# 56.4 OBSERVATIONS PAR MOIS
# ------------------------------------------------------------

model_by_month <- data_model[
  ,
  .(
    N = .N,
    N_stocks = uniqueN(PERMNO)
  ),
  by = MthCalDt
]

summary(
  model_by_month$N
)


# ------------------------------------------------------------
# 56.5 PREMIERS ET DERNIERS MOIS
# ------------------------------------------------------------

head(
  model_by_month,
  12
)

tail(
  model_by_month,
  12
)

# ============================================================
# 57. DECOUPAGE TEMPOREL TRAIN / VALIDATION / TEST
# ============================================================

# SUPPLEMENT
# Les données financières ont une structure temporelle.
#
# On ne réalise donc PAS de séparation aléatoire.
#
# Les observations les plus anciennes servent à entraîner
# le modèle, les observations suivantes à sélectionner et
# régler le modèle, et les observations les plus récentes
# constituent l'échantillon de test hors échantillon.
#
# Découpage retenu :
#
# Train      : 2001 - 2018
# Validation : 2019 - 2021
# Test       : 2022 - 2025


# ------------------------------------------------------------
# 57.1 CREATION DE L'ANNEE
# ------------------------------------------------------------

data_model[
  ,
  Year := as.integer(
    format(MthCalDt, "%Y")
  )
]


# ------------------------------------------------------------
# 57.2 CREATION DES TROIS ECHANTILLONS
# ------------------------------------------------------------

train_data <- data_model[
  Year <= 2018
]

validation_data <- data_model[
  Year >= 2019 &
    Year <= 2021
]

test_data <- data_model[
  Year >= 2022
]


# ============================================================
# 58. CONTROLES DU DECOUPAGE TEMPOREL
# ============================================================

# ------------------------------------------------------------
# 58.1 DIMENSIONS
# ------------------------------------------------------------

dim(train_data)

dim(validation_data)

dim(test_data)


# ------------------------------------------------------------
# 58.2 PERIODES
# ------------------------------------------------------------

range(
  train_data$MthCalDt
)

range(
  validation_data$MthCalDt
)

range(
  test_data$MthCalDt
)


# ------------------------------------------------------------
# 58.3 NOMBRE DE MOIS
# ------------------------------------------------------------

uniqueN(
  format(
    train_data$MthCalDt,
    "%Y-%m"
  )
)

uniqueN(
  format(
    validation_data$MthCalDt,
    "%Y-%m"
  )
)

uniqueN(
  format(
    test_data$MthCalDt,
    "%Y-%m"
  )
)


# ------------------------------------------------------------
# 58.4 NOMBRE DE TITRES
# ------------------------------------------------------------

uniqueN(
  train_data$PERMNO
)

uniqueN(
  validation_data$PERMNO
)

uniqueN(
  test_data$PERMNO
)


# ------------------------------------------------------------
# 58.5 PROPORTION DES OBSERVATIONS
# ------------------------------------------------------------

split_summary <- data.table(
  Sample = c(
    "Train",
    "Validation",
    "Test"
  ),
  N = c(
    nrow(train_data),
    nrow(validation_data),
    nrow(test_data)
  )
)

split_summary[
  ,
  Pct := 100 * N / nrow(data_model)
]

split_summary[]


# ------------------------------------------------------------
# 58.6 VERIFIER QUE TOUTES LES OBSERVATIONS SONT CONSERVEES
# ------------------------------------------------------------

sum(
  nrow(train_data),
  nrow(validation_data),
  nrow(test_data)
)

nrow(
  data_model
)

stopifnot(
  nrow(train_data) +
    nrow(validation_data) +
    nrow(test_data) ==
    nrow(data_model)
)


# ------------------------------------------------------------
# 58.7 VERIFIER L'ORDRE TEMPOREL
# ------------------------------------------------------------

max(train_data$MthCalDt)

min(validation_data$MthCalDt)

max(validation_data$MthCalDt)

min(test_data$MthCalDt)

stopifnot(
  max(train_data$MthCalDt) <
    min(validation_data$MthCalDt)
)

stopifnot(
  max(validation_data$MthCalDt) <
    min(test_data$MthCalDt)
)

# ============================================================
# 59. STANDARDISATION DES CARACTERISTIQUES
# ============================================================

# SUPPLEMENT
# Certaines méthodes de machine learning sont sensibles
# à l'échelle des variables.
#
# On standardise donc les caractéristiques selon :
#
#               x - moyenne
#       z = ---------------------
#            ecart-type
#
# IMPORTANT :
# les moyennes et écarts-types sont calculés uniquement
# sur l'échantillon d'entraînement.
#
# Les mêmes paramètres sont ensuite appliqués aux données
# de validation et de test afin d'éviter toute fuite
# d'information provenant du futur.


# ------------------------------------------------------------
# 59.1 MOYENNES ET ECARTS-TYPES DU TRAIN
# ------------------------------------------------------------

scaling_parameters <- data.table(
  Feature = features_final,
  
  Mean = sapply(
    train_data[
      ,
      ..features_final
    ],
    mean
  ),
  
  SD = sapply(
    train_data[
      ,
      ..features_final
    ],
    sd
  )
)

scaling_parameters[]


# ------------------------------------------------------------
# 59.2 VERIFIER LES ECARTS-TYPES
# ------------------------------------------------------------

# Aucun écart-type ne doit être nul.

scaling_parameters[
  SD == 0 |
    is.na(SD)
]


# ------------------------------------------------------------
# 59.3 COPIES DES TROIS ECHANTILLONS
# ------------------------------------------------------------

train_scaled <- copy(train_data)

validation_scaled <- copy(validation_data)

test_scaled <- copy(test_data)


# ------------------------------------------------------------
# 59.4 STANDARDISATION DU TRAIN
# ------------------------------------------------------------

for (feature in features_final) {
  
  mu <- scaling_parameters[
    Feature == feature,
    Mean
  ]
  
  sigma <- scaling_parameters[
    Feature == feature,
    SD
  ]
  
  train_scaled[
    ,
    (feature) :=
      (get(feature) - mu) / sigma
  ]
}


# ------------------------------------------------------------
# 59.5 STANDARDISATION DE LA VALIDATION
# ------------------------------------------------------------

for (feature in features_final) {
  
  mu <- scaling_parameters[
    Feature == feature,
    Mean
  ]
  
  sigma <- scaling_parameters[
    Feature == feature,
    SD
  ]
  
  validation_scaled[
    ,
    (feature) :=
      (get(feature) - mu) / sigma
  ]
}


# ------------------------------------------------------------
# 59.6 STANDARDISATION DU TEST
# ------------------------------------------------------------

for (feature in features_final) {
  
  mu <- scaling_parameters[
    Feature == feature,
    Mean
  ]
  
  sigma <- scaling_parameters[
    Feature == feature,
    SD
  ]
  
  test_scaled[
    ,
    (feature) :=
      (get(feature) - mu) / sigma
  ]
}


# ============================================================
# 60. CONTROLES DE LA STANDARDISATION
# ============================================================

# ------------------------------------------------------------
# 60.1 MOYENNES DU TRAIN STANDARDISE
# ------------------------------------------------------------

sapply(
  train_scaled[
    ,
    ..features_final
  ],
  mean
)


# ------------------------------------------------------------
# 60.2 ECARTS-TYPES DU TRAIN STANDARDISE
# ------------------------------------------------------------

sapply(
  train_scaled[
    ,
    ..features_final
  ],
  sd
)


# ------------------------------------------------------------
# 60.3 VALEURS MANQUANTES
# ------------------------------------------------------------

sapply(
  train_scaled[
    ,
    ..features_final
  ],
  function(x) sum(is.na(x))
)

sapply(
  validation_scaled[
    ,
    ..features_final
  ],
  function(x) sum(is.na(x))
)

sapply(
  test_scaled[
    ,
    ..features_final
  ],
  function(x) sum(is.na(x))
)


# ------------------------------------------------------------
# 60.4 VALEURS INFINIES
# ------------------------------------------------------------

sapply(
  train_scaled[
    ,
    ..features_final
  ],
  function(x) sum(is.infinite(x))
)

sapply(
  validation_scaled[
    ,
    ..features_final
  ],
  function(x) sum(is.infinite(x))
)

sapply(
  test_scaled[
    ,
    ..features_final
  ],
  function(x) sum(is.infinite(x))
)


# ------------------------------------------------------------
# 60.5 VERIFIER QUE LA CIBLE N'A PAS ETE MODIFIEE
# ------------------------------------------------------------

stopifnot(
  identical(
    train_scaled$Return_NextMonth,
    train_data$Return_NextMonth
  )
)

stopifnot(
  identical(
    validation_scaled$Return_NextMonth,
    validation_data$Return_NextMonth
  )
)

stopifnot(
  identical(
    test_scaled$Return_NextMonth,
    test_data$Return_NextMonth
  )
)


# ============================================================
# 61. VALIDATION TEMPORELLE PAR EXPANDING WINDOW
# ============================================================

# SUPPLEMENT
# Les observations financières sont ordonnées dans le temps.
#
# Une validation croisée aléatoire n'est donc pas utilisée,
# car elle pourrait introduire de l'information future
# dans l'entraînement.
#
# On utilise une validation temporelle de type
# EXPANDING WINDOW :
#
# Fold 1 : Train 2001-2010 -> Validation 2011
# Fold 2 : Train 2001-2011 -> Validation 2012
# ...
# Fold 11: Train 2001-2020 -> Validation 2021
#
# IMPORTANT :
# Return_NextMonth correspond au rendement du mois suivant.
# Le dernier mois de chaque fenêtre d'entraînement est donc
# retiré afin que sa cible ne soit pas réalisée pendant
# la période de validation.
#
# Le test final 2022-2025 reste totalement hors échantillon.


# ------------------------------------------------------------
# 61.1 DONNEES DISPONIBLES POUR LA VALIDATION TEMPORELLE
# ------------------------------------------------------------

development_data <- data_model[
  Year <= 2021
]

range(development_data$MthCalDt)

uniqueN(
  format(
    development_data$MthCalDt,
    "%Y-%m"
  )
)


# ------------------------------------------------------------
# 61.2 ANNEES DE VALIDATION
# ------------------------------------------------------------

validation_years <- 2011:2021

validation_years


# ------------------------------------------------------------
# 61.3 CONSTRUCTION DES FOLDS EXPANDING WINDOW
# ------------------------------------------------------------

expanding_folds <- vector(
  "list",
  length(validation_years)
)

names(expanding_folds) <- paste0(
  "Validation_",
  validation_years
)


for (i in seq_along(validation_years)) {
  
  validation_year <- validation_years[i]
  
  
  # ----------------------------------------------------------
  # Données antérieures à l'année de validation
  # ----------------------------------------------------------
  
  fold_train <- development_data[
    Year < validation_year
  ]
  
  
  # ----------------------------------------------------------
  # PURGE D'UN MOIS
  #
  # La cible Return_NextMonth du dernier mois du train
  # est réalisée pendant le premier mois de validation.
  # On retire donc ce dernier mois.
  # ----------------------------------------------------------
  
  last_train_month <- max(
    fold_train$MthCalDt
  )
  
  fold_train <- fold_train[
    MthCalDt < last_train_month
  ]
  
  
  # ----------------------------------------------------------
  # Validation : année suivante complète
  # ----------------------------------------------------------
  
  fold_validation <- development_data[
    Year == validation_year
  ]
  
  
  # ----------------------------------------------------------
  # Stockage du fold
  # ----------------------------------------------------------
  
  expanding_folds[[i]] <- list(
    validation_year = validation_year,
    train = fold_train,
    validation = fold_validation
  )
}


# ============================================================
# 62. CONTROLE DES FOLDS TEMPORELS
# ============================================================

# ------------------------------------------------------------
# 62.1 RESUME DES FOLDS
# ------------------------------------------------------------

fold_summary <- rbindlist(
  lapply(
    seq_along(expanding_folds),
    function(i) {
      
      fold <- expanding_folds[[i]]
      
      data.table(
        Fold = i,
        
        Validation_Year =
          fold$validation_year,
        
        Train_Start =
          min(fold$train$MthCalDt),
        
        Train_End =
          max(fold$train$MthCalDt),
        
        Validation_Start =
          min(fold$validation$MthCalDt),
        
        Validation_End =
          max(fold$validation$MthCalDt),
        
        N_Train =
          nrow(fold$train),
        
        N_Validation =
          nrow(fold$validation),
        
        N_Stocks_Train =
          uniqueN(fold$train$PERMNO),
        
        N_Stocks_Validation =
          uniqueN(fold$validation$PERMNO)
      )
    }
  )
)

fold_summary[]


# ------------------------------------------------------------
# 62.2 VERIFIER L'ORDRE TEMPOREL
# ------------------------------------------------------------

for (fold in expanding_folds) {
  
  stopifnot(
    max(fold$train$MthCalDt) <
      min(fold$validation$MthCalDt)
  )
}


# ------------------------------------------------------------
# 62.3 VERIFIER LA PURGE
# ------------------------------------------------------------

purge_check <- rbindlist(
  lapply(
    expanding_folds,
    function(fold) {
      
      data.table(
        Validation_Year =
          fold$validation_year,
        
        Last_Train_Date =
          max(fold$train$MthCalDt),
        
        First_Validation_Date =
          min(fold$validation$MthCalDt)
      )
    }
  )
)

purge_check[]


# ------------------------------------------------------------
# 62.4 NOMBRE TOTAL DE FOLDS
# ------------------------------------------------------------

length(expanding_folds)


# ============================================================
# 63. SAUVEGARDE DES DONNEES PREPAREES
# ============================================================

# Créer le dossier de sauvegarde s'il n'existe pas
dir.create(
  "data/données_preparees",
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 63.1 SAUVEGARDE DE LA BASE DE MODELISATION
# ------------------------------------------------------------

saveRDS(
  data_model,
  "data/données_preparees/data_model.rds"
)


# ------------------------------------------------------------
# 63.2 SAUVEGARDE DES FOLDS TEMPORELS
# ------------------------------------------------------------

saveRDS(
  expanding_folds,
  "data/données_preparees/expanding_folds.rds"
)


# ------------------------------------------------------------
# 63.3 SAUVEGARDE DE LA LISTE DES CARACTERISTIQUES
# ------------------------------------------------------------

saveRDS(
  features_final,
  "data/données_preparees/features_final.rds"
)


# ------------------------------------------------------------
# 63.4 VERIFICATION
# ------------------------------------------------------------

file.exists(
  "data/données_preparees/data_model.rds"
)

file.exists(
  "data/données_preparees/expanding_folds.rds"
)

file.exists(
  "data/données_preparees/features_final.rds"
)