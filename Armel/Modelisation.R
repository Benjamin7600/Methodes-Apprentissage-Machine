# ============================================================
# 1. INITIALISATION DE LA MODELISATION
# ============================================================

library(data.table)


# ------------------------------------------------------------
# 1.1 CHARGEMENT DES DONNEES PREPAREES
# ------------------------------------------------------------

data_model <- readRDS(
  "Armel/data/donnees_preparees/data_model.rds"
)

expanding_folds <- readRDS(
  "Armel/data/donnees_preparees/expanding_folds.rds"
)

features_final <- readRDS(
  "Armel/data/donnees_preparees/features_final.rds"
)


# ------------------------------------------------------------
# 1.2 CONTROLES GENERAUX
# ------------------------------------------------------------

dim(data_model)

names(data_model)

length(features_final)

features_final

length(expanding_folds)


# ============================================================
# 2. PREPARATION DE LA MODELISATION FAMA-FRENCH 5
# ============================================================

# Objectif :
#
# Décomposer le rendement excédentaire du mois suivant :
#
# R_(i,t+1) - RF_(t+1)
#
# en :
#
# 1. une composante expliquée par Fama-French 5 ;
# 2. une composante résiduelle.
#
# Le résidu FF5 sera ensuite étudié à partir des
# 13 caractéristiques propres aux actions.
#
# IMPORTANT :
#
# Les facteurs FF5 du mois t+1 sont utilisés pour mesurer
# EX POST la composante FF5 du rendement réalisé en t+1.
#
# Ils ne seront PAS utilisés comme variables prédictives
# dans la PCA ou dans le modèle ML.


# ------------------------------------------------------------
# 2.1 VARIABLES FAMA-FRENCH
# ------------------------------------------------------------

ff5_predictors <- c(
  "MKT_RF",
  "SMB",
  "HML",
  "RMW",
  "CMA"
)

ff5_predictors_next <- c(
  "MKT_RF_NextMonth",
  "SMB_NextMonth",
  "HML_NextMonth",
  "RMW_NextMonth",
  "CMA_NextMonth"
)


# ------------------------------------------------------------
# 2.2 VERIFICATION DES VARIABLES NECESSAIRES
# ------------------------------------------------------------

ff5_required <- c(
  "PERMNO",
  "MthCalDt",
  "Return_NextMonth",
  "Excess_Return_NextMonth",
  ff5_predictors_next
)

stopifnot(
  all(
    ff5_required %in%
      names(data_model)
  )
)


# Vérifier également les 13 caractéristiques

stopifnot(
  all(
    features_final %in%
      names(data_model)
  )
)

stopifnot(
  length(features_final) == 13
)


# ------------------------------------------------------------
# 2.3 OBSERVATIONS COMPLETES POUR FAMA-FRENCH
# ------------------------------------------------------------

ff5_complete <- complete.cases(
  data_model[
    ,
    ..ff5_required
  ]
)

sum(ff5_complete)

100 * mean(ff5_complete)


# ------------------------------------------------------------
# 2.4 HISTORIQUE DISPONIBLE PAR ACTION
# ------------------------------------------------------------

ff_history_check <- data_model[
  ff5_complete,
  .(
    N_Months = .N,
    
    First_Month =
      min(MthCalDt),
    
    Last_Month =
      max(MthCalDt)
  ),
  by = PERMNO
]


summary(
  ff_history_check$N_Months
)


# ------------------------------------------------------------
# 2.5 COUVERTURE SELON LA LONGUEUR DE LA FENETRE
# ------------------------------------------------------------

ff_window_check <- data.table(
  Window = c(
    24,
    36,
    48,
    60
  )
)


ff_window_check[
  ,
  N_Stocks :=
    sapply(
      Window,
      function(w) {
        sum(
          ff_history_check$N_Months >= w
        )
      }
    )
]


ff_window_check[
  ,
  Pct_Stocks :=
    100 *
    N_Stocks /
    nrow(ff_history_check)
]


ff_window_check

# ============================================================
# 3. ESTIMATION ROULANTE FAMA-FRENCH 5 FACTEURS
# ============================================================

# Objectif :
#
# Estimer les expositions FF5 de chaque action à la date t
# uniquement à partir de son historique de rendements CRSP
# disponible jusqu'à t.
#
# IMPORTANT :
#
# - l'historique FF5 est indépendant de Usable ;
# - il ne dépend pas de Return_NextMonth ;
# - il ne dépend pas de la disponibilité des 13 caractéristiques ;
# - fenêtre maximale = 60 mois calendaires ;
# - minimum = 36 observations complètes ;
# - les bêta estimés à t sont ensuite rattachés à data_model.


ff_window  <- 60L
ff_min_obs <- 36L


# ------------------------------------------------------------
# 3.1 CONSTRUCTION DE L'HISTORIQUE FF5 INDEPENDANT
# ------------------------------------------------------------

# On part de CRSP complet et NON de data_model.

ff5_history <- copy(crsp)

ff5_history[
  ,
  YearMonth := format(
    MthCalDt,
    "%Y-%m"
  )
]


# ------------------------------------------------------------
# 3.2 PREPARATION DES FACTEURS FF5 AU MOIS t
# ------------------------------------------------------------

# L'objet ff5 possède déjà :
# MthCalDt, MKT_RF, SMB, HML, RMW, CMA, RF, YearMonth

ff5_history_factors <- copy(ff5)

ff5_history_factors <- ff5_history_factors[
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

# ------------------------------------------------------------
# 3.3 FUSION CRSP - FF5
# ------------------------------------------------------------

ff5_history <- merge(
  ff5_history,
  ff5_history_factors,
  by = "YearMonth",
  all.x = TRUE,
  sort = FALSE
)


# Rendement excédentaire réellement réalisé au mois t.

ff5_history[
  ,
  Excess_Return_Current :=
    MthRet - RF
]


# Trier chronologiquement.

setorder(
  ff5_history,
  PERMNO,
  MthCalDt
)


# ------------------------------------------------------------
# 3.4 NETTOYAGE ET CONTROLE DES DOUBLONS CRSP
# ------------------------------------------------------------

# CRSP peut contenir plusieurs lignes pour un même PERMNO-mois,
# notamment une ligne normale et une ligne "LAST KNOWN".
#
# On applique la même logique de nettoyage que dans la
# préparation principale :
# priorité à l'observation normale plutôt qu'à "LAST KNOWN".


# Identifier les observations LAST KNOWN

ff5_history[
  ,
  IsLastKnown :=
    grepl(
      "LAST KNOWN",
      SecurityNm,
      ignore.case = TRUE
    )
]


# Priorité :
# FALSE = observation normale
# TRUE  = LAST KNOWN

setorder(
  ff5_history,
  PERMNO,
  YearMonth,
  IsLastKnown
)


# Une seule observation par PERMNO et mois

ff5_history <- ff5_history[
  ,
  .SD[1],
  by = .(
    PERMNO,
    YearMonth
  )
]


# Remettre l'ordre chronologique

setorder(
  ff5_history,
  PERMNO,
  MthCalDt
)


# ------------------------------------------------------------
# CONTROLE FINAL
# ------------------------------------------------------------

ff5_history_duplicates <- ff5_history[
  ,
  .N,
  by = .(
    PERMNO,
    YearMonth
  )
][
  N > 1
]


nrow(
  ff5_history_duplicates
)


stopifnot(
  nrow(ff5_history_duplicates) == 0
)


# Variable technique devenue inutile

ff5_history[
  ,
  IsLastKnown := NULL
]

# ------------------------------------------------------------
# 3.5 FONCTION D'ESTIMATION ROULANTE
# ------------------------------------------------------------

estimate_ff5_history <- function(dt) {
  
  setorder(
    dt,
    MthCalDt
  )
  
  n <- nrow(dt)
  
  dt[
    ,
    `:=`(
      Alpha_FF5 = NA_real_,
      Beta_MKT = NA_real_,
      Beta_SMB = NA_real_,
      Beta_HML = NA_real_,
      Beta_RMW = NA_real_,
      Beta_CMA = NA_real_,
      FF5_N_Obs = NA_integer_,
      FF5_First_Month = as.IDate(NA)
    )
  ]
  
  
  # Index mensuel permettant d'imposer
  # une vraie fenêtre de 60 mois calendaires.
  
  obs_year <- as.integer(
    format(
      dt$MthCalDt,
      "%Y"
    )
  )
  
  obs_month <- as.integer(
    format(
      dt$MthCalDt,
      "%m"
    )
  )
  
  obs_month_index <-
    12L * obs_year +
    obs_month
  
  
  for (i in seq_len(n)) {
    
    end_month_index <-
      obs_month_index[i]
    
    start_month_index <-
      end_month_index -
      ff_window +
      1L
    
    
    hist_idx <- which(
      obs_month_index >=
        start_month_index &
        obs_month_index <=
        end_month_index
    )
    
    
    hist <- dt[
      hist_idx,
      .(
        MthCalDt,
        Excess_Return_Current,
        MKT_RF,
        SMB,
        HML,
        RMW,
        CMA
      )
    ]
    
    
    # Retirer uniquement les observations
    # impossibles à utiliser dans la régression FF5.
    
    hist <- hist[
      complete.cases(
        Excess_Return_Current,
        MKT_RF,
        SMB,
        HML,
        RMW,
        CMA
      )
    ]
    
    
    dt$FF5_N_Obs[i] <-
      nrow(hist)
    
    
    if (
      nrow(hist) <
      ff_min_obs
    ) {
      next
    }
    
    
    fit <- lm(
      Excess_Return_Current ~
        MKT_RF +
        SMB +
        HML +
        RMW +
        CMA,
      data = hist
    )
    
    
    b <- coef(fit)
    
    
    # Protection contre une régression singulière.
    
    if (
      length(b) != 6L ||
      anyNA(b)
    ) {
      next
    }
    
    
    dt$Alpha_FF5[i] <- b[1]
    dt$Beta_MKT[i]  <- b[2]
    dt$Beta_SMB[i]  <- b[3]
    dt$Beta_HML[i]  <- b[4]
    dt$Beta_RMW[i]  <- b[5]
    dt$Beta_CMA[i]  <- b[6]
    
    dt$FF5_First_Month[i] <-
      min(
        hist$MthCalDt
      )
  }
  
  
  dt[
    ,
    .(
      MthCalDt,
      Alpha_FF5,
      Beta_MKT,
      Beta_SMB,
      Beta_HML,
      Beta_RMW,
      Beta_CMA,
      FF5_N_Obs,
      FF5_First_Month
    )
  ]
}


# ------------------------------------------------------------
# 3.6 ESTIMATION SUR L'HISTORIQUE CRSP
# ------------------------------------------------------------

ff5_betas_history <- ff5_history[
  ,
  estimate_ff5_history(
    copy(.SD)
  ),
  by = PERMNO
]


# Vérifier l'unicité de la clé.

stopifnot(
  nrow(
    ff5_betas_history[
      ,
      .N,
      by = .(
        PERMNO,
        MthCalDt
      )
    ][
      N > 1
    ]
  ) == 0
)


# ------------------------------------------------------------
# 3.7 RATTACHER LES BETA A L'ECHANTILLON ML
# ------------------------------------------------------------

# data_model reste notre échantillon ML.
# On ne change ni ses 13 caractéristiques,
# ni sa cible, ni son univers.

ff5_model_data <- merge(
  copy(data_model),
  ff5_betas_history,
  by = c(
    "PERMNO",
    "MthCalDt"
  ),
  all.x = TRUE,
  sort = FALSE
)

setorder(
  ff5_model_data,
  MthCalDt,
  PERMNO
)


# Vérification fondamentale :
# le rattachement des bêta ne doit supprimer
# ni créer aucune observation ML.

stopifnot(
  nrow(ff5_model_data) ==
    nrow(data_model)
)


# ------------------------------------------------------------
# 3.8 PREDICTION FF5 EX POST DU RENDEMENT t+1
# ------------------------------------------------------------

# Les bêta sont connus à t.
#
# Les facteurs FF5 réalisés à t+1 servent uniquement
# à décomposer EX POST le rendement futur.
#
# Ils ne sont jamais transmis au LASSO.

ff5_model_data[
  ,
  FF5_Prediction_NextMonth :=
    
    Alpha_FF5 +
    
    Beta_MKT *
    MKT_RF_NextMonth +
    
    Beta_SMB *
    SMB_NextMonth +
    
    Beta_HML *
    HML_NextMonth +
    
    Beta_RMW *
    RMW_NextMonth +
    
    Beta_CMA *
    CMA_NextMonth
]


# ------------------------------------------------------------
# 3.9 RESIDU FF5 DU MOIS t+1
# ------------------------------------------------------------

ff5_model_data[
  ,
  FF5_Residual_NextMonth :=
    
    Excess_Return_NextMonth -
    
    FF5_Prediction_NextMonth
]


# ------------------------------------------------------------
# 3.10 CONTROLES FINAUX
# ------------------------------------------------------------

# Maximum 60 observations.

max(
  ff5_model_data$FF5_N_Obs,
  na.rm = TRUE
)

stopifnot(
  max(
    ff5_model_data$FF5_N_Obs,
    na.rm = TRUE
  ) <= ff_window
)


summary(
  ff5_model_data$FF5_N_Obs
)


# Vérification de la durée calendaire
# uniquement lorsqu'une estimation existe.

ff5_window_diagnostic <- ff5_model_data[
  !is.na(Alpha_FF5) &
    !is.na(FF5_First_Month),
  .(
    PERMNO,
    MthCalDt,
    FF5_First_Month,
    FF5_N_Obs,
    
    Calendar_Span_Months =
      
      12L *
      (
        as.integer(
          format(
            MthCalDt,
            "%Y"
          )
        ) -
          as.integer(
            format(
              FF5_First_Month,
              "%Y"
            )
          )
      ) +
      
      (
        as.integer(
          format(
            MthCalDt,
            "%m"
          )
        ) -
          as.integer(
            format(
              FF5_First_Month,
              "%m"
            )
          )
      ) +
      
      1L
  )
]


summary(
  ff5_window_diagnostic$Calendar_Span_Months
)


stopifnot(
  all(
    ff5_window_diagnostic$Calendar_Span_Months <=
      ff_window
  )
)


# Vérifier l'identité du résidu.

ff5_residual_identity_error <-
  ff5_model_data[
    !is.na(
      FF5_Residual_NextMonth
    ),
    max(
      abs(
        FF5_Residual_NextMonth -
          (
            Excess_Return_NextMonth -
              FF5_Prediction_NextMonth
          )
      )
    )
  ]


ff5_residual_identity_error


stopifnot(
  ff5_residual_identity_error <
    1e-12
)


# ------------------------------------------------------------
# 3.11 CONTROLE DU DECOUPLAGE ML / FF5
# ------------------------------------------------------------

cat(
  "\nNombre observations data_model :",
  nrow(data_model),
  "\n"
)

cat(
  "Nombre observations ff5_model_data :",
  nrow(ff5_model_data),
  "\n"
)

cat(
  "Nombre de bêta FF5 disponibles :",
  sum(
    !is.na(
      ff5_model_data$Alpha_FF5
    )
  ),
  "\n"
)

cat(
  "Nombre de résidus disponibles :",
  sum(
    !is.na(
      ff5_model_data$FF5_Residual_NextMonth
    )
  ),
  "\n"
)


# ============================================================
# 4. CONTROLES DU RESIDU FAMA-FRENCH
# ============================================================


# ------------------------------------------------------------
# 4.1 NOMBRE D'OBSERVATIONS DISPONIBLES
# ------------------------------------------------------------

ff5_residual_check <- ff5_model_data[
  ,
  .(
    N_Total =
      .N,
    
    N_With_Betas =
      sum(
        complete.cases(
          Alpha_FF5,
          Beta_MKT,
          Beta_SMB,
          Beta_HML,
          Beta_RMW,
          Beta_CMA
        )
      ),
    
    N_With_Residual =
      sum(
        !is.na(
          FF5_Residual_NextMonth
        )
      )
  )
]


ff5_residual_check


# ------------------------------------------------------------
# 4.2 POURCENTAGE CONSERVE
# ------------------------------------------------------------

ff5_residual_check[
  ,
  Pct_With_Residual :=
    100 *
    N_With_Residual /
    N_Total
]


ff5_residual_check


# ------------------------------------------------------------
# 4.3 DISTRIBUTION DU RESIDU
# ------------------------------------------------------------

summary(
  ff5_model_data$FF5_Residual_NextMonth
)


# ------------------------------------------------------------
# 4.4 CONTROLE DES COEFFICIENTS
# ------------------------------------------------------------

summary(
  ff5_model_data[
    ,
    .(
      Alpha_FF5,
      Beta_MKT,
      Beta_SMB,
      Beta_HML,
      Beta_RMW,
      Beta_CMA
    )
  ]
)


# ------------------------------------------------------------
# 4.5 CONTROLE VISUEL
# ------------------------------------------------------------

ff5_model_data[
  !is.na(
    FF5_Residual_NextMonth
  ),
  .(
    PERMNO,
    MthCalDt,
    
    Excess_Return_NextMonth,
    
    FF5_Prediction_NextMonth,
    
    FF5_Residual_NextMonth,
    
    Beta_MKT,
    Beta_SMB,
    Beta_HML,
    Beta_RMW,
    Beta_CMA
  )
][1:20]



# ============================================================
# 5. PREPARATION DE L'ECHANTILLON POUR LE LASSO
# ============================================================

# Objectif :
#
# Prédire :
#
#     FF5_Residual_NextMonth
#
# à partir UNIQUEMENT des 13 caractéristiques disponibles
# au mois t.
#
# Les facteurs FF5 t+1 ne sont donc PAS des prédicteurs.


# ------------------------------------------------------------
# 5.1 CONSTRUCTION DE L'ECHANTILLON
# ------------------------------------------------------------

lasso_data <- ff5_model_data[
  !is.na(FF5_Residual_NextMonth)
]


# Vérifier les 13 caractéristiques

stopifnot(
  length(features_final) == 13
)

stopifnot(
  all(
    features_final %in%
      names(lasso_data)
  )
)


# Vérifier qu'elles sont complètes

stopifnot(
  all(
    complete.cases(
      lasso_data[
        ,
        ..features_final
      ]
    )
  )
)


# ------------------------------------------------------------
# 5.2 ANNEE
# ------------------------------------------------------------

lasso_data[
  ,
  Year :=
    as.integer(
      format(
        MthCalDt,
        "%Y"
      )
    )
]


# ------------------------------------------------------------
# 5.3 SPLIT FINAL
# ------------------------------------------------------------

# Développement :
# jusqu'à 2021.
#
# Test final :
# 2022-2025.
#
# LE TEST RESTE FERME pendant le choix de lambda.

lasso_development <- lasso_data[
  Year <= 2021
]

lasso_test <- lasso_data[
  Year >= 2022
]


cat(
  "\nDevelopment :",
  nrow(lasso_development),
  "\n"
)

cat(
  "Test final :",
  nrow(lasso_test),
  "\n"
)


# ============================================================
# 6. PREPARATION DU LASSO
# ============================================================

library(glmnet)


# ------------------------------------------------------------
# 6.1 GRILLE DE LAMBDA
# ------------------------------------------------------------

# On utilise la même grille pour tous les folds.
#
# Elle sera affinée automatiquement par la validation
# temporelle.

lambda_grid <- 10^seq(
  from = -5,
  to = 0,
  length.out = 60
)


# ------------------------------------------------------------
# 6.2 TABLE POUR STOCKER LES RESULTATS
# ------------------------------------------------------------

lasso_cv_results <- data.table()


# ============================================================
# 7. VALIDATION TEMPORELLE DU LASSO
# ============================================================

# Validation expanding window :
#
# Train <= année précédente
# Validation = année suivante
#
# Exemple :
#
# Train 2001-2010 -> Validation 2011
# Train 2001-2011 -> Validation 2012
# ...
# Train 2001-2020 -> Validation 2021
#
# IMPORTANT :
# lambda est choisi SANS utiliser 2022-2025.


validation_years_lasso <- 2011:2021


for (validation_year in validation_years_lasso) {
  
  cat(
    "\nValidation :",
    validation_year,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 7.1 TRAIN DU FOLD
  # ----------------------------------------------------------
  
  fold_train <- lasso_development[
    Year < validation_year
  ]
  
  
  # ----------------------------------------------------------
  # 7.2 PURGE DU DERNIER MOIS
  # ----------------------------------------------------------
  
  # La cible du dernier mois du train correspond au
  # rendement du mois suivant.
  #
  # On retire donc le dernier mois disponible avant
  # la validation.
  
  last_train_month <- max(
    fold_train$MthCalDt
  )
  
  
  fold_train <- fold_train[
    MthCalDt < last_train_month
  ]
  
  
  # ----------------------------------------------------------
  # 7.3 VALIDATION DU FOLD
  # ----------------------------------------------------------
  
  fold_validation <- lasso_development[
    Year == validation_year
  ]
  
  
  # ----------------------------------------------------------
  # 7.4 MATRICES X ET Y
  # ----------------------------------------------------------
  
  X_train <- as.matrix(
    fold_train[
      ,
      ..features_final
    ]
  )
  
  
  y_train <-
    fold_train$FF5_Residual_NextMonth
  
  
  X_validation <- as.matrix(
    fold_validation[
      ,
      ..features_final
    ]
  )
  
  
  y_validation <-
    fold_validation$FF5_Residual_NextMonth
  
  
  # ----------------------------------------------------------
  # 7.5 STANDARDISATION DU FOLD
  # ----------------------------------------------------------
  
  # Les moyennes et écarts-types sont calculés UNIQUEMENT
  # sur le train du fold.
  
  mu_fold <- colMeans(
    X_train
  )
  
  
  sd_fold <- apply(
    X_train,
    2,
    sd
  )
  
  
  # Protection éventuelle contre sd = 0
  
  sd_fold[
    !is.finite(sd_fold) |
      sd_fold == 0
  ] <- 1
  
  
  X_train_scaled <- scale(
    X_train,
    center = mu_fold,
    scale = sd_fold
  )
  
  
  X_validation_scaled <- scale(
    X_validation,
    center = mu_fold,
    scale = sd_fold
  )
  
  
  # ----------------------------------------------------------
  # 7.6 ESTIMATION DE TOUT LE CHEMIN LASSO
  # ----------------------------------------------------------
  
  fit_lasso <- glmnet(
    x = X_train_scaled,
    y = y_train,
    
    alpha = 1,
    
    lambda = lambda_grid,
    
    standardize = FALSE,
    
    intercept = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 7.7 PREDICTIONS POUR TOUS LES LAMBDA
  # ----------------------------------------------------------
  
  pred_matrix <- predict(
    fit_lasso,
    newx = X_validation_scaled,
    s = lambda_grid
  )
  
  
  # ----------------------------------------------------------
  # 7.8 PERFORMANCE DE CHAQUE LAMBDA
  # ----------------------------------------------------------
  
  for (j in seq_along(lambda_grid)) {
    
    pred_j <-
      as.numeric(
        pred_matrix[
          ,
          j
        ]
      )
    
    
    mse_j <- mean(
      (
        y_validation -
          pred_j
      )^2
    )
    
    
    mae_j <- mean(
      abs(
        y_validation -
          pred_j
      )
    )
    
    
    # R² hors échantillon relativement à une prédiction
    # constante estimée sur le TRAIN uniquement.
    
    benchmark <-
      mean(y_train)
    
    
    sse_model <- sum(
      (
        y_validation -
          pred_j
      )^2
    )
    
    
    sse_benchmark <- sum(
      (
        y_validation -
          benchmark
      )^2
    )
    
    
    r2_oos_j <-
      1 -
      sse_model /
      sse_benchmark
    
    
    # Corrélations
    
    pearson_j <- suppressWarnings(
      cor(
        y_validation,
        pred_j,
        method = "pearson"
      )
    )
    
    
    spearman_j <- suppressWarnings(
      cor(
        y_validation,
        pred_j,
        method = "spearman"
      )
    )
    
    
    # Nombre de coefficients non nuls
    
    coef_j <- coef(
      fit_lasso,
      s = lambda_grid[j]
    )
    
    
    n_nonzero_j <-
      sum(
        as.numeric(
          coef_j[-1, ]
        ) != 0
      )
    
    
    lasso_cv_results <- rbind(
      lasso_cv_results,
      
      data.table(
        Validation_Year =
          validation_year,
        
        Lambda =
          lambda_grid[j],
        
        MSE =
          mse_j,
        
        MAE =
          mae_j,
        
        R2_OOS =
          r2_oos_j,
        
        Pearson =
          pearson_j,
        
        Spearman =
          spearman_j,
        
        N_NonZero =
          n_nonzero_j
      )
    )
  }
}


# ============================================================
# 8. SELECTION DE LAMBDA
# ============================================================


# ------------------------------------------------------------
# 8.1 PERFORMANCE MOYENNE PAR LAMBDA
# ------------------------------------------------------------

lasso_lambda_summary <- lasso_cv_results[
  ,
  .(
    Mean_MSE =
      mean(
        MSE,
        na.rm = TRUE
      ),
    
    Mean_MAE =
      mean(
        MAE,
        na.rm = TRUE
      ),
    
    Mean_R2_OOS =
      mean(
        R2_OOS,
        na.rm = TRUE
      ),
    
    Mean_Pearson =
      mean(
        Pearson,
        na.rm = TRUE
      ),
    
    Mean_Spearman =
      mean(
        Spearman,
        na.rm = TRUE
      ),
    
    Mean_N_NonZero =
      mean(
        N_NonZero,
        na.rm = TRUE
      )
  ),
  by = Lambda
]


# ------------------------------------------------------------
# 8.2 CHOIX SELON MSE DE VALIDATION
# ------------------------------------------------------------

setorder(
  lasso_lambda_summary,
  Mean_MSE
)


lasso_lambda_summary[
  1:10
]


best_lambda <-
  lasso_lambda_summary$Lambda[1]


cat(
  "\nLambda retenu :",
  best_lambda,
  "\n"
)


# ============================================================
# 9. ESTIMATION FINALE DU LASSO ET EVALUATION OUT-OF-SAMPLE
# ============================================================

# Objectif :
#
# - utiliser le lambda optimal choisi avec la validation
#   temporelle expanding-window ;
# - réestimer le modèle LASSO sur toute la période de
#   développement disponible AVANT la période test ;
# - purger décembre 2021 car sa cible correspond au rendement
#   réalisé en janvier 2022 ;
# - standardiser le test uniquement avec les paramètres
#   calculés sur le train ;
# - évaluer le modèle sur la période test 2022-2025.
#
# IMPORTANT :
#
# La validation temporelle utilisée pour sélectionner
# best_lambda est déjà purgée.
#
# On conserve donc best_lambda.
#
# La correction ici concerne uniquement le REFIT FINAL :
# décembre 2021 ne doit pas entrer dans l'entraînement final
# puisque Return_NextMonth de décembre 2021 est réalisé
# en janvier 2022, c'est-à-dire dans la période test.


# ------------------------------------------------------------
# 9.1 PURGE DE LA FRONTIERE DEVELOPPEMENT / TEST
# ------------------------------------------------------------

final_train_last_month <- max(
  lasso_development$MthCalDt
)

lasso_development_final <- lasso_development[
  MthCalDt < final_train_last_month
]


cat(
  "\n========================================\n",
  "PURGE FINALE TRAIN / TEST\n",
  "========================================\n"
)

cat(
  "Dernier mois du development original :",
  as.character(
    max(lasso_development$MthCalDt)
  ),
  "\n"
)

cat(
  "Dernier mois utilise pour le refit final :",
  as.character(
    max(lasso_development_final$MthCalDt)
  ),
  "\n"
)

cat(
  "Observations development originales :",
  nrow(lasso_development),
  "\n"
)

cat(
  "Observations du train final :",
  nrow(lasso_development_final),
  "\n"
)

cat(
  "Observations retirees par la purge :",
  nrow(lasso_development) -
    nrow(lasso_development_final),
  "\n"
)

cat(
  "========================================\n"
)


# Vérification de sécurité :
# le train final doit se terminer avant décembre 2021.

stopifnot(
  max(lasso_development_final$MthCalDt) <
    as.IDate("2021-12-01")
)


# Vérification supplémentaire :
# aucune observation de décembre 2021 ne doit être présente.

stopifnot(
  nrow(
    lasso_development_final[
      format(MthCalDt, "%Y-%m") == "2021-12"
    ]
  ) == 0
)


# ------------------------------------------------------------
# 9.2 CONSTRUCTION DES MATRICES DU TRAIN FINAL
# ------------------------------------------------------------

X_dev <- as.matrix(
  lasso_development_final[
    ,
    ..features_final
  ]
)

y_dev <- lasso_development_final[
  ,
  FF5_Residual_NextMonth
]


# ------------------------------------------------------------
# 9.3 CONSTRUCTION DES MATRICES DU TEST
# ------------------------------------------------------------

X_test <- as.matrix(
  lasso_test[
    ,
    ..features_final
  ]
)

y_test <- lasso_test[
  ,
  FF5_Residual_NextMonth
]


# Vérifications dimensions.

stopifnot(
  nrow(X_dev) ==
    length(y_dev)
)

stopifnot(
  nrow(X_test) ==
    length(y_test)
)

stopifnot(
  ncol(X_dev) ==
    length(features_final)
)

stopifnot(
  ncol(X_test) ==
    length(features_final)
)


# ------------------------------------------------------------
# 9.4 STANDARDISATION A PARTIR DU TRAIN UNIQUEMENT
# ------------------------------------------------------------

# Moyennes du train final.

dev_means <- apply(
  X_dev,
  2,
  mean
)


# Ecarts-types du train final.

dev_sds <- apply(
  X_dev,
  2,
  sd
)


# Vérification :
# les paramètres de standardisation doivent être valides.

stopifnot(
  all(
    is.finite(dev_means)
  )
)

stopifnot(
  all(
    is.finite(dev_sds)
  )
)

stopifnot(
  all(
    dev_sds > 0
  )
)


# Standardisation du train.

X_dev_scaled <- scale(
  X_dev,
  center = dev_means,
  scale = dev_sds
)


# Standardisation du test avec EXACTEMENT
# les paramètres calculés sur le train.

X_test_scaled <- scale(
  X_test,
  center = dev_means,
  scale = dev_sds
)


# Vérification :
# aucune valeur non finie après standardisation.

stopifnot(
  all(
    is.finite(X_dev_scaled)
  )
)

stopifnot(
  all(
    is.finite(X_test_scaled)
  )
)


# ------------------------------------------------------------
# 9.5 ESTIMATION DU LASSO FINAL
# ------------------------------------------------------------

# best_lambda a déjà été sélectionné avec la validation
# temporelle expanding-window purgée.
#
# On ne sélectionne PAS de nouveau lambda avec le test.

lasso_final <- glmnet(
  x = X_dev_scaled,
  y = y_dev,
  alpha = 1,
  lambda = best_lambda,
  standardize = FALSE,
  intercept = TRUE
)


# ------------------------------------------------------------
# 9.6 PREDICTIONS OUT-OF-SAMPLE
# ------------------------------------------------------------

lasso_test_prediction <- as.numeric(
  predict(
    lasso_final,
    newx = X_test_scaled,
    s = best_lambda
  )
)


stopifnot(
  length(lasso_test_prediction) ==
    nrow(lasso_test)
)

stopifnot(
  all(
    is.finite(lasso_test_prediction)
  )
)


# ------------------------------------------------------------
# 9.7 TABLEAU DES RESULTATS DU TEST
# ------------------------------------------------------------

lasso_test_results <- copy(
  lasso_test
)

lasso_test_results[
  ,
  Prediction_LASSO :=
    lasso_test_prediction
]


# ------------------------------------------------------------
# 9.8 RMSE
# ------------------------------------------------------------

lasso_test_rmse <- sqrt(
  mean(
    (
      lasso_test_results$FF5_Residual_NextMonth -
        lasso_test_results$Prediction_LASSO
    )^2
  )
)


# ------------------------------------------------------------
# 9.9 MAE
# ------------------------------------------------------------

lasso_test_mae <- mean(
  abs(
    lasso_test_results$FF5_Residual_NextMonth -
      lasso_test_results$Prediction_LASSO
  )
)


# ------------------------------------------------------------
# 9.10 R2 OUT-OF-SAMPLE
# ------------------------------------------------------------

# Benchmark :
# prévision constante égale à la moyenne de la cible
# du TRAIN FINAL PURGE.

benchmark_mean <- mean(
  y_dev
)


benchmark_mse <- mean(
  (
    y_test -
      benchmark_mean
  )^2
)


model_mse <- mean(
  (
    y_test -
      lasso_test_prediction
  )^2
)


lasso_test_r2_oos <-
  1 -
  model_mse /
  benchmark_mse


# ------------------------------------------------------------
# 9.11 CORRELATION DE PEARSON
# ------------------------------------------------------------

lasso_test_pearson <- cor(
  y_test,
  lasso_test_prediction,
  method = "pearson"
)


# ------------------------------------------------------------
# 9.12 CORRELATION DE SPEARMAN
# ------------------------------------------------------------

lasso_test_spearman <- cor(
  y_test,
  lasso_test_prediction,
  method = "spearman"
)


# ------------------------------------------------------------
# 9.13 COEFFICIENTS DU MODELE FINAL
# ------------------------------------------------------------

lasso_coef <- as.matrix(
  coef(
    lasso_final,
    s = best_lambda
  )
)


lasso_selected <- data.table(
  Variable = rownames(lasso_coef),
  Coefficient = as.numeric(
    lasso_coef[, 1]
  )
)[
  Variable != "(Intercept)" &
    Coefficient != 0
]


# Trier les variables sélectionnées par coefficient absolu.

lasso_selected[
  ,
  Abs_Coefficient :=
    abs(Coefficient)
]

setorder(
  lasso_selected,
  -Abs_Coefficient
)


# ------------------------------------------------------------
# 9.14 RESUME DU MODELE FINAL
# ------------------------------------------------------------

lasso_test_summary <- data.table(
  Model = "LASSO",
  RMSE = lasso_test_rmse,
  MAE = lasso_test_mae,
  R2_OOS = lasso_test_r2_oos,
  Pearson = lasso_test_pearson,
  Spearman = lasso_test_spearman,
  Lambda = best_lambda,
  N_Selected = nrow(lasso_selected),
  N_Test = nrow(lasso_test_results)
)


# ------------------------------------------------------------
# 9.15 AFFICHAGE FINAL
# ------------------------------------------------------------

cat(
  "\n========================================\n",
  "LASSO FINAL - TEST 2022-2025\n",
  "========================================\n"
)

cat(
  "Train final jusqu'au :",
  as.character(
    max(lasso_development_final$MthCalDt)
  ),
  "\n"
)

cat(
  "Test a partir du :",
  as.character(
    min(lasso_test$MthCalDt)
  ),
  "\n"
)

cat(
  "Best lambda :",
  best_lambda,
  "\n"
)

cat(
  "Nombre de variables selectionnees :",
  nrow(lasso_selected),
  "/",
  length(features_final),
  "\n"
)

cat(
  "Nombre d'observations test :",
  nrow(lasso_test_results),
  "\n"
)

cat(
  "========================================\n\n"
)


print(
  lasso_test_summary
)


cat(
  "\nVariables selectionnees :\n"
)

print(
  lasso_selected
)


# ------------------------------------------------------------
# 9.16 VERIFICATIONS FINALES ANTI-LOOK-AHEAD
# ------------------------------------------------------------

# Le dernier mois utilisé pour l'entraînement doit être
# strictement antérieur à décembre 2021.

stopifnot(
  max(lasso_development_final$MthCalDt) <=
    as.IDate("2021-11-30")
)


# Le test doit commencer en 2022.

stopifnot(
  min(lasso_test$MthCalDt) >=
    as.IDate("2022-01-01")
)


cat(
  "\nPASS : refit final LASSO purge correctement.\n"
)

cat(
  "Aucune cible realisee en janvier 2022 n'est utilisee",
  "pour entrainer le LASSO final.\n"
)

# ============================================================
# 10. VARIABLES RETENUES PAR LE LASSO
# ============================================================

lasso_coef <- as.matrix(
  coef(
    lasso_final,
    s = best_lambda
  )
)


lasso_selected <- data.table(
  Variable =
    rownames(lasso_coef),
  
  Coefficient =
    as.numeric(
      lasso_coef[
        ,
        1
      ]
    )
)


lasso_selected <- lasso_selected[
  Variable != "(Intercept)" &
    Coefficient != 0
]


lasso_selected[
  ,
  Abs_Coefficient :=
    abs(Coefficient)
]


setorder(
  lasso_selected,
  -Abs_Coefficient
)


lasso_selected


cat(
  "\nNombre de variables retenues :",
  nrow(lasso_selected),
  "/ 13\n"
)


# ============================================================
# 11. EVALUATION FINALE SUR 2022-2025
# ============================================================

# IMPORTANT :
#
# C'est la première fois que le test final est utilisé.


lasso_test_prediction <- as.numeric(
  predict(
    lasso_final,
    newx = X_test_scaled,
    s = best_lambda
  )
)


# ------------------------------------------------------------
# 11.1 RMSE
# ------------------------------------------------------------

lasso_test_rmse <- sqrt(
  mean(
    (
      y_test -
        lasso_test_prediction
    )^2
  )
)


# ------------------------------------------------------------
# 11.2 MAE
# ------------------------------------------------------------

lasso_test_mae <- mean(
  abs(
    y_test -
      lasso_test_prediction
  )
)


# ------------------------------------------------------------
# 11.3 R2 HORS ECHANTILLON
# ------------------------------------------------------------

benchmark_test <-
  mean(y_dev)


lasso_test_r2_oos <-
  1 -
  sum(
    (
      y_test -
        lasso_test_prediction
    )^2
  ) /
  sum(
    (
      y_test -
        benchmark_test
    )^2
  )


# ------------------------------------------------------------
# 11.4 CORRELATIONS
# ------------------------------------------------------------

lasso_test_pearson <- cor(
  y_test,
  lasso_test_prediction,
  method = "pearson"
)


lasso_test_spearman <- cor(
  y_test,
  lasso_test_prediction,
  method = "spearman"
)


# ------------------------------------------------------------
# 11.5 TABLEAU FINAL
# ------------------------------------------------------------

lasso_test_results <- data.table(
  RMSE =
    lasso_test_rmse,
  
  MAE =
    lasso_test_mae,
  
  R2_OOS =
    lasso_test_r2_oos,
  
  Pearson =
    lasso_test_pearson,
  
  Spearman =
    lasso_test_spearman,
  
  Lambda =
    best_lambda,
  
  N_Selected =
    nrow(lasso_selected),
  
  N_Test =
    length(y_test)
)


lasso_test_results


# ============================================================
# 12. PCA DES 13 CARACTERISTIQUES
# ============================================================

# Objectif :
#
# Réduire les 13 caractéristiques en composantes principales,
# puis utiliser ces composantes pour prédire :
#
#     FF5_Residual_NextMonth
#
# IMPORTANT :
#
# - PCA uniquement sur les 13 caractéristiques
# - PCA estimée uniquement sur le TRAIN de chaque fold
# - aucune information de validation dans la PCA
# - aucune information du test 2022-2025
# - même protocole temporel que pour le LASSO


# ------------------------------------------------------------
# 12.1 NOMBRE DE COMPOSANTES A TESTER
# ------------------------------------------------------------

pca_k_grid <- 1:length(features_final)

pca_k_grid


# ------------------------------------------------------------
# 12.2 TABLE DES RESULTATS
# ------------------------------------------------------------

pca_cv_results <- data.table()


# ============================================================
# 13. VALIDATION TEMPORELLE PCA
# ============================================================

validation_years_pca <- 2011:2021


for (validation_year in validation_years_pca) {
  
  cat(
    "\nValidation PCA :",
    validation_year,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 13.1 TRAIN
  # ----------------------------------------------------------
  
  fold_train <- lasso_development[
    Year < validation_year
  ]
  
  
  # ----------------------------------------------------------
  # 13.2 PURGE DU DERNIER MOIS
  # ----------------------------------------------------------
  
  last_train_month <- max(
    fold_train$MthCalDt
  )
  
  
  fold_train <- fold_train[
    MthCalDt < last_train_month
  ]
  
  
  # ----------------------------------------------------------
  # 13.3 VALIDATION
  # ----------------------------------------------------------
  
  fold_validation <- lasso_development[
    Year == validation_year
  ]
  
  
  # ----------------------------------------------------------
  # 13.4 MATRICES
  # ----------------------------------------------------------
  
  X_train <- as.matrix(
    fold_train[
      ,
      ..features_final
    ]
  )
  
  
  y_train <-
    fold_train$FF5_Residual_NextMonth
  
  
  X_validation <- as.matrix(
    fold_validation[
      ,
      ..features_final
    ]
  )
  
  
  y_validation <-
    fold_validation$FF5_Residual_NextMonth
  
  
  # ----------------------------------------------------------
  # 13.5 PCA ESTIMEE UNIQUEMENT SUR LE TRAIN
  # ----------------------------------------------------------
  
  pca_fit <- prcomp(
    X_train,
    center = TRUE,
    scale. = TRUE
  )
  
  
  # Scores PCA du train
  
  PC_train <- pca_fit$x
  
  
  # Projection de la validation sur la PCA DU TRAIN
  
  PC_validation <- predict(
    pca_fit,
    newdata = X_validation
  )
  
  
  # ----------------------------------------------------------
  # 13.6 VARIANCE EXPLIQUEE
  # ----------------------------------------------------------
  
  variance_ratio <-
    pca_fit$sdev^2 /
    sum(
      pca_fit$sdev^2
    )
  
  
  cumulative_variance <-
    cumsum(
      variance_ratio
    )
  
  
  # ----------------------------------------------------------
  # 13.7 TEST DE k = 1,...,13 COMPOSANTES
  # ----------------------------------------------------------
  
  for (k in pca_k_grid) {
    
    # Régression linéaire sur les k premières PC
    
    train_pc_df <- data.frame(
      y = y_train,
      PC_train[
        ,
        1:k,
        drop = FALSE
      ]
    )
    
    
    pca_lm <- lm(
      y ~ .,
      data = train_pc_df
    )
    
    
    validation_pc_df <- data.frame(
      PC_validation[
        ,
        1:k,
        drop = FALSE
      ]
    )
    
    
    pred_pca <- as.numeric(
      predict(
        pca_lm,
        newdata = validation_pc_df
      )
    )
    
    
    # --------------------------------------------------------
    # MSE
    # --------------------------------------------------------
    
    mse_pca <- mean(
      (
        y_validation -
          pred_pca
      )^2
    )
    
    
    # --------------------------------------------------------
    # MAE
    # --------------------------------------------------------
    
    mae_pca <- mean(
      abs(
        y_validation -
          pred_pca
      )
    )
    
    
    # --------------------------------------------------------
    # R2 HORS ECHANTILLON
    # --------------------------------------------------------
    
    benchmark <-
      mean(y_train)
    
    
    sse_model <- sum(
      (
        y_validation -
          pred_pca
      )^2
    )
    
    
    sse_benchmark <- sum(
      (
        y_validation -
          benchmark
      )^2
    )
    
    
    r2_oos_pca <-
      1 -
      sse_model /
      sse_benchmark
    
    
    # --------------------------------------------------------
    # CORRELATIONS
    # --------------------------------------------------------
    
    pearson_pca <- suppressWarnings(
      cor(
        y_validation,
        pred_pca,
        method = "pearson"
      )
    )
    
    
    spearman_pca <- suppressWarnings(
      cor(
        y_validation,
        pred_pca,
        method = "spearman"
      )
    )
    
    
    # --------------------------------------------------------
    # STOCKAGE
    # --------------------------------------------------------
    
    pca_cv_results <- rbind(
      pca_cv_results,
      
      data.table(
        Validation_Year =
          validation_year,
        
        N_PC =
          k,
        
        Cumulative_Variance =
          cumulative_variance[k],
        
        MSE =
          mse_pca,
        
        MAE =
          mae_pca,
        
        R2_OOS =
          r2_oos_pca,
        
        Pearson =
          pearson_pca,
        
        Spearman =
          spearman_pca
      )
    )
  }
}


# ============================================================
# 14. SELECTION DU NOMBRE DE COMPOSANTES
# ============================================================


# ------------------------------------------------------------
# 14.1 PERFORMANCE MOYENNE
# ------------------------------------------------------------

pca_summary <- pca_cv_results[
  ,
  .(
    Mean_Cumulative_Variance =
      mean(
        Cumulative_Variance,
        na.rm = TRUE
      ),
    
    Mean_MSE =
      mean(
        MSE,
        na.rm = TRUE
      ),
    
    Mean_MAE =
      mean(
        MAE,
        na.rm = TRUE
      ),
    
    Mean_R2_OOS =
      mean(
        R2_OOS,
        na.rm = TRUE
      ),
    
    Mean_Pearson =
      mean(
        Pearson,
        na.rm = TRUE
      ),
    
    Mean_Spearman =
      mean(
        Spearman,
        na.rm = TRUE
      )
  ),
  by = N_PC
]


# ------------------------------------------------------------
# 14.2 CLASSEMENT SELON MSE
# ------------------------------------------------------------

setorder(
  pca_summary,
  Mean_MSE
)


pca_summary[]


best_k <-
  pca_summary$N_PC[1]


cat(
  "\nNombre de composantes retenu :",
  best_k,
  "\n"
)

# ============================================================
# 15. PCA FINALE SUR DEVELOPMENT PURGE
# ============================================================

# Objectif :
#
# - best_k a déjà été choisi avec la validation temporelle ;
# - on réestime maintenant la PCA sur tout le development
#   disponible AVANT la période test ;
# - décembre 2021 est retiré car sa cible est réalisée
#   en janvier 2022 ;
# - le test 2022-2025 reste totalement hors entraînement.


# ------------------------------------------------------------
# 15.1 PURGE DE LA FRONTIERE TRAIN / TEST
# ------------------------------------------------------------

# On réutilise exactement la même période finale purgée
# que pour le LASSO.

pca_development_final <- lasso_development[
  MthCalDt <
    max(lasso_development$MthCalDt)
]


cat(
  "\n========================================\n",
  "PURGE FINALE PCA\n",
  "========================================\n"
)

cat(
  "Dernier mois development original :",
  as.character(
    max(lasso_development$MthCalDt)
  ),
  "\n"
)

cat(
  "Dernier mois utilise pour PCA finale :",
  as.character(
    max(pca_development_final$MthCalDt)
  ),
  "\n"
)

cat(
  "Observations retirees :",
  nrow(lasso_development) -
    nrow(pca_development_final),
  "\n"
)

cat(
  "========================================\n"
)


# Vérifications anti-look-ahead.

stopifnot(
  max(pca_development_final$MthCalDt) <=
    as.IDate("2021-11-30")
)

stopifnot(
  nrow(
    pca_development_final[
      format(MthCalDt, "%Y-%m") == "2021-12"
    ]
  ) == 0
)


# ------------------------------------------------------------
# 15.2 MATRICES
# ------------------------------------------------------------

X_dev_pca <- as.matrix(
  pca_development_final[
    ,
    ..features_final
  ]
)

y_dev_pca <-
  pca_development_final$FF5_Residual_NextMonth


X_test_pca <- as.matrix(
  lasso_test[
    ,
    ..features_final
  ]
)

y_test_pca <-
  lasso_test$FF5_Residual_NextMonth


# Vérifications dimensions.

stopifnot(
  nrow(X_dev_pca) ==
    length(y_dev_pca)
)

stopifnot(
  nrow(X_test_pca) ==
    length(y_test_pca)
)

stopifnot(
  ncol(X_dev_pca) ==
    length(features_final)
)

stopifnot(
  ncol(X_test_pca) ==
    length(features_final)
)


# Vérification des valeurs.

stopifnot(
  all(
    is.finite(X_dev_pca)
  )
)

stopifnot(
  all(
    is.finite(X_test_pca)
  )
)

stopifnot(
  all(
    is.finite(y_dev_pca)
  )
)

stopifnot(
  all(
    is.finite(y_test_pca)
  )
)


# ------------------------------------------------------------
# 15.3 ESTIMATION PCA
# ------------------------------------------------------------

# IMPORTANT :
#
# prcomp calcule le centrage et la standardisation
# UNIQUEMENT à partir du development purgé.
#
# Le test n'intervient donc pas dans l'estimation
# des composantes principales.

pca_final <- prcomp(
  X_dev_pca,
  center = TRUE,
  scale. = TRUE
)


# ------------------------------------------------------------
# 15.4 VARIANCE EXPLIQUEE
# ------------------------------------------------------------

pca_variance <-
  pca_final$sdev^2 /
  sum(
    pca_final$sdev^2
  )


pca_variance_table <- data.table(
  
  PC =
    paste0(
      "PC",
      seq_along(pca_variance)
    ),
  
  Variance_Explained =
    pca_variance,
  
  Cumulative_Variance =
    cumsum(
      pca_variance
    )
)


print(
  pca_variance_table
)


# ------------------------------------------------------------
# 15.5 SCORES PCA
# ------------------------------------------------------------

# Scores du development.

PC_dev_final <-
  pca_final$x


# Projection du test sur les composantes apprises
# uniquement avec le development.

PC_test_final <- predict(
  pca_final,
  newdata = X_test_pca
)


# Vérifications.

stopifnot(
  nrow(PC_dev_final) ==
    nrow(pca_development_final)
)

stopifnot(
  nrow(PC_test_final) ==
    nrow(lasso_test)
)


stopifnot(
  all(
    is.finite(PC_dev_final)
  )
)

stopifnot(
  all(
    is.finite(PC_test_final)
  )
)


# ------------------------------------------------------------
# 15.6 CONTROLE FINAL
# ------------------------------------------------------------

cat(
  "\nPCA finale :",
  best_k,
  "composante(s) retenue(s).\n"
)

cat(
  "Train PCA jusqu'au :",
  as.character(
    max(pca_development_final$MthCalDt)
  ),
  "\n"
)

cat(
  "Test PCA a partir du :",
  as.character(
    min(lasso_test$MthCalDt)
  ),
  "\n"
)

cat(
  "PASS : PCA finale purgee correctement.\n"
)

cat(
  "Aucune cible realisee en janvier 2022",
  "n'est utilisee pour estimer la PCA finale.\n"
)


# ============================================================
# 16. REGRESSION FINALE SUR LES COMPOSANTES
# ============================================================

pca_train_df <- data.frame(
  y = y_dev_pca,
  
  PC_dev_final[
    ,
    1:best_k,
    drop = FALSE
  ]
)


pca_final_lm <- lm(
  y ~ .,
  data = pca_train_df
)


# ============================================================
# 17. PREDICTION PCA SUR LE TEST
# ============================================================

pca_test_df <- data.frame(
  PC_test_final[
    ,
    1:best_k,
    drop = FALSE
  ]
)


pca_test_prediction <- as.numeric(
  predict(
    pca_final_lm,
    newdata = pca_test_df
  )
)


# ============================================================
# 18. EVALUATION PCA SUR LE TEST
# ============================================================


# ------------------------------------------------------------
# 18.1 RMSE
# ------------------------------------------------------------

pca_test_rmse <- sqrt(
  mean(
    (
      y_test_pca -
        pca_test_prediction
    )^2
  )
)


# ------------------------------------------------------------
# 18.2 MAE
# ------------------------------------------------------------

pca_test_mae <- mean(
  abs(
    y_test_pca -
      pca_test_prediction
  )
)


# ------------------------------------------------------------
# 18.3 R2 HORS ECHANTILLON
# ------------------------------------------------------------

benchmark_pca <-
  mean(y_dev_pca)


pca_test_r2_oos <-
  1 -
  sum(
    (
      y_test_pca -
        pca_test_prediction
    )^2
  ) /
  sum(
    (
      y_test_pca -
        benchmark_pca
    )^2
  )


# ------------------------------------------------------------
# 18.4 CORRELATIONS
# ------------------------------------------------------------

pca_test_pearson <- cor(
  y_test_pca,
  pca_test_prediction,
  method = "pearson"
)


pca_test_spearman <- cor(
  y_test_pca,
  pca_test_prediction,
  method = "spearman"
)


# ------------------------------------------------------------
# 18.5 RESULTATS
# ------------------------------------------------------------

pca_test_results <- data.table(
  RMSE =
    pca_test_rmse,
  
  MAE =
    pca_test_mae,
  
  R2_OOS =
    pca_test_r2_oos,
  
  Pearson =
    pca_test_pearson,
  
  Spearman =
    pca_test_spearman,
  
  N_PC =
    best_k,
  
  N_Test =
    length(y_test_pca)
)


pca_test_results

# ============================================================
# 19. COMPARAISON PCA VS LASSO
# ============================================================

comparison_models <- rbind(
  
  data.table(
    Model = "LASSO",
    
    RMSE =
      lasso_test_rmse,
    
    MAE =
      lasso_test_mae,
    
    R2_OOS =
      lasso_test_r2_oos,
    
    Pearson =
      lasso_test_pearson,
    
    Spearman =
      lasso_test_spearman
  ),
  
  data.table(
    Model = "PCA + OLS",
    
    RMSE =
      pca_test_rmse,
    
    MAE =
      pca_test_mae,
    
    R2_OOS =
      pca_test_r2_oos,
    
    Pearson =
      pca_test_pearson,
    
    Spearman =
      pca_test_spearman
  )
)


comparison_models


pca_summary[]
pca_variance_table
comparison_models


# ============================================================
# 20. CLASSEMENT DES ACTIONS AVEC LE LASSO
# ============================================================

# Objectif :
#
# Vérifier si le score prédit par le LASSO permet de
# classer correctement les actions.
#
# Chaque mois :
#
# Q1 = 20 % des actions avec les scores prédits les plus faibles
# Q5 = 20 % des actions avec les scores prédits les plus élevés
#
# On étudie ensuite les performances réellement observées
# au mois suivant.


# ------------------------------------------------------------
# 20.1 AJOUT DES PREDICTIONS AU TEST
# ------------------------------------------------------------

stopifnot(
  nrow(lasso_test) ==
    length(lasso_test_prediction)
)


lasso_rank_test <- copy(
  lasso_test
)


lasso_rank_test[
  ,
  Predicted_Residual :=
    lasso_test_prediction
]


# ------------------------------------------------------------
# 20.2 CONTROLE
# ------------------------------------------------------------

lasso_rank_test[
  ,
  .(
    N = .N,
    
    Missing_Prediction =
      sum(
        is.na(Predicted_Residual)
      ),
    
    Missing_Residual =
      sum(
        is.na(FF5_Residual_NextMonth)
      ),
    
    Missing_Return =
      sum(
        is.na(Return_NextMonth)
      )
  )
]


# ============================================================
# 21. CONSTRUCTION DES QUINTILES MENSUELS
# ============================================================

# IMPORTANT :
#
# Le classement est effectué MOIS PAR MOIS.
#
# On ne compare donc pas directement une action de 2022
# avec une action de 2025.


lasso_rank_test[
  ,
  Predicted_Rank :=
    frank(
      Predicted_Residual,
      ties.method = "average"
    ),
  by = MthCalDt
]


lasso_rank_test[
  ,
  Predicted_Percentile :=
    Predicted_Rank /
    .N,
  by = MthCalDt
]


lasso_rank_test[
  ,
  Predicted_Quintile :=
    pmin(
      5L,
      ceiling(
        5 *
          Predicted_Percentile
      )
    ),
  by = MthCalDt
]


# ------------------------------------------------------------
# 21.1 CONTROLE DES QUINTILES
# ------------------------------------------------------------

quintile_counts <- lasso_rank_test[
  ,
  .N,
  by = .(
    MthCalDt,
    Predicted_Quintile
  )
]


quintile_counts[
  order(
    MthCalDt,
    Predicted_Quintile
  )
][1:25]


# ============================================================
# 22. PERFORMANCE REALISEE PAR QUINTILE
# ============================================================

quintile_performance <- lasso_rank_test[
  ,
  .(
    N =
      .N,
    
    Mean_Predicted_Residual =
      mean(
        Predicted_Residual
      ),
    
    Mean_Realized_Residual =
      mean(
        FF5_Residual_NextMonth
      ),
    
    Median_Realized_Residual =
      median(
        FF5_Residual_NextMonth
      ),
    
    Mean_Return_NextMonth =
      mean(
        Return_NextMonth
      ),
    
    Median_Return_NextMonth =
      median(
        Return_NextMonth
      ),
    
    Positive_Residual_Rate =
      mean(
        FF5_Residual_NextMonth > 0
      ),
    
    Positive_Return_Rate =
      mean(
        Return_NextMonth > 0
      )
  ),
  by = Predicted_Quintile
]


setorder(
  quintile_performance,
  Predicted_Quintile
)


quintile_performance


# ============================================================
# 23. TOP 20 % VS BOTTOM 20 %
# ============================================================

top20 <- lasso_rank_test[
  Predicted_Quintile == 5
]


bottom20 <- lasso_rank_test[
  Predicted_Quintile == 1
]


top_bottom_summary <- data.table(
  
  Group = c(
    "Bottom 20%",
    "Top 20%"
  ),
  
  N = c(
    nrow(bottom20),
    nrow(top20)
  ),
  
  Mean_Residual = c(
    mean(
      bottom20$FF5_Residual_NextMonth
    ),
    
    mean(
      top20$FF5_Residual_NextMonth
    )
  ),
  
  Mean_Return = c(
    mean(
      bottom20$Return_NextMonth
    ),
    
    mean(
      top20$Return_NextMonth
    )
  ),
  
  Positive_Residual_Rate = c(
    mean(
      bottom20$FF5_Residual_NextMonth > 0
    ),
    
    mean(
      top20$FF5_Residual_NextMonth > 0
    )
  ),
  
  Positive_Return_Rate = c(
    mean(
      bottom20$Return_NextMonth > 0
    ),
    
    mean(
      top20$Return_NextMonth > 0
    )
  )
)


top_bottom_summary


# ============================================================
# 24. SPREAD TOP - BOTTOM MOIS PAR MOIS
# ============================================================

# C'est un test particulièrement important.
#
# Pour chaque mois :
#
# rendement moyen Q5
#       -
# rendement moyen Q1


monthly_top_bottom <- lasso_rank_test[
  Predicted_Quintile %in% c(
    1L,
    5L
  ),
  .(
    Mean_Residual =
      mean(
        FF5_Residual_NextMonth
      ),
    
    Mean_Return =
      mean(
        Return_NextMonth
      )
  ),
  by = .(
    MthCalDt,
    Predicted_Quintile
  )
]


monthly_spread <- dcast(
  monthly_top_bottom,
  MthCalDt ~ Predicted_Quintile,
  value.var = c(
    "Mean_Residual",
    "Mean_Return"
  )
)


monthly_spread[
  ,
  Residual_Spread :=
    Mean_Residual_5 -
    Mean_Residual_1
]


monthly_spread[
  ,
  Return_Spread :=
    Mean_Return_5 -
    Mean_Return_1
]


# ------------------------------------------------------------
# 24.1 RESULTATS DU SPREAD
# ------------------------------------------------------------

spread_summary <- monthly_spread[
  ,
  .(
    N_Months =
      .N,
    
    Mean_Residual_Spread =
      mean(
        Residual_Spread
      ),
    
    Median_Residual_Spread =
      median(
        Residual_Spread
      ),
    
    Positive_Residual_Spread_Rate =
      mean(
        Residual_Spread > 0
      ),
    
    Mean_Return_Spread =
      mean(
        Return_Spread
      ),
    
    Median_Return_Spread =
      median(
        Return_Spread
      ),
    
    Positive_Return_Spread_Rate =
      mean(
        Return_Spread > 0
      )
  )
]


spread_summary


# ============================================================
# 25. PERFORMANCE PAR ANNEE
# ============================================================

lasso_rank_test[
  ,
  Test_Year :=
    as.integer(
      format(
        MthCalDt,
        "%Y"
      )
    )
]


yearly_rank_performance <- lasso_rank_test[
  Predicted_Quintile %in% c(
    1L,
    5L
  ),
  .(
    Mean_Residual =
      mean(
        FF5_Residual_NextMonth
      ),
    
    Mean_Return =
      mean(
        Return_NextMonth
      )
  ),
  by = .(
    Test_Year,
    Predicted_Quintile
  )
]


yearly_rank_spread <- dcast(
  yearly_rank_performance,
  Test_Year ~ Predicted_Quintile,
  value.var = c(
    "Mean_Residual",
    "Mean_Return"
  )
)


yearly_rank_spread[
  ,
  Residual_Spread :=
    Mean_Residual_5 -
    Mean_Residual_1
]


yearly_rank_spread[
  ,
  Return_Spread :=
    Mean_Return_5 -
    Mean_Return_1
]


yearly_rank_spread


# ============================================================
# 26. PRECISION DIRECTIONNELLE
# ============================================================

# Question :
#
# Quand le modèle prédit un résidu positif,
# le résidu réalisé est-il effectivement positif ?


lasso_rank_test[
  ,
  Predicted_Direction :=
    Predicted_Residual > 0
]


lasso_rank_test[
  ,
  Realized_Direction :=
    FF5_Residual_NextMonth > 0
]


directional_accuracy <- lasso_rank_test[
  ,
  .(
    Accuracy =
      mean(
        Predicted_Direction ==
          Realized_Direction
      ),
    
    Predicted_Positive_Rate =
      mean(
        Predicted_Direction
      ),
    
    Realized_Positive_Rate =
      mean(
        Realized_Direction
      )
  )
]


directional_accuracy


# ============================================================
# 27. CORRELATION DE RANG MOIS PAR MOIS
# ============================================================

# Le Spearman global que nous avions calculé mélangeait
# toutes les observations du test.
#
# Pour une stratégie de sélection d'actions, il est très
# intéressant de mesurer également la corrélation
# cross-sectionnelle CHAQUE MOIS.


monthly_rank_correlation <- lasso_rank_test[
  ,
  .(
    Spearman =
      suppressWarnings(
        cor(
          Predicted_Residual,
          FF5_Residual_NextMonth,
          method = "spearman"
        )
      ),
    
    N_Stocks =
      .N
  ),
  by = MthCalDt
]


monthly_rank_summary <- monthly_rank_correlation[
  ,
  .(
    N_Months =
      .N,
    
    Mean_Monthly_Spearman =
      mean(
        Spearman,
        na.rm = TRUE
      ),
    
    Median_Monthly_Spearman =
      median(
        Spearman,
        na.rm = TRUE
      ),
    
    Positive_Spearman_Rate =
      mean(
        Spearman > 0,
        na.rm = TRUE
      )
  )
]


monthly_rank_summary


# ============================================================
# 28. RESUME DES RESULTATS DE CLASSEMENT
# ============================================================

cat(
  "\n==============================\n"
)

cat(
  "PERFORMANCE PAR QUINTILE\n"
)

cat(
  "==============================\n"
)

print(
  quintile_performance
)


cat(
  "\n==============================\n"
)

cat(
  "TOP 20 % VS BOTTOM 20 %\n"
)

cat(
  "==============================\n"
)

print(
  top_bottom_summary
)


cat(
  "\n==============================\n"
)

cat(
  "SPREAD TOP - BOTTOM\n"
)

cat(
  "==============================\n"
)

print(
  spread_summary
)


cat(
  "\n==============================\n"
)

cat(
  "PERFORMANCE PAR ANNEE\n"
)

cat(
  "==============================\n"
)

print(
  yearly_rank_spread
)


cat(
  "\n==============================\n"
)

cat(
  "PRECISION DIRECTIONNELLE\n"
)

cat(
  "==============================\n"
)

print(
  directional_accuracy
)


cat(
  "\n==============================\n"
)

cat(
  "SPEARMAN MENSUEL\n"
)

cat(
  "==============================\n"
)

print(
  monthly_rank_summary
)

# ============================================================
# 29. SIGNIFICATIVITE STATISTIQUE DES SPREADS
# ============================================================

# Objectif :
#
# Tester si les spreads mensuels Q5 - Q1 sont
# statistiquement différents de zéro.
#
# Deux variables :
#
# 1. Residual_Spread
# 2. Return_Spread
#
# Les erreurs standards Newey-West permettent de tenir compte
# de l'hétéroscédasticité et de l'autocorrélation éventuelle.


# ------------------------------------------------------------
# 29.1 PACKAGES
# ------------------------------------------------------------

library(lmtest)
library(sandwich)


# ------------------------------------------------------------
# 29.2 ORDRE TEMPOREL
# ------------------------------------------------------------

setorder(
  monthly_spread,
  MthCalDt
)


stopifnot(
  nrow(monthly_spread) == 47
)


# ============================================================
# 30. TEST DU SPREAD RESIDUEL
# ============================================================


# ------------------------------------------------------------
# 30.1 REGRESSION SUR CONSTANTE
# ------------------------------------------------------------

residual_spread_model <- lm(
  Residual_Spread ~ 1,
  data = monthly_spread
)


# ------------------------------------------------------------
# 30.2 ERREURS STANDARDS NEWEY-WEST
# ------------------------------------------------------------

residual_nw_vcov <- NeweyWest(
  residual_spread_model,
  lag = 3,
  prewhite = FALSE,
  adjust = TRUE
)


residual_nw_test <- coeftest(
  residual_spread_model,
  vcov. = residual_nw_vcov
)


residual_nw_test


# ============================================================
# 31. TEST DU SPREAD DE RENDEMENT
# ============================================================


# ------------------------------------------------------------
# 31.1 REGRESSION SUR CONSTANTE
# ------------------------------------------------------------

return_spread_model <- lm(
  Return_Spread ~ 1,
  data = monthly_spread
)


# ------------------------------------------------------------
# 31.2 ERREURS STANDARDS NEWEY-WEST
# ------------------------------------------------------------

return_nw_vcov <- NeweyWest(
  return_spread_model,
  lag = 3,
  prewhite = FALSE,
  adjust = TRUE
)


return_nw_test <- coeftest(
  return_spread_model,
  vcov. = return_nw_vcov
)


return_nw_test


# ============================================================
# 32. EXTRACTION DES RESULTATS
# ============================================================


# ------------------------------------------------------------
# 32.1 SPREAD RESIDUEL
# ------------------------------------------------------------

residual_mean <-
  coef(
    residual_spread_model
  )[1]


residual_se_nw <-
  sqrt(
    residual_nw_vcov[1, 1]
  )


residual_t_nw <-
  residual_mean /
  residual_se_nw


residual_p_nw <-
  2 *
  pt(
    -abs(residual_t_nw),
    df = nrow(monthly_spread) - 1
  )


residual_ci_low <-
  residual_mean -
  qt(
    0.975,
    df = nrow(monthly_spread) - 1
  ) *
  residual_se_nw


residual_ci_high <-
  residual_mean +
  qt(
    0.975,
    df = nrow(monthly_spread) - 1
  ) *
  residual_se_nw


# ------------------------------------------------------------
# 32.2 SPREAD DE RENDEMENT
# ------------------------------------------------------------

return_mean <-
  coef(
    return_spread_model
  )[1]


return_se_nw <-
  sqrt(
    return_nw_vcov[1, 1]
  )


return_t_nw <-
  return_mean /
  return_se_nw


return_p_nw <-
  2 *
  pt(
    -abs(return_t_nw),
    df = nrow(monthly_spread) - 1
  )


return_ci_low <-
  return_mean -
  qt(
    0.975,
    df = nrow(monthly_spread) - 1
  ) *
  return_se_nw


return_ci_high <-
  return_mean +
  qt(
    0.975,
    df = nrow(monthly_spread) - 1
  ) *
  return_se_nw


# ============================================================
# 33. TABLEAU FINAL DES TESTS
# ============================================================

spread_significance <- data.table(
  
  Spread = c(
    "FF5 Residual Q5-Q1",
    "Raw Return Q5-Q1"
  ),
  
  Mean_Monthly = c(
    residual_mean,
    return_mean
  ),
  
  Mean_Monthly_Percent = c(
    residual_mean,
    return_mean
  ) * 100,
  
  NW_Standard_Error = c(
    residual_se_nw,
    return_se_nw
  ),
  
  NW_t_stat = c(
    residual_t_nw,
    return_t_nw
  ),
  
  NW_p_value = c(
    residual_p_nw,
    return_p_nw
  ),
  
  CI95_Lower = c(
    residual_ci_low,
    return_ci_low
  ),
  
  CI95_Upper = c(
    residual_ci_high,
    return_ci_high
  )
)


spread_significance


# ============================================================
# 34. ROBUSTESSE AU CHOIX DU LAG NEWEY-WEST
# ============================================================

# On vérifie que la conclusion ne dépend pas uniquement
# du choix lag = 3.


nw_lags <- c(
  0,
  1,
  3,
  6
)


nw_robustness <- rbindlist(
  lapply(
    nw_lags,
    function(L) {
      
      vcov_res <- NeweyWest(
        residual_spread_model,
        lag = L,
        prewhite = FALSE,
        adjust = TRUE
      )
      
      
      vcov_ret <- NeweyWest(
        return_spread_model,
        lag = L,
        prewhite = FALSE,
        adjust = TRUE
      )
      
      
      se_res <-
        sqrt(
          vcov_res[1, 1]
        )
      
      
      se_ret <-
        sqrt(
          vcov_ret[1, 1]
        )
      
      
      t_res <-
        residual_mean /
        se_res
      
      
      t_ret <-
        return_mean /
        se_ret
      
      
      data.table(
        
        Lag = L,
        
        Residual_t =
          t_res,
        
        Residual_p =
          2 *
          pt(
            -abs(t_res),
            df = nrow(monthly_spread) - 1
          ),
        
        Return_t =
          t_ret,
        
        Return_p =
          2 *
          pt(
            -abs(t_ret),
            df = nrow(monthly_spread) - 1
          )
      )
    }
  )
)


nw_robustness


# ============================================================
# 35. CLASSIFICATION DE LA HAUSSE DU RENDEMENT
# ============================================================

# Objectif :
#
# Prédire si le rendement BRUT de l'action au mois suivant
# sera positif.
#
# Y = 1 si Return_NextMonth > 0
# Y = 0 sinon
#
# Modèle :
# LASSO LOGISTIQUE
#
# Variables explicatives :
# les 13 caractéristiques disponibles au mois t.
#
# IMPORTANT :
# 2022-2025 reste le test final.


# ------------------------------------------------------------
# 35.1 CREATION DE LA CIBLE
# ------------------------------------------------------------

classification_data <- copy(
  lasso_data
)


classification_data[
  ,
  Up_NextMonth :=
    as.integer(
      Return_NextMonth > 0
    )
]


# ------------------------------------------------------------
# 35.2 DISTRIBUTION DES CLASSES
# ------------------------------------------------------------

classification_data[
  ,
  .(
    N = .N,
    
    N_Down =
      sum(Up_NextMonth == 0),
    
    N_Up =
      sum(Up_NextMonth == 1),
    
    Pct_Down =
      100 *
      mean(Up_NextMonth == 0),
    
    Pct_Up =
      100 *
      mean(Up_NextMonth == 1)
  )
]


# ------------------------------------------------------------
# 35.3 SPLIT DEVELOPMENT / TEST
# ------------------------------------------------------------

classification_dev <- classification_data[
  Year <= 2021
]


classification_test <- classification_data[
  Year >= 2022
]


cat(
  "\nDevelopment :",
  nrow(classification_dev),
  "\n"
)

cat(
  "Test final :",
  nrow(classification_test),
  "\n"
)


# ============================================================
# 36. VALIDATION TEMPORELLE DU LASSO LOGISTIQUE
# ============================================================

library(glmnet)


lambda_class_grid <- 10^seq(
  from = -5,
  to = 0,
  length.out = 60
)


classification_cv_results <- data.table()


validation_years_class <- 2011:2021


for (validation_year in validation_years_class) {
  
  cat(
    "\nClassification - Validation :",
    validation_year,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 36.1 TRAIN
  # ----------------------------------------------------------
  
  fold_train <- classification_dev[
    Year < validation_year
  ]
  
  
  # ----------------------------------------------------------
  # 36.2 PURGE DU DERNIER MOIS
  # ----------------------------------------------------------
  
  last_train_month <- max(
    fold_train$MthCalDt
  )
  
  
  fold_train <- fold_train[
    MthCalDt < last_train_month
  ]
  
  
  # ----------------------------------------------------------
  # 36.3 VALIDATION
  # ----------------------------------------------------------
  
  fold_validation <- classification_dev[
    Year == validation_year
  ]
  
  
  # ----------------------------------------------------------
  # 36.4 X / Y
  # ----------------------------------------------------------
  
  X_train <- as.matrix(
    fold_train[
      ,
      ..features_final
    ]
  )
  
  
  y_train <-
    fold_train$Up_NextMonth
  
  
  X_validation <- as.matrix(
    fold_validation[
      ,
      ..features_final
    ]
  )
  
  
  y_validation <-
    fold_validation$Up_NextMonth
  
  
  # ----------------------------------------------------------
  # 36.5 STANDARDISATION SUR LE TRAIN UNIQUEMENT
  # ----------------------------------------------------------
  
  mu_fold <- colMeans(
    X_train
  )
  
  
  sd_fold <- apply(
    X_train,
    2,
    sd
  )
  
  
  sd_fold[
    !is.finite(sd_fold) |
      sd_fold == 0
  ] <- 1
  
  
  X_train_scaled <- scale(
    X_train,
    center = mu_fold,
    scale = sd_fold
  )
  
  
  X_validation_scaled <- scale(
    X_validation,
    center = mu_fold,
    scale = sd_fold
  )
  
  
  # ----------------------------------------------------------
  # 36.6 LASSO LOGISTIQUE
  # ----------------------------------------------------------
  
  fit_class <- glmnet(
    x = X_train_scaled,
    y = y_train,
    
    family = "binomial",
    
    alpha = 1,
    
    lambda = lambda_class_grid,
    
    standardize = FALSE,
    
    intercept = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 36.7 PROBABILITES
  # ----------------------------------------------------------
  
  prob_matrix <- predict(
    fit_class,
    
    newx = X_validation_scaled,
    
    s = lambda_class_grid,
    
    type = "response"
  )
  
  
  # ----------------------------------------------------------
  # 36.8 PERFORMANCE DE CHAQUE LAMBDA
  # ----------------------------------------------------------
  
  for (j in seq_along(lambda_class_grid)) {
    
    prob_j <- as.numeric(
      prob_matrix[
        ,
        j
      ]
    )
    
    
    # Classe prédite avec seuil 0.50
    
    class_j <- as.integer(
      prob_j >= 0.50
    )
    
    
    # Accuracy
    
    accuracy_j <- mean(
      class_j ==
        y_validation
    )
    
    
    # Sensibilité
    
    sensitivity_j <-
      if (
        sum(y_validation == 1) > 0
      ) {
        
        mean(
          class_j[
            y_validation == 1
          ] == 1
        )
        
      } else {
        
        NA_real_
        
      }
    
    
    # Spécificité
    
    specificity_j <-
      if (
        sum(y_validation == 0) > 0
      ) {
        
        mean(
          class_j[
            y_validation == 0
          ] == 0
        )
        
      } else {
        
        NA_real_
        
      }
    
    
    # Balanced accuracy
    
    balanced_accuracy_j <-
      mean(
        c(
          sensitivity_j,
          specificity_j
        ),
        na.rm = TRUE
      )
    
    
    # Brier score
    
    brier_j <- mean(
      (
        y_validation -
          prob_j
      )^2
    )
    
    
    # Log-loss
    
    eps <- 1e-15
    
    
    prob_safe <- pmin(
      pmax(
        prob_j,
        eps
      ),
      1 - eps
    )
    
    
    logloss_j <- -mean(
      y_validation *
        log(prob_safe) +
        
        (1 - y_validation) *
        log(1 - prob_safe)
    )
    
    
    # Nombre de variables retenues
    
    coef_j <- coef(
      fit_class,
      s = lambda_class_grid[j]
    )
    
    
    n_nonzero_j <- sum(
      as.numeric(
        coef_j[-1, ]
      ) != 0
    )
    
    
    classification_cv_results <- rbind(
      classification_cv_results,
      
      data.table(
        
        Validation_Year =
          validation_year,
        
        Lambda =
          lambda_class_grid[j],
        
        Accuracy =
          accuracy_j,
        
        Sensitivity =
          sensitivity_j,
        
        Specificity =
          specificity_j,
        
        Balanced_Accuracy =
          balanced_accuracy_j,
        
        Brier =
          brier_j,
        
        LogLoss =
          logloss_j,
        
        N_NonZero =
          n_nonzero_j
      )
    )
  }
}


# ============================================================
# 37. CHOIX DU LAMBDA DE CLASSIFICATION
# ============================================================

classification_lambda_summary <-
  classification_cv_results[
    ,
    .(
      Mean_Accuracy =
        mean(
          Accuracy,
          na.rm = TRUE
        ),
      
      Mean_Sensitivity =
        mean(
          Sensitivity,
          na.rm = TRUE
        ),
      
      Mean_Specificity =
        mean(
          Specificity,
          na.rm = TRUE
        ),
      
      Mean_Balanced_Accuracy =
        mean(
          Balanced_Accuracy,
          na.rm = TRUE
        ),
      
      Mean_Brier =
        mean(
          Brier,
          na.rm = TRUE
        ),
      
      Mean_LogLoss =
        mean(
          LogLoss,
          na.rm = TRUE
        ),
      
      Mean_N_NonZero =
        mean(
          N_NonZero,
          na.rm = TRUE
        )
    ),
    by = Lambda
  ]


# ------------------------------------------------------------
# 37.1 SELECTION
# ------------------------------------------------------------

# On sélectionne selon la Balanced Accuracy.
#
# Pourquoi ?
#
# Une accuracy brute peut être trompeuse si les classes
# hausse/baisse ne sont pas parfaitement équilibrées.


setorder(
  classification_lambda_summary,
  -Mean_Balanced_Accuracy,
  Mean_LogLoss
)


classification_lambda_summary[
  1:10
]


best_lambda_class <-
  classification_lambda_summary$Lambda[1]


cat(
  "\nLambda classification retenu :",
  best_lambda_class,
  "\n"
)


# ============================================================
# 38. MODELE FINAL DE CLASSIFICATION
# ============================================================

# Objectif :
#
# - best_lambda_class a déjà été choisi avec la validation
#   temporelle ;
# - on conserve ce lambda ;
# - on purge décembre 2021 du development avant le refit final ;
# - aucune cible réalisée en janvier 2022 ne doit entrer
#   dans l'entraînement de la classification.


# ------------------------------------------------------------
# 38.1 PURGE DE LA FRONTIERE TRAIN / TEST
# ------------------------------------------------------------

classification_dev_final <- classification_dev[
  MthCalDt <
    max(classification_dev$MthCalDt)
]


cat(
  "\n========================================\n",
  "PURGE FINALE CLASSIFICATION\n",
  "========================================\n"
)

cat(
  "Dernier mois development original :",
  as.character(
    max(classification_dev$MthCalDt)
  ),
  "\n"
)

cat(
  "Dernier mois utilise pour classification :",
  as.character(
    max(classification_dev_final$MthCalDt)
  ),
  "\n"
)

cat(
  "Observations development originales :",
  nrow(classification_dev),
  "\n"
)

cat(
  "Observations du train final :",
  nrow(classification_dev_final),
  "\n"
)

cat(
  "Observations retirees :",
  nrow(classification_dev) -
    nrow(classification_dev_final),
  "\n"
)

cat(
  "========================================\n"
)


# Vérifications anti-look-ahead.

stopifnot(
  max(classification_dev_final$MthCalDt) <=
    as.IDate("2021-11-30")
)

stopifnot(
  nrow(
    classification_dev_final[
      format(MthCalDt, "%Y-%m") == "2021-12"
    ]
  ) == 0
)


# ------------------------------------------------------------
# 38.2 MATRICES DEVELOPMENT
# ------------------------------------------------------------

X_dev_class <- as.matrix(
  classification_dev_final[
    ,
    ..features_final
  ]
)

y_dev_class <-
  classification_dev_final$Up_NextMonth


# ------------------------------------------------------------
# 38.3 MATRICES TEST
# ------------------------------------------------------------

X_test_class <- as.matrix(
  classification_test[
    ,
    ..features_final
  ]
)

y_test_class <-
  classification_test$Up_NextMonth


# Vérifications dimensions.

stopifnot(
  nrow(X_dev_class) ==
    length(y_dev_class)
)

stopifnot(
  nrow(X_test_class) ==
    length(y_test_class)
)

stopifnot(
  ncol(X_dev_class) ==
    length(features_final)
)

stopifnot(
  ncol(X_test_class) ==
    length(features_final)
)


# Vérification des valeurs.

stopifnot(
  all(
    is.finite(X_dev_class)
  )
)

stopifnot(
  all(
    is.finite(X_test_class)
  )
)

stopifnot(
  all(
    y_dev_class %in% c(0, 1)
  )
)

stopifnot(
  all(
    y_test_class %in% c(0, 1)
  )
)


# ------------------------------------------------------------
# 38.4 STANDARDISATION
# ------------------------------------------------------------

# Paramètres calculés uniquement sur le train final purgé.

mu_class <- colMeans(
  X_dev_class
)


sd_class <- apply(
  X_dev_class,
  2,
  sd
)


# Protection contre une éventuelle variable constante.

sd_class[
  !is.finite(sd_class) |
    sd_class == 0
] <- 1


# Standardisation du development.

X_dev_class_scaled <- scale(
  X_dev_class,
  center = mu_class,
  scale = sd_class
)


# Standardisation du test avec les paramètres
# du development uniquement.

X_test_class_scaled <- scale(
  X_test_class,
  center = mu_class,
  scale = sd_class
)


# Vérifications.

stopifnot(
  all(
    is.finite(X_dev_class_scaled)
  )
)

stopifnot(
  all(
    is.finite(X_test_class_scaled)
  )
)


# ------------------------------------------------------------
# 38.5 ESTIMATION FINALE
# ------------------------------------------------------------

# best_lambda_class vient de la validation temporelle.
# Il n'est PAS recalculé sur le test.

lasso_class_final <- glmnet(
  x = X_dev_class_scaled,
  y = y_dev_class,
  
  family = "binomial",
  
  alpha = 1,
  
  lambda = best_lambda_class,
  
  standardize = FALSE,
  
  intercept = TRUE
)


# ------------------------------------------------------------
# 38.6 CONTROLE FINAL
# ------------------------------------------------------------

cat(
  "\nClassification finale\n"
)

cat(
  "Train jusqu'au :",
  as.character(
    max(classification_dev_final$MthCalDt)
  ),
  "\n"
)

cat(
  "Test a partir du :",
  as.character(
    min(classification_test$MthCalDt)
  ),
  "\n"
)

cat(
  "Lambda retenu :",
  best_lambda_class,
  "\n"
)

cat(
  "N train final :",
  nrow(classification_dev_final),
  "\n"
)

cat(
  "N test :",
  nrow(classification_test),
  "\n"
)

cat(
  "PASS : classification finale purgee correctement.\n"
)

cat(
  "Aucune cible realisee en janvier 2022",
  "n'est utilisee pour entrainer",
  "la classification finale.\n"
)


# ============================================================
# 39. VARIABLES RETENUES
# ============================================================

class_coef <- as.matrix(
  coef(
    lasso_class_final,
    s = best_lambda_class
  )
)


class_selected <- data.table(
  
  Variable =
    rownames(class_coef),
  
  Coefficient =
    as.numeric(
      class_coef[
        ,
        1
      ]
    )
)


class_selected <- class_selected[
  Variable != "(Intercept)" &
    Coefficient != 0
]


class_selected[
  ,
  Abs_Coefficient :=
    abs(Coefficient)
]


setorder(
  class_selected,
  -Abs_Coefficient
)


class_selected


cat(
  "\nNombre de variables retenues :",
  nrow(class_selected),
  "/ 13\n"
)


# ============================================================
# 40. PREDICTIONS SUR LE TEST 2022-2025
# ============================================================

class_test_probability <- as.numeric(
  predict(
    lasso_class_final,
    
    newx = X_test_class_scaled,
    
    s = best_lambda_class,
    
    type = "response"
  )
)


class_test_prediction <- as.integer(
  class_test_probability >= 0.50
)


# ============================================================
# 41. MATRICE DE CONFUSION
# ============================================================

# On impose explicitement les deux classes 0 et 1
# même si le modèle n'en prédit qu'une seule.

confusion_matrix <- table(
  
  Predicted = factor(
    class_test_prediction,
    levels = c(0, 1)
  ),
  
  Actual = factor(
    y_test_class,
    levels = c(0, 1)
  )
)


confusion_matrix


# ------------------------------------------------------------
# 41.1 EXTRACTION TN / FN / FP / TP
# ------------------------------------------------------------

TN <- as.numeric(
  confusion_matrix[
    "0",
    "0"
  ]
)

FN <- as.numeric(
  confusion_matrix[
    "0",
    "1"
  ]
)

FP <- as.numeric(
  confusion_matrix[
    "1",
    "0"
  ]
)

TP <- as.numeric(
  confusion_matrix[
    "1",
    "1"
  ]
)


cat(
  "\nTN =", TN,
  "\nFP =", FP,
  "\nFN =", FN,
  "\nTP =", TP,
  "\n"
)

# ============================================================
# 42. METRIQUES DE CLASSIFICATION
# ============================================================

accuracy_test <-
  (TP + TN) /
  (TP + TN + FP + FN)


sensitivity_test <-
  TP /
  (TP + FN)


specificity_test <-
  TN /
  (TN + FP)


balanced_accuracy_test <-
  (
    sensitivity_test +
      specificity_test
  ) / 2


precision_test <-
  TP /
  (TP + FP)


negative_predictive_value <-
  TN /
  (TN + FN)


F1_test <-
  2 *
  precision_test *
  sensitivity_test /
  (
    precision_test +
      sensitivity_test
  )


# ------------------------------------------------------------
# 42.1 BENCHMARK CLASSE MAJORITAIRE
# ------------------------------------------------------------

majority_class <-
  as.integer(
    mean(y_dev_class) >= 0.50
  )


benchmark_accuracy <-
  mean(
    y_test_class ==
      majority_class
  )


# ------------------------------------------------------------
# 42.2 BRIER SCORE
# ------------------------------------------------------------

brier_test <- mean(
  (
    y_test_class -
      class_test_probability
  )^2
)


# ------------------------------------------------------------
# 42.3 LOG LOSS
# ------------------------------------------------------------

eps <- 1e-15


prob_safe <- pmin(
  pmax(
    class_test_probability,
    eps
  ),
  1 - eps
)


logloss_test <- -mean(
  
  y_test_class *
    log(prob_safe) +
    
    (1 - y_test_class) *
    log(1 - prob_safe)
)


# ============================================================
# 43. AUC SANS PACKAGE SUPPLEMENTAIRE
# ============================================================

# AUC calculée à partir des rangs des probabilités.
#
# AUC = 0.5 -> aucun pouvoir discriminant
# AUC > 0.5 -> information de classement


n_positive <- sum(
  y_test_class == 1
)


n_negative <- sum(
  y_test_class == 0
)


prob_ranks <- rank(
  class_test_probability,
  ties.method = "average"
)


auc_test <- (
  sum(
    prob_ranks[
      y_test_class == 1
    ]
  ) -
    n_positive *
    (n_positive + 1) /
    2
) /
  (
    n_positive *
      n_negative
  )


# ============================================================
# 44. TABLEAU FINAL
# ============================================================

classification_test_results <- data.table(
  
  Metric = c(
    "Accuracy",
    "Benchmark Accuracy",
    "Balanced Accuracy",
    "Sensitivity",
    "Specificity",
    "Precision",
    "NPV",
    "F1",
    "AUC",
    "Brier Score",
    "Log Loss"
  ),
  
  Value = c(
    accuracy_test,
    benchmark_accuracy,
    balanced_accuracy_test,
    sensitivity_test,
    specificity_test,
    precision_test,
    negative_predictive_value,
    F1_test,
    auc_test,
    brier_test,
    logloss_test
  )
)


classification_test_results


# ============================================================
# 45. COMPARAISON DIRECTE AU BENCHMARK
# ============================================================

cat(
  "\n=======================================\n"
)

cat(
  "CLASSIFICATION HAUSSE / BAISSE\n"
)

cat(
  "=======================================\n\n"
)


cat(
  "Accuracy LASSO :",
  round(
    100 * accuracy_test,
    2
  ),
  "%\n"
)


cat(
  "Accuracy benchmark :",
  round(
    100 * benchmark_accuracy,
    2
  ),
  "%\n"
)


cat(
  "Gain d'accuracy :",
  round(
    100 *
      (
        accuracy_test -
          benchmark_accuracy
      ),
    2
  ),
  "points\n\n"
)


cat(
  "Balanced Accuracy :",
  round(
    100 * balanced_accuracy_test,
    2
  ),
  "%\n"
)


cat(
  "AUC :",
  round(
    auc_test,
    4
  ),
  "\n"
)


cat(
  "Sensibilite (hausse) :",
  round(
    100 * sensitivity_test,
    2
  ),
  "%\n"
)


cat(
  "Specificite (baisse) :",
  round(
    100 * specificity_test,
    2
  ),
  "%\n\n"
)


cat(
  "Variables retenues :",
  nrow(class_selected),
  "/ 13\n"
)


# ============================================================
# 46. PERFORMANCE PAR ANNEE DU TEST
# ============================================================

classification_test_results_detail <- copy(
  classification_test
)


classification_test_results_detail[
  ,
  Predicted_Probability :=
    class_test_probability
]


classification_test_results_detail[
  ,
  Predicted_Class :=
    class_test_prediction
]


classification_test_results_detail[
  ,
  .(
    N =
      .N,
    
    Actual_Up_Rate =
      mean(
        Up_NextMonth
      ),
    
    Predicted_Up_Rate =
      mean(
        Predicted_Class
      ),
    
    Accuracy =
      mean(
        Predicted_Class ==
          Up_NextMonth
      )
  ),
  by = Year
]


