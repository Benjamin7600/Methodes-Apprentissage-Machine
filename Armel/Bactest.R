
# ============================================================
# 47. BACKTEST LONG-SHORT DU SIGNAL LASSO
# ============================================================

# Objectif :
#
# Chaque mois :
#
# LONG  = 20 % des actions ayant les prédictions LASSO
#         les plus élevées
#
# SHORT = 20 % des actions ayant les prédictions LASSO
#         les plus faibles
#
# Portefeuilles équipondérés.
#
# Rendement Long-Short :
#
# R_LS = R_LONG - R_SHORT
#
# IMPORTANT :
# Le portefeuille est formé à partir du SCORE PREDIT.
# La performance est évaluée avec Return_NextMonth réalisé.


# ------------------------------------------------------------
# 47.1 IDENTIFIER LA TABLE DE TEST DU LASSO DE REGRESSION
# ------------------------------------------------------------

# Dans les sections précédentes, nous avions déjà construit
# la table contenant les prédictions LASSO.
#
# Vérifie d'abord les objets disponibles :

# ------------------------------------------------------------
# 47.1 IDENTIFIER LES OBJETS EXISTANTS
# ------------------------------------------------------------

ls(
  pattern = "(?i)lasso|test|pred"
)



# ------------------------------------------------------------
# 47.2 CONSTRUCTION DE LA BASE DE BACKTEST
# ------------------------------------------------------------

# On repart du test final 2022-2025.
# Les prédictions sont celles du LASSO de régression
# déjà estimé précédemment.

ls_backtest <- copy(
  lasso_test
)


# Ajouter le score prédit par le LASSO

ls_backtest[
  ,
  LASSO_Score :=
    as.numeric(
      lasso_test_prediction
    )
]


# Contrôles

stopifnot(
  nrow(ls_backtest) ==
    length(lasso_test_prediction)
)

stopifnot(
  !anyNA(
    ls_backtest$LASSO_Score
  )
)


setorder(
  ls_backtest,
  MthCalDt,
  PERMNO
)


# ============================================================
# 48. FORMATION DES PORTEFEUILLES LONG ET SHORT
# ============================================================

# Chaque mois :
#
# Q1 = 20 % des scores LASSO les plus faibles  -> SHORT
# Q5 = 20 % des scores LASSO les plus élevés  -> LONG
#
# On utilise frank() plutôt que des seuils fixes afin
# d'effectuer le classement séparément chaque mois.


# ------------------------------------------------------------
# 48.1 CLASSEMENT CROSS-SECTIONNEL MENSUEL
# ------------------------------------------------------------

ls_backtest[
  ,
  Rank_Percentile :=
    frank(
      LASSO_Score,
      ties.method = "average"
    ) /
    .N,
  by = MthCalDt
]


# ------------------------------------------------------------
# 48.2 ATTRIBUTION LONG / SHORT
# ------------------------------------------------------------

ls_backtest[
  ,
  Position :=
    fifelse(
      Rank_Percentile <= 0.20,
      "SHORT",
      fifelse(
        Rank_Percentile > 0.80,
        "LONG",
        "MIDDLE"
      )
    )
]


# ------------------------------------------------------------
# 48.3 CONTROLE DU NOMBRE D'ACTIONS
# ------------------------------------------------------------

portfolio_counts <- ls_backtest[
  ,
  .(
    N_Total = .N,
    
    N_Long =
      sum(Position == "LONG"),
    
    N_Short =
      sum(Position == "SHORT"),
    
    Pct_Long =
      mean(Position == "LONG"),
    
    Pct_Short =
      mean(Position == "SHORT")
  ),
  by = MthCalDt
]


portfolio_counts[]


summary(
  portfolio_counts[
    ,
    .(
      Pct_Long,
      Pct_Short
    )
  ]
)


# ============================================================
# 49. RENDEMENTS MENSUELS DES PORTEFEUILLES
# ============================================================

# Portefeuilles équipondérés.
#
# IMPORTANT :
#
# Le score est connu au mois t.
# Return_NextMonth est le rendement réalisé au mois t+1.
#
# Il n'y a donc pas d'utilisation du rendement futur
# pour former le portefeuille.


monthly_portfolio <- ls_backtest[
  ,
  .(
    Long_Return =
      mean(
        Return_NextMonth[
          Position == "LONG"
        ]
      ),
    
    Short_Stock_Return =
      mean(
        Return_NextMonth[
          Position == "SHORT"
        ]
      ),
    
    Market_EW_Return =
      mean(
        Return_NextMonth
      ),
    
    N_Long =
      sum(
        Position == "LONG"
      ),
    
    N_Short =
      sum(
        Position == "SHORT"
      )
  ),
  by = MthCalDt
]


# ------------------------------------------------------------
# 49.1 RENDEMENT DE LA JAMBE SHORT
# ------------------------------------------------------------

# Short_Stock_Return représente le rendement des actions
# que nous vendons à découvert.
#
# Le P&L de la position short est donc son opposé.

monthly_portfolio[
  ,
  Short_PnL :=
    -Short_Stock_Return
]


# ------------------------------------------------------------
# 49.2 PORTEFEUILLE LONG-SHORT
# ------------------------------------------------------------

monthly_portfolio[
  ,
  LS_Return :=
    Long_Return -
    Short_Stock_Return
]


# ------------------------------------------------------------
# 49.3 EXCES DU LONG PAR RAPPORT AU PORTEFEUILLE EW
# ------------------------------------------------------------

monthly_portfolio[
  ,
  Long_Minus_EW :=
    Long_Return -
    Market_EW_Return
]


setorder(
  monthly_portfolio,
  MthCalDt
)


monthly_portfolio[]


# ============================================================
# 50. STATISTIQUES DESCRIPTIVES
# ============================================================

portfolio_summary <- data.table(
  
  Portfolio = c(
    "LONG Q5",
    "SHORT Q1 P&L",
    "LONG-SHORT",
    "ALL STOCKS EW"
  ),
  
  Mean_Monthly = c(
    
    mean(
      monthly_portfolio$Long_Return
    ),
    
    mean(
      monthly_portfolio$Short_PnL
    ),
    
    mean(
      monthly_portfolio$LS_Return
    ),
    
    mean(
      monthly_portfolio$Market_EW_Return
    )
  ),
  
  Vol_Monthly = c(
    
    sd(
      monthly_portfolio$Long_Return
    ),
    
    sd(
      monthly_portfolio$Short_PnL
    ),
    
    sd(
      monthly_portfolio$LS_Return
    ),
    
    sd(
      monthly_portfolio$Market_EW_Return
    )
  ),
  
  Positive_Months = c(
    
    mean(
      monthly_portfolio$Long_Return > 0
    ),
    
    mean(
      monthly_portfolio$Short_PnL > 0
    ),
    
    mean(
      monthly_portfolio$LS_Return > 0
    ),
    
    mean(
      monthly_portfolio$Market_EW_Return > 0
    )
  )
)


# ------------------------------------------------------------
# 50.1 ANNUALISATION
# ------------------------------------------------------------

portfolio_summary[
  ,
  Annualized_Mean :=
    12 *
    Mean_Monthly
]


portfolio_summary[
  ,
  Annualized_Vol :=
    sqrt(12) *
    Vol_Monthly
]


# Sharpe simplifié pour le Long-Short.
#
# Le LS est approximativement dollar-neutral :
# on ne soustrait donc pas RF une deuxième fois ici.

portfolio_summary[
  ,
  Annualized_Mean_Vol :=
    Annualized_Mean /
    Annualized_Vol
]


portfolio_summary[]


# ============================================================
# 51. PERFORMANCE CUMULEE
# ============================================================

# ATTENTION :
#
# Le cumul du Long-Short avec prod(1 + LS_Return)
# est utilisé ici comme indice de richesse synthétique.
# Il suppose une stratégie rééquilibrée mensuellement.


monthly_portfolio[
  ,
  Wealth_Long :=
    cumprod(
      1 + Long_Return
    )
]


monthly_portfolio[
  ,
  Wealth_EW :=
    cumprod(
      1 + Market_EW_Return
    )
]


monthly_portfolio[
  ,
  Wealth_LS :=
    cumprod(
      1 + LS_Return
    )
]


tail(
  monthly_portfolio[
    ,
    .(
      MthCalDt,
      Wealth_Long,
      Wealth_EW,
      Wealth_LS
    )
  ]
)


# ============================================================
# 52. MAXIMUM DRAWDOWN
# ============================================================

max_drawdown <- function(wealth) {
  
  # Ajouter le capital initial
  wealth_with_initial <- c(
    1,
    wealth
  )
  
  running_max <- cummax(
    wealth_with_initial
  )
  
  drawdown <-
    wealth_with_initial /
    running_max -
    1
  
  min(
    drawdown
  )
}


drawdown_results <- data.table(
  
  Portfolio = c(
    "LONG Q5",
    "LONG-SHORT",
    "ALL STOCKS EW"
  ),
  
  Max_Drawdown = c(
    
    max_drawdown(
      monthly_portfolio$Wealth_Long
    ),
    
    max_drawdown(
      monthly_portfolio$Wealth_LS
    ),
    
    max_drawdown(
      monthly_portfolio$Wealth_EW
    )
  )
)


drawdown_results[]


# ============================================================
# 53. SIGNIFICATIVITE DU RENDEMENT LONG-SHORT
# ============================================================

library(lmtest)
library(sandwich)


ls_mean_model <- lm(
  LS_Return ~ 1,
  data = monthly_portfolio
)


ls_mean_nw <- NeweyWest(
  ls_mean_model,
  lag = 3,
  prewhite = FALSE,
  adjust = TRUE
)


ls_mean_test <- coeftest(
  ls_mean_model,
  vcov. = ls_mean_nw
)


ls_mean_test


# ============================================================
# 54. AJOUT DES FACTEURS FF5 AUX RENDEMENTS DU PORTEFEUILLE
# ============================================================

# ATTENTION AU DECALAGE TEMPOREL :
#
# Une ligne datée t dans lasso_test utilise Return_NextMonth,
# donc le rendement du portefeuille correspond économiquement
# au mois t+1.
#
# On utilise donc les facteurs *_NextMonth déjà alignés
# dans data_model.


ff_monthly <- ls_backtest[
  ,
  .(
    MKT_RF =
      first(
        MKT_RF_NextMonth
      ),
    
    SMB =
      first(
        SMB_NextMonth
      ),
    
    HML =
      first(
        HML_NextMonth
      ),
    
    RMW =
      first(
        RMW_NextMonth
      ),
    
    CMA =
      first(
        CMA_NextMonth
      ),
    
    RF =
      first(
        RF_NextMonth
      )
  ),
  by = MthCalDt
]


monthly_portfolio <- merge(
  monthly_portfolio,
  ff_monthly,
  by = "MthCalDt",
  all.x = TRUE,
  sort = FALSE
)


setorder(
  monthly_portfolio,
  MthCalDt
)


stopifnot(
  !anyNA(
    monthly_portfolio[
      ,
      .(
        MKT_RF,
        SMB,
        HML,
        RMW,
        CMA,
        RF
      )
    ]
  )
)


# ============================================================
# 55. ALPHA FAMA-FRENCH 5 DU LONG-SHORT
# ============================================================

# Pour un portefeuille LONG-SHORT dollar-neutral :
#
# R_LONG - R_SHORT
#
# le RF s'annule algébriquement.
#
# On régresse donc directement LS_Return sur FF5.


ff5_ls_model <- lm(
  
  LS_Return ~
    MKT_RF +
    SMB +
    HML +
    RMW +
    CMA,
  
  data = monthly_portfolio
)


summary(
  ff5_ls_model
)


# ------------------------------------------------------------
# 55.1 ERREURS STANDARDS NEWEY-WEST
# ------------------------------------------------------------

ff5_ls_nw <- NeweyWest(
  ff5_ls_model,
  lag = 3,
  prewhite = FALSE,
  adjust = TRUE
)


ff5_ls_test <- coeftest(
  ff5_ls_model,
  vcov. = ff5_ls_nw
)


ff5_ls_test


# ------------------------------------------------------------
# 55.2 EXTRACTION DE L'ALPHA
# ------------------------------------------------------------

ff5_alpha <-
  coef(
    ff5_ls_model
  )[
    "(Intercept)"
  ]


ff5_alpha_se <-
  sqrt(
    ff5_ls_nw[
      "(Intercept)",
      "(Intercept)"
    ]
  )


ff5_alpha_t <-
  ff5_alpha /
  ff5_alpha_se


ff5_alpha_p <-
  2 *
  pt(
    -abs(
      ff5_alpha_t
    ),
    df =
      df.residual(
        ff5_ls_model
      )
  )


alpha_results <- data.table(
  
  Alpha_Monthly =
    ff5_alpha,
  
  Alpha_Monthly_Percent =
    100 *
    ff5_alpha,
  
  Alpha_Annualized_Approx =
    12 *
    ff5_alpha,
  
  NW_Standard_Error =
    ff5_alpha_se,
  
  NW_t_stat =
    ff5_alpha_t,
  
  NW_p_value =
    ff5_alpha_p
)


alpha_results[]


# ============================================================
# 56. PERFORMANCE PAR ANNEE
# ============================================================

monthly_portfolio[
  ,
  Year :=
    as.integer(
      format(
        MthCalDt,
        "%Y"
      )
    )
]


performance_by_year <- monthly_portfolio[
  ,
  .(
    N_Months =
      .N,
    
    Long_Return =
      mean(
        Long_Return
      ),
    
    Short_Stock_Return =
      mean(
        Short_Stock_Return
      ),
    
    LongShort_Return =
      mean(
        LS_Return
      ),
    
    EW_Return =
      mean(
        Market_EW_Return
      ),
    
    LS_Positive_Months =
      mean(
        LS_Return > 0
      )
  ),
  by = Year
]


performance_by_year[]


# ============================================================
# 57. RESULTATS ESSENTIELS DU BACKTEST
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "BACKTEST LASSO LONG-SHORT - TEST 2022-2025\n"
)

cat(
  "============================================\n\n"
)


cat(
  "Nombre de mois :",
  nrow(monthly_portfolio),
  "\n\n"
)


cat(
  "Rendement mensuel moyen LONG :",
  round(
    100 *
      mean(
        monthly_portfolio$Long_Return
      ),
    3
  ),
  "%\n"
)


cat(
  "Rendement mensuel moyen actions SHORT :",
  round(
    100 *
      mean(
        monthly_portfolio$Short_Stock_Return
      ),
    3
  ),
  "%\n"
)


cat(
  "Rendement mensuel moyen LONG-SHORT :",
  round(
    100 *
      mean(
        monthly_portfolio$LS_Return
      ),
    3
  ),
  "%\n\n"
)


cat(
  "Mois LONG-SHORT positifs :",
  round(
    100 *
      mean(
        monthly_portfolio$LS_Return > 0
      ),
    2
  ),
  "%\n\n"
)


cat(
  "Alpha FF5 mensuel :",
  round(
    100 *
      ff5_alpha,
    3
  ),
  "%\n"
)


cat(
  "t-stat Newey-West alpha :",
  round(
    ff5_alpha_t,
    3
  ),
  "\n"
)


cat(
  "p-value alpha :",
  round(
    ff5_alpha_p,
    4
  ),
  "\n"
)


portfolio_summary
drawdown_results
ls_mean_test
ff5_ls_test
performance_by_year



# ============================================================
# 58. EVALUATION DU PORTEFEUILLE LONG-ONLY Q5
# ============================================================

# Objectif :
#
# Evaluer le portefeuille constitué uniquement des actions
# appartenant au Top 20 % des scores LASSO.
#
# Deux questions :
#
# 1. Q5 surperforme-t-il le portefeuille équipondere EW ?
#
# 2. Q5 genere-t-il un alpha apres controle des facteurs FF5 ?


# ------------------------------------------------------------
# 58.1 SURPERFORMANCE Q5 PAR RAPPORT A EW
# ------------------------------------------------------------

monthly_portfolio[
  ,
  Q5_Minus_EW :=
    Long_Return -
    Market_EW_Return
]


summary(
  monthly_portfolio$Q5_Minus_EW
)


cat(
  "\nSurperformance moyenne mensuelle Q5 - EW :",
  round(
    100 *
      mean(
        monthly_portfolio$Q5_Minus_EW
      ),
    4
  ),
  "%\n"
)


cat(
  "Mois ou Q5 bat EW :",
  round(
    100 *
      mean(
        monthly_portfolio$Q5_Minus_EW > 0
      ),
    2
  ),
  "%\n"
)


# ============================================================
# 59. TEST NEWEY-WEST DE Q5 - EW
# ============================================================

q5_ew_model <- lm(
  Q5_Minus_EW ~ 1,
  data = monthly_portfolio
)


q5_ew_nw <- NeweyWest(
  q5_ew_model,
  lag = 3,
  prewhite = FALSE,
  adjust = TRUE
)


q5_ew_test <- coeftest(
  q5_ew_model,
  vcov. = q5_ew_nw
)


q5_ew_test


# ------------------------------------------------------------
# 59.1 INTERVALLE DE CONFIANCE
# ------------------------------------------------------------

q5_ew_mean <-
  coef(
    q5_ew_model
  )[1]


q5_ew_se <-
  sqrt(
    q5_ew_nw[1, 1]
  )


q5_ew_t <-
  q5_ew_mean /
  q5_ew_se


q5_ew_p <-
  2 *
  pt(
    -abs(q5_ew_t),
    df = nrow(monthly_portfolio) - 1
  )


q5_ew_ci_low <-
  q5_ew_mean -
  qt(
    0.975,
    df = nrow(monthly_portfolio) - 1
  ) *
  q5_ew_se


q5_ew_ci_high <-
  q5_ew_mean +
  qt(
    0.975,
    df = nrow(monthly_portfolio) - 1
  ) *
  q5_ew_se


q5_vs_ew_results <- data.table(
  
  Mean_Monthly =
    q5_ew_mean,
  
  Mean_Monthly_Percent =
    100 * q5_ew_mean,
  
  Annualized_Approx =
    12 * q5_ew_mean,
  
  NW_SE =
    q5_ew_se,
  
  NW_t_stat =
    q5_ew_t,
  
  NW_p_value =
    q5_ew_p,
  
  CI95_Lower =
    q5_ew_ci_low,
  
  CI95_Upper =
    q5_ew_ci_high,
  
  Pct_Months_Q5_Beats_EW =
    mean(
      monthly_portfolio$Q5_Minus_EW > 0
    )
)


q5_vs_ew_results[]


# ============================================================
# 60. RENDEMENT EXCEDENTAIRE DU PORTEFEUILLE Q5
# ============================================================

# Pour FF5, la variable dependante doit etre :
#
# R_Q5 - RF


monthly_portfolio[
  ,
  Long_Excess_Return :=
    Long_Return -
    RF
]


summary(
  monthly_portfolio$Long_Excess_Return
)

monthly_portfolio[
  ,
  Long_Excess_Return :=
    Long_Return -
    RF
]
# ============================================================
# 61. ALPHA FAMA-FRENCH 5 DU PORTEFEUILLE LONG Q5
# ============================================================

q5_ff5_model <- lm(
  
  Long_Excess_Return ~
    MKT_RF +
    SMB +
    HML +
    RMW +
    CMA,
  
  data = monthly_portfolio
)


summary(
  q5_ff5_model
)


# ------------------------------------------------------------
# 61.1 ERREURS STANDARDS NEWEY-WEST
# ------------------------------------------------------------

q5_ff5_nw <- NeweyWest(
  q5_ff5_model,
  lag = 3,
  prewhite = FALSE,
  adjust = TRUE
)


q5_ff5_test <- coeftest(
  q5_ff5_model,
  vcov. = q5_ff5_nw
)


q5_ff5_test


# ------------------------------------------------------------
# 61.2 EXTRACTION DE L'ALPHA
# ------------------------------------------------------------

q5_alpha <-
  coef(
    q5_ff5_model
  )[
    "(Intercept)"
  ]


q5_alpha_se <-
  sqrt(
    q5_ff5_nw[
      "(Intercept)",
      "(Intercept)"
    ]
  )


q5_alpha_t <-
  q5_alpha /
  q5_alpha_se


q5_alpha_p <-
  2 *
  pt(
    -abs(q5_alpha_t),
    df =
      df.residual(
        q5_ff5_model
      )
  )


q5_alpha_ci_low <-
  q5_alpha -
  qt(
    0.975,
    df =
      df.residual(
        q5_ff5_model
      )
  ) *
  q5_alpha_se


q5_alpha_ci_high <-
  q5_alpha +
  qt(
    0.975,
    df =
      df.residual(
        q5_ff5_model
      )
  ) *
  q5_alpha_se


q5_alpha_results <- data.table(
  
  Alpha_Monthly =
    q5_alpha,
  
  Alpha_Monthly_Percent =
    100 * q5_alpha,
  
  Alpha_Annualized_Approx =
    12 * q5_alpha,
  
  NW_SE =
    q5_alpha_se,
  
  NW_t_stat =
    q5_alpha_t,
  
  NW_p_value =
    q5_alpha_p,
  
  CI95_Lower =
    q5_alpha_ci_low,
  
  CI95_Upper =
    q5_alpha_ci_high
)


q5_alpha_results[]


# ============================================================
# 62. SHARPE DU LONG Q5 ET DU BENCHMARK EW
# ============================================================

# Ici on utilise les rendements excedentaires par rapport
# au taux sans risque.


monthly_portfolio[
  ,
  EW_Excess_Return :=
    Market_EW_Return -
    RF
]


q5_sharpe <-
  sqrt(12) *
  mean(
    monthly_portfolio$Long_Excess_Return
  ) /
  sd(
    monthly_portfolio$Long_Excess_Return
  )


ew_sharpe <-
  sqrt(12) *
  mean(
    monthly_portfolio$EW_Excess_Return
  ) /
  sd(
    monthly_portfolio$EW_Excess_Return
  )


sharpe_comparison <- data.table(
  
  Portfolio = c(
    "LONG Q5",
    "ALL STOCKS EW"
  ),
  
  Annualized_Sharpe = c(
    q5_sharpe,
    ew_sharpe
  )
)


sharpe_comparison[]


# ============================================================
# 63. TRACKING ERROR ET INFORMATION RATIO
# ============================================================

tracking_error <-
  sqrt(12) *
  sd(
    monthly_portfolio$Q5_Minus_EW
  )


information_ratio <-
  (
    12 *
      mean(
        monthly_portfolio$Q5_Minus_EW
      )
  ) /
  tracking_error


relative_performance <- data.table(
  
  Annualized_Excess_Return =
    12 *
    mean(
      monthly_portfolio$Q5_Minus_EW
    ),
  
  Tracking_Error =
    tracking_error,
  
  Information_Ratio =
    information_ratio,
  
  Pct_Months_Outperform =
    mean(
      monthly_portfolio$Q5_Minus_EW > 0
    )
)


relative_performance[]


# ============================================================
# 64. PERFORMANCE LONG Q5 VS EW PAR ANNEE
# ============================================================

q5_vs_ew_by_year <- monthly_portfolio[
  ,
  .(
    N_Months =
      .N,
    
    Q5_Mean_Return =
      mean(
        Long_Return
      ),
    
    EW_Mean_Return =
      mean(
        Market_EW_Return
      ),
    
    Q5_Minus_EW =
      mean(
        Q5_Minus_EW
      ),
    
    Q5_Beats_EW =
      mean(
        Q5_Minus_EW > 0
      ),
    
    Q5_Volatility =
      sd(
        Long_Return
      ),
    
    EW_Volatility =
      sd(
        Market_EW_Return
      )
  ),
  by = Year
]


q5_vs_ew_by_year[]


# ============================================================
# 65. TABLEAU DE SYNTHESE DU PORTEFEUILLE LONG-ONLY
# ============================================================

long_only_summary <- data.table(
  
  Metric = c(
    
    "Q5 Mean Monthly Return",
    "EW Mean Monthly Return",
    
    "Q5 Annualized Mean",
    "EW Annualized Mean",
    
    "Q5 Annualized Volatility",
    "EW Annualized Volatility",
    
    "Q5 Sharpe",
    "EW Sharpe",
    
    "Q5 Maximum Drawdown",
    "EW Maximum Drawdown",
    
    "Q5-EW Mean Monthly",
    
    "Q5-EW Newey-West t-stat",
    "Q5-EW Newey-West p-value",
    
    "Q5 FF5 Alpha Monthly",
    "Q5 FF5 Alpha t-stat",
    "Q5 FF5 Alpha p-value",
    
    "Information Ratio"
  ),
  
  Value = c(
    
    mean(
      monthly_portfolio$Long_Return
    ),
    
    mean(
      monthly_portfolio$Market_EW_Return
    ),
    
    12 *
      mean(
        monthly_portfolio$Long_Return
      ),
    
    12 *
      mean(
        monthly_portfolio$Market_EW_Return
      ),
    
    sqrt(12) *
      sd(
        monthly_portfolio$Long_Return
      ),
    
    sqrt(12) *
      sd(
        monthly_portfolio$Market_EW_Return
      ),
    
    q5_sharpe,
    
    ew_sharpe,
    
    max_drawdown(
      monthly_portfolio$Wealth_Long
    ),
    
    max_drawdown(
      monthly_portfolio$Wealth_EW
    ),
    
    mean(
      monthly_portfolio$Q5_Minus_EW
    ),
    
    q5_ew_t,
    
    q5_ew_p,
    
    q5_alpha,
    
    q5_alpha_t,
    
    q5_alpha_p,
    
    information_ratio
  )
)


long_only_summary[]

q5_vs_ew_results
q5_ff5_test
q5_alpha_results
sharpe_comparison
q5_vs_ew_by_year