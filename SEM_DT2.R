library(lavaan)
library(semPlot)
library(dplyr)

df <- read.csv('df_final.csv')

####CLEAN####
df <- df[!duplicated(df$Participant), ]

####Z-SCORES####
vars_to_scale <- c(
  # Questionnaires
  "bfne_3",   "bfne_6",   "bfne_12",
  "bai_3",  "bai_6",  "bai_12",
  "epds_3", "epds_6","epds_12",
  # DT_2
  "DT_Anger_2_T3",   "DT_Anger_2_T6",   "DT_Anger_2_T12",
  "DT_Fear_2_T3",    "DT_Fear_2_T6",    "DT_Fear_2_T12",
  "DT_Joy_2_T3",     "DT_Joy_2_T6",     "DT_Joy_2_T12",
  "DT_Neutral_2_T3", "DT_Neutral_2_T6", "DT_Neutral_2_T12"
)

vars_to_scale <- vars_to_scale[vars_to_scale %in% names(df)]

df_z <- df
df_z[vars_to_scale] <- lapply(df[vars_to_scale], scale)


####complete longitudinal CLPM####
M1_CLPM_4conditions <- '

  # AUTORÉGRESSIFS
  DT_Anger_2_T6   ~ DT_Anger_2_T3
  DT_Anger_2_T12  ~ DT_Anger_2_T6

  DT_Fear_2_T6    ~ DT_Fear_2_T3
  DT_Fear_2_T12   ~ DT_Fear_2_T6

  DT_Joy_2_T6     ~ DT_Joy_2_T3
  DT_Joy_2_T12    ~ DT_Joy_2_T6

  DT_Neutral_2_T6  ~ DT_Neutral_2_T3
  DT_Neutral_2_T12 ~ DT_Neutral_2_T6

  # CROSS-LAGS : BAI → DT_2 (anxiété générale)
  DT_Anger_2_T6   ~ bai_3
  DT_Anger_2_T12  ~ bai_6
  DT_Fear_2_T6    ~ bai_3
  DT_Fear_2_T12   ~ bai_6
  DT_Joy_2_T6     ~ bai_3
  DT_Joy_2_T12    ~ bai_6
  DT_Neutral_2_T6  ~ bai_3
  DT_Neutral_2_T12 ~ bai_6

  # CROSS-LAGS : BFNE → DT_2 (anxiété sociale)
  DT_Anger_2_T6   ~ bfne_3
  DT_Anger_2_T12  ~ bfne_6
  DT_Fear_2_T6    ~ bfne_3
  DT_Fear_2_T12   ~ bfne_6
  DT_Joy_2_T6     ~ bfne_3
  DT_Joy_2_T12    ~ bfne_6
  DT_Neutral_2_T6  ~ bfne_3
  DT_Neutral_2_T12 ~ bfne_6
  
  # CORRÉLATIONS CONTEMPORAINES entre prédicteurs
  bai_3  ~~ bfne_3
  bai_6  ~~ bfne_6

  # CORRÉLATIONS RÉSIDUELLES entre conditions (même temps)
  DT_Anger_2_T6  ~~ DT_Fear_2_T6
  DT_Anger_2_T6  ~~ DT_Joy_2_T6
  DT_Anger_2_T6  ~~ DT_Neutral_2_T6
  DT_Fear_2_T6   ~~ DT_Joy_2_T6
  DT_Fear_2_T6   ~~ DT_Neutral_2_T6
  DT_Joy_2_T6    ~~ DT_Neutral_2_T6

  DT_Anger_2_T12  ~~ DT_Fear_2_T12
  DT_Anger_2_T12  ~~ DT_Joy_2_T12
  DT_Anger_2_T12  ~~ DT_Neutral_2_T12
  DT_Fear_2_T12   ~~ DT_Joy_2_T12
  DT_Fear_2_T12   ~~ DT_Neutral_2_T12
  DT_Joy_2_T12    ~~ DT_Neutral_2_T12

'

fit_M1 <- sem(M1_CLPM_4conditions,
              data      = df_z,
              estimator = "MLR",
              missing   = "FIML",
              fixed.x   = FALSE)

if (lavInspect(fit_M1, "converged")) {
  summary(fit_M1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
}

M1_BAI <- '
 
  # AUTORÉGRESSIFS
  DT_Anger_2_T6    ~ DT_Anger_2_T3
  DT_Anger_2_T12   ~ DT_Anger_2_T6
 
  DT_Fear_2_T6     ~ DT_Fear_2_T3
  DT_Fear_2_T12    ~ DT_Fear_2_T6
 
  DT_Joy_2_T6      ~ DT_Joy_2_T3
  DT_Joy_2_T12     ~ DT_Joy_2_T6
 
  DT_Neutral_2_T6  ~ DT_Neutral_2_T3
  DT_Neutral_2_T12 ~ DT_Neutral_2_T6
 
  # CROSS-LAGS : BAI → DT_2
  DT_Anger_2_T6    ~ bai_3
  DT_Anger_2_T12   ~ bai_6
 
  DT_Fear_2_T6     ~ bai_3
  DT_Fear_2_T12    ~ bai_6
 
  DT_Joy_2_T6      ~ bai_3
  DT_Joy_2_T12     ~ bai_6
 
  DT_Neutral_2_T6  ~ bai_3
  DT_Neutral_2_T12 ~ bai_6
 
  # CORRÉLATIONS RÉSIDUELLES entre conditions 
  DT_Anger_2_T6  ~~ DT_Fear_2_T6
  DT_Anger_2_T6  ~~ DT_Joy_2_T6
  DT_Anger_2_T6  ~~ DT_Neutral_2_T6
  DT_Fear_2_T6   ~~ DT_Joy_2_T6
  DT_Fear_2_T6   ~~ DT_Neutral_2_T6
  DT_Joy_2_T6    ~~ DT_Neutral_2_T6
 
  DT_Anger_2_T12  ~~ DT_Fear_2_T12
  DT_Anger_2_T12  ~~ DT_Joy_2_T12
  DT_Anger_2_T12  ~~ DT_Neutral_2_T12
  DT_Fear_2_T12   ~~ DT_Joy_2_T12
  DT_Fear_2_T12   ~~ DT_Neutral_2_T12
  DT_Joy_2_T12    ~~ DT_Neutral_2_T12
 
'

fit_M1_BAI <- sem(M1_BAI,
                  data      = df_z,
                  estimator = "MLR",
                  missing   = "FIML",
                  fixed.x   = FALSE)

summary(fit_M1_BAI, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

M1_epds <- '

  # AUTORÉGRESSIFS
  DT_Anger_2_T6    ~ DT_Anger_2_T3
  DT_Anger_2_T12   ~ DT_Anger_2_T6

  DT_Fear_2_T6     ~ DT_Fear_2_T3
  DT_Fear_2_T12    ~ DT_Fear_2_T6

  DT_Joy_2_T6      ~ DT_Joy_2_T3
  DT_Joy_2_T12     ~ DT_Joy_2_T6

  DT_Neutral_2_T6  ~ DT_Neutral_2_T3
  DT_Neutral_2_T12 ~ DT_Neutral_2_T6

  # CROSS-LAGS : epds → DT_2
  DT_Anger_2_T6    ~ epds_3
  DT_Anger_2_T12   ~ epds_6

  DT_Fear_2_T6     ~ epds_3
  DT_Fear_2_T12    ~ epds_6

  DT_Joy_2_T6      ~ epds_3
  DT_Joy_2_T12     ~ epds_6

  DT_Neutral_2_T6  ~ epds_3
  DT_Neutral_2_T12 ~ epds_6

  # CORRÉLATIONS RÉSIDUELLES entre conditions
  DT_Anger_2_T6  ~~ DT_Fear_2_T6
  DT_Anger_2_T6  ~~ DT_Joy_2_T6
  DT_Anger_2_T6  ~~ DT_Neutral_2_T6
  DT_Fear_2_T6   ~~ DT_Joy_2_T6
  DT_Fear_2_T6   ~~ DT_Neutral_2_T6
  DT_Joy_2_T6    ~~ DT_Neutral_2_T6

  DT_Anger_2_T12  ~~ DT_Fear_2_T12
  DT_Anger_2_T12  ~~ DT_Joy_2_T12
  DT_Anger_2_T12  ~~ DT_Neutral_2_T12
  DT_Fear_2_T12   ~~ DT_Joy_2_T12
  DT_Fear_2_T12   ~~ DT_Neutral_2_T12
  DT_Joy_2_T12    ~~ DT_Neutral_2_T12

'
fit_M1_epds <- sem(M1_epds,
                   data      = df_z,
                   estimator = "MLR",
                   missing   = "FIML",
                   fixed.x   = FALSE)

summary(fit_M1_epds, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


M1_bfne <- '

  # AUTORÉGRESSIFS
  DT_Anger_2_T6    ~ DT_Anger_2_T3
  DT_Anger_2_T12   ~ DT_Anger_2_T6

  DT_Fear_2_T6     ~ DT_Fear_2_T3
  DT_Fear_2_T12    ~ DT_Fear_2_T6

  DT_Joy_2_T6      ~ DT_Joy_2_T3
  DT_Joy_2_T12     ~ DT_Joy_2_T6

  DT_Neutral_2_T6  ~ DT_Neutral_2_T3
  DT_Neutral_2_T12 ~ DT_Neutral_2_T6

  # CROSS-LAGS : bfne → DT_2
  DT_Anger_2_T6    ~ bfne_3
  DT_Anger_2_T12   ~ bfne_6

  DT_Fear_2_T6     ~ bfne_3
  DT_Fear_2_T12    ~ bfne_6

  DT_Joy_2_T6      ~ bfne_3
  DT_Joy_2_T12     ~ bfne_6

  DT_Neutral_2_T6  ~ bfne_3
  DT_Neutral_2_T12 ~ bfne_6

  # CORRÉLATIONS RÉSIDUELLES entre conditions
  DT_Anger_2_T6  ~~ DT_Fear_2_T6
  DT_Anger_2_T6  ~~ DT_Joy_2_T6
  DT_Anger_2_T6  ~~ DT_Neutral_2_T6
  DT_Fear_2_T6   ~~ DT_Joy_2_T6
  DT_Fear_2_T6   ~~ DT_Neutral_2_T6
  DT_Joy_2_T6    ~~ DT_Neutral_2_T6

  DT_Anger_2_T12  ~~ DT_Fear_2_T12
  DT_Anger_2_T12  ~~ DT_Joy_2_T12
  DT_Anger_2_T12  ~~ DT_Neutral_2_T12
  DT_Fear_2_T12   ~~ DT_Joy_2_T12
  DT_Fear_2_T12   ~~ DT_Neutral_2_T12
  DT_Joy_2_T12    ~~ DT_Neutral_2_T12

'

fit_M1_bfne <- sem(M1_bfne,
                  data      = df_z,
                  estimator = "MLR",
                  missing   = "FIML",
                  fixed.x   = FALSE)

summary(fit_M1_bfne, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)