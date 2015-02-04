# faktorenanalyse
library(psych)
library(nFactors)
library(FactoMineR)

# allgemein
library(ggplot2)
library(foreign)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(magrittr)
library(scales)



fa_oblimin <- function(x, n) {
  fit <- fa(x, nfactors = n, fm = "pa", rotate = "oblimin")
  print.psych(fit, cut = 0.15, sort = TRUE)
}



# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))


## Motive -------
motive <- df_sav %>%
  select(., q_6_1:q_6_16) %>%
  data.matrix

motive <- df_sav %>%
  select(., q_6_1:q_6_16) %>%
  data.matrix %>%
  na.omit %>%
  as.data.frame

# factormap
PCA(motive, graph=F) %>%
  plot.PCA(., choix="var")

# parallel test
ev <- eigen(cor(motive, use = "pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject = nrow(motive), var = ncol(motive), rep = 100, cent = 0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

# kaiser-Kriterium
fit_pca <- principal(motive, nfactors = 5, rotate = "none")
print.psych(fit_pca, cut = 0.15, sort = TRUE)
# kaiser spricht für 5-6 faktoren

# 5 Faktorenlösung -----

fit_pca <- principal(motive, nfactors = 5, rotate = "oblimin")
fit_fa <- fa(motive, nfactors = 5, fm = "pa", rotate = "oblimin")
print.psych(fit_pca, cut = 0.15, sort = TRUE)
print.psych(fit_fa, cut = 0.15, sort = TRUE)

# mache nur mit fa weiter
fit_fa_5 <- fa(motive, nfactors = 5, fm = "pa", rotate = "oblimin")
print.psych(fit_fa_5, cut = 0.15, sort = TRUE)

# raus: q_13

motive5 <- motive %>%
  select(-q_6_13)

fa_oblimin(motive5, 5)

# 5 faktorenlösung scheint nicht geeignet: items, die klar für das wissenschaftliche Interesse stehen, laden teils doppelt
# -> fahre mit 4 faktorenlösung fort


# 4 Faktoren ------
fa_oblimin(motive, 4)

# raus: q_6_13
motive4 <- motive %>%
  select(-q_6_13)

fa_oblimin(motive4, 4)

# raus: q_6_3
motive4 <- motive4 %>%
  select(-q_6_3)

fa_oblimin(motive4, 4)

# raus: q_6_8
motive4 <- motive4 %>%
  select(-q_6_8)

fa_oblimin(motive4, 4)
# gutes interpretierbares ergebnis



# 3 Faktoren -----
fa_oblimin(motive, 3)

motive3 <- motive %>%
  select(-q_6_13)
fa_oblimin(motive3, 3)

motive3 <- motive3 %>%
  select(-q_6_3, -q_6_8)
fa_oblimin(motive3, 3)

motive3 <- motive3 %>%
  select(-q_6_12)
fa_oblimin(motive3, 3)

# -> kein gut interpretierbares ergebnis: mit 4 Faktoren scheint es besser

