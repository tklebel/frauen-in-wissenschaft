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



fa_oblimin <- function(x, n, rotation="oblimin") {
  fit <- fa(x, nfactors = n, fm = "pa", rotate = rotation)
  print.psych(fit, cut = 0.15, sort = TRUE)
}


fa_parallel <- function(x) {
  ev <- eigen(cor(x, use = "pairwise.complete.obs")) # get eigenvalues
  ap <- parallel(subject = nrow(x), var = ncol(x), rep = 100, cent = 0.05)
  nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
  plotnScree(nS)
}


# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))


## Motive -------
zukunft <- df_sav %>%
  select(., q_19_1:q_19_6) %>%
  data.matrix %>%
  na.omit %>%
  as.data.frame

# eignung der matrix
KMO(zukunft)
# eignung ist ok, aber nicht super

# factormap
PCA(zukunft, graph=F) %>%
  plot.PCA(., choix="var")

# parallel test
fa_parallel(zukunft)
# 2 Faktoren

# kaiser-Kriterium
fit_pca <- principal(zukunft, nfactors = 6, rotate = "none")
print.psych(fit_pca, cut = 0.15, sort = TRUE)
# kaiser spricht für 2 faktoren

KMO(zukunft)
# eignung ist ok, aber nicht super


# 2 faktorenlösung
fa_oblimin(zukunft, 2)

# nehmen wir mal q_19_1 raus, vielleicht gehts dann besser
zukunft <- zukunft %>%
  select(-q_19_1)

fa_oblimin(zukunft, 2)

# nö, ist nicht besser.

# vielleicht mal fa nur für bwl?
# geht nicht, produziert heywood case

# probiere standard pca
fit_pca <- principal(zukunft, nfactors = 2, rotate = "oblimin")
print.psych(fit_pca, cut = .20, sort = TRUE)

# v5 raushauen
zukunft <- zukunft %>%
  select(-q_19_5)
fit_pca <- principal(zukunft, nfactors = 2, rotate = "varimax")
print.psych(fit_pca, cut = .20, sort = TRUE)
