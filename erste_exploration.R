# pakete ----------
# allgemein
library(ggplot2)
library(foreign)
library(Kmisc)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(magrittr)

# faktorenanalyse
library(psych)
library(nFactors)
library(FactoMineR)


# daten einlesen -----------
df <- read.spss("Datensatz_15-12.sav", use.value.labels=T, to.data.frame=T)

# local data frame erstellen: wird besser dargestellt
df <- tbl_df(df)

# nicht komplette Antworten aussortieren:
df <- df %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))


# faktorenplots ----------

# Motive --
motive <- df %>%
  dplyr::select(., q_6_1:q_6_16) %>%
  data.matrix 


# factormap 
result <- PCA(motive, graph=F)
plot.PCA(motive, choix= "var")

# parallel test
ev <- eigen(cor(motive, use = "pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject = nrow(motive), var = ncol(motive), rep = 100, cent = 0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

# -> 4 Faktorenlösung angebracht



# Zukunft
zukunft <- df %>%
  dplyr::select(., q_19_1:q_19_6) %>%
  data.matrix 

# parallel test
ev <- eigen(cor(zukunft, use = "pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject = nrow(zukunft), var = ncol(zukunft), rep = 100, cent = 0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)


# balkendiagramme für abbruch und unterbrechung --------------
# unterbrechung

# abbruch
