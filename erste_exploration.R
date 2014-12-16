# pakete ----------
# faktorenanalyse
library(psych)
library(nFactors)
library(FactoMineR)

# allgemein
library(ggplot2)
library(foreign)
library(Kmisc)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(magrittr)


# working directory
setwd("/Users/thomask/Dropbox/Soziologie/Frauen\ in\ der\ Wissenschaft/quantitative\ Erhebung/Arbeitsbereich\ Thomas/Berechnungen")

# daten einlesen -----------
df <- read.spss("Data/Datensatz_15-12.sav", use.value.labels=T, to.data.frame=T)

# alternative Daten, direkt aus LimeSurvey
source("Data/survey_49753_R_syntax_file.R", encoding = "UTF-8")

# local data frame erstellen: wird besser dargestellt
df_sav <- tbl_df(df)
df_r <- tbl_df(data)

# nicht komplette Antworten aussortieren:
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))
df_r <- df_r %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))



# faktorenplots ----------

# Motive --
motive <- df_r %>%
  select(., q_6_1:q_6_16) 

# NAs setzen
motive[motive=="weiß nicht"] <- NA

# fälle mit NAs rauswerfen
motive <- motive %>%
  data.matrix %>%
  na.omit 


# factormap
PCA(motive, graph=F) %>%
  plot.PCA(., choix="var")

# parallel test
ev <- eigen(cor(motive, use = "pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject = nrow(motive), var = ncol(motive), rep = 100, cent = 0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

# -> 5 Faktorenlösung angebracht

fit_pca <- principal(motive, nfactors = 5, rotate = "oblimin")
fit_fa <- fa(motive, nfactors = 5, fm = "pa", rotate = "oblimin")
print.psych(fit_pca, cut = 0.15, sort = TRUE)
print.psych(fit_fa, cut = 0.15, sort = TRUE)




# FEHLT ÜBERPRÜFUNG: GIBT ES WEIß NICHT?!!!!!!! ######
# Zukunft
zukunft <- df_r %>%
  select(., q_19_1:q_19_6) %>%
  data.matrix %>%
  na.omit # fälle mit NAs rauswerfen

# factormap
PCA(zukunft, graph=F) %>%
  plot.PCA(., choix="var")

# parallel test
ev <- eigen(cor(zukunft, use = "pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject = nrow(zukunft), var = ncol(zukunft), rep = 100, quantile = 0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

# -> 2 Faktorenlösung
fit_pca <- principal(zukunft, nfactors = 2, rotate = "oblimin")
fit_fa <- fa(zukunft, nfactors = 2, fm = "pa", rotate = "oblimin")
print.psych(fit_pca, cut = 0.15, sort = TRUE)
print.psych(fit_fa, cut = 0.15, sort = TRUE)




# balkendiagramme für abbruch und unterbrechung --------------
# unterbrechung
unterbrecher <- df_r %>%
  filter(q_14 == "Ja") %>%
  select(., q_15_1:q_15_17)

round(apply(unterbrecher, 2, function(col)sum(col=="Ja")/length(col))*100)

# abbruch
