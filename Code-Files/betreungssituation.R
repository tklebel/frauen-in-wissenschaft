##########################################
### Darstellung der Betreuungssituation ##
##########################################

# allgemein
library(ggplot2)
library(foreign)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyr)

# Kreuztabellen
library(gmodels)

# working directory
setwd("/Users/thomask/Dropbox/Soziologie/Frauen\ in\ der\ Wissenschaft/quantitative\ Erhebung/Arbeitsbereich\ Thomas/Berechnungen")

# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))


# Betreuer_in ja/nein? q_7
df_sav %>%
  select(q_7) %>%
  describe

# nach geschlecht
by(df_sav$q_7, df_sav$q_24, describe)
CrossTable(df_sav$q_7, df_sav$q_24, prop.r=F, prop.t=F, prop.chisq=F)

##  Schwierigkeit q_9
describe(df_sav$q_9)

# nach geschlecht
by(df_sav$q_9, df_sav$q_24, describe)
CrossTable(df_sav$q_9, df_sav$q_24, prop.r=F, prop.t=F, prop.chisq=F)

# nach Master-Herkunft
by(df_sav$q_9, df_sav$q_3, describe)
CrossTable(df_sav$q_9, df_sav$q_3, prop.r=F, prop.t=F, prop.chisq=F)


# gründe für Betreuerwahl
df_sav %>%
  select(q_11_1) %>%
  describe


betreuer <- df_sav %>%
  filter(q_7 == "Ja") %>%
  select(., q_11_1:q_11_6)

# berechne die Prozente
d <- round(apply(betreuer, 2, function(col)sum(col=="Ja")/length(col))*100)
d <- as.data.frame(d)

f <- function(x) {
  x == "Ja"
}

pdata <- betreuer %>%
  summarise_each(., funs(sum(f(.))/n()))

df_sav %>%
  select(q_11_other) %>%
  na.omit


attributes(pdata)$names <- c("Weil er/sie bereits meine Master-/Diplomarbeit betreut hat", "Weil er/sie in meinem Dissertationsbereich fachlich kompetent ist", "Weil er/sie mir von anderen empfohlen wurde", "Weil er/sie mich gut motivieren kann", "Weil er/sie mich angesprochen hat", "Weil er/sie mir sympathisch ist")

par(mar=c(5, 18, 4, 2) + 0.1)
pdata[order(pdata, decreasing=F)] %>%
  as.matrix %>%
  barplot(., horiz=T, las=1, cex.names=.7)

par(mar=c(5, 4, 4, 2) + 0.1)

# Prozentangaben: mean = Prozente
pdata[order(pdata, decreasing=T)] %>%
  describe


CrossTable(df_sav$q_14, df_sav$q_24, prop.r=F, prop.t=F, prop.chisq=F)
