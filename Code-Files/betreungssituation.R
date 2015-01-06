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
