#################################
### Darstellung der Stichprobe ##
#################################

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

## Studienfächer
df_sav %>%
  select(q_1) %>%
  describe

ggplot(df_sav, aes(q_1)) + geom_bar() + labs(title="Studienfächer", x="Studienrichtung", y="Anzahl")

# nach geschlecht
table(df_sav$q_1, df_sav$q_24)

ggplot(df_sav, aes(q_1, color=q_24)) + geom_bar(aes(fill=q_24), position="dodge") + labs(title="Studienfächer", x="Studienrichtung", y="Anzahl")

# Geschlecht allgemein
df_sav %>%
  select(q_24) %>%
  describe

# Alter allgemein
df_sav %>%
  select(q_25) %>%
  describe

# Alter nach Geschlecht
ggplot(df_sav, aes(q_25, fill=q_24)) + geom_histogram()
ggplot(df_sav, aes(id, q_25, colour=q_24)) + geom_point()
ggplot(df_sav, aes(q_24, q_25)) + geom_boxplot()
ggplot(df_sav, aes(q_24, q_25)) + geom_violin()


# Rigorosum
df_sav %>%
  select(q_99) %>%
  describe

# nach geschlecht
by(df_sav$q_99, df_sav$q_24, describe)
ggplot(df_sav, aes(q_99, color=q_24)) + geom_bar(aes(fill=q_24), position="dodge") 