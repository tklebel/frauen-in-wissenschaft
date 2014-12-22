###############################################################################
### Berechnung für: Im wievielten Semester des Dok-Studiums sind die Studis? ##
###############################################################################

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
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))

# wie lange studieren sie Doktorat? #####################

# neuen vektor erstellen mit WS / SS
# 7-12 = WS, 1-6 = SS

df_sav %>%
  select(q_5_1) 

# sortieren, ob sommer oder wintersemester
sem <- with(df_sav, recode(q_5_1, "1:6='SS'; 7:12='WS'")) %>%
  factor

df_sav %>%
  select(q_5_2, sem) 

# berechnen Studiendauer, so dass 1 == WS 2014
# Basis -> WS 2014

# berechnung für WS = T

(2014-2013)*2 + 1


# SS = T
(2014-2013)*2 + 2


# Berechnung ########
# Ausgangspunkt 
basis <- 2014

# verwendete Daten
d <- data.frame(df_sav$sem, df_sav$q_5_2, df_sav$id)

d_1 <- d %>%
  filter(df_sav.sem=="SS") %>%
  mutate(dauer = (basis - df_sav.q_5_2)*2 + 2)

d_2 <- d %>%
  filter(df_sav.sem=="WS") %>%
  mutate(dauer = (basis - df_sav.q_5_2)*2 + 1)

d_3 <- d %>% # auch NAs mit filtern, damit alle Fälle bleiben
  filter(df_sav.sem %nin% c("WS", "SS")) %>%
  mutate(dauer = NA)

d_end <- rbind(d_1, d_2, d_3)

d_end %>%
  arrange(df_sav.id)
 
# fertiger Vektor. ÜBERPRÜFEN!!!!!!!!!!!!!
d_end <- d_end %>%
  arrange(df_sav.id)

d_end$dauer %>%
  median(., na.rm=T)

# plot 1
d_end$dauer[order(d_end$dauer, decreasing=F)] %>%
  barplot(., names=d_end$dauer[order(d_end$dauer, decreasing=F)])
title(main="Absolute Studiendauern", xlab="Dauer des Studiums in Semestern")

# plot 2
describe(d_end$dauer)
describe(d_end$dauer)$values[2,] %>%
  barplot(., names=attributes(describe(d_end$dauer)$values)$dimnames[[2]])
title(main="Studiendauer", ylab="Angaben in Prozent", xlab="Semester in dem die Studierenden sind")