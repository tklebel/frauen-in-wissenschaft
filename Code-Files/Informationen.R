# allgemein
library(ggplot2)
library(foreign)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(magrittr)
library(scales)
library(ggvis)

# Kreuztabellen
library(gmodels)

# working directory
setwd("/Users/thomask/Dropbox/Soziologie/Frauen\ in\ der\ Wissenschaft/quantitative\ Erhebung/Arbeitsbereich\ Thomas/Berechnungen")

# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))




pdata <- df_sav %>%
  with(., table(q_20_1, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Informationen = q_20_1, Studium = q_1)

plot1 <- ggplot(pdata, aes(Informationen, p, fill=Studium)) 
plot1 + geom_bar(stat="identity", position="dodge") + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Accent") + labs(title="Ablauf und Rahmenbedingungen des Doktoratsstudiums \n (z.B. Studienplan)", y="Prozentanteile innerhalb der Geschlechter") 


df_sav$q_20a

