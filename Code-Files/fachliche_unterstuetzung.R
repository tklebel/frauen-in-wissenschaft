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

# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))



# Qualitative Antworten
df_sav$q_21a[24]
df_sav$q_21a[68]


# plots für die einzelnen Variablen
# nach geschlecht

# fachkundige
pdata <- df_sav %>%
  with(., table(q_21_1, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Fachkundige = q_21_1, Geschlecht = q_24)

plot1 <- ggplot(pdata, aes(Fachkundige, p, fill=Geschlecht)) 
plot1 + geom_bar(stat="identity", position="dodge") + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired") + labs(title="Informationen von fachkundigen Personen außerhalb Universität", y="Prozentanteile innerhalb der Geschlechter") 

# frauen deutlich häufiger -> warum? wie ist das in den disziplinen?

# betreuer_in
pdata <- df_sav %>%
  with(., table(q_21_2, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Betreuer_in = q_21_2, Geschlecht = q_24)

plot1 <- ggplot(pdata, aes(Betreuer_in, p, fill=Geschlecht)) 
plot1 + geom_bar(stat="identity", position="dodge")  + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired") + labs(title="Informationen von Betreuer_in", y="Prozentanteile innerhalb der Geschlechter") 



# wiss. mitarbeiter an SOWI
pdata <- df_sav %>%
  with(., table(q_21_3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., wissMitarbeiter_in = q_21_3, Geschlecht = q_24)

plot1 <- ggplot(pdata, aes(wissMitarbeiter_in, p, fill=Geschlecht)) 
plot1 + geom_bar(stat="identity", position="dodge") + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired") + labs(title="Informationen von wissenschafltichem/r Mitarbeiter_in", y="Prozentanteile innerhalb der Geschlechter") 

# obwohl niveau niedrig ist: männer haben mehr kontakt

# Studienkolleg_innen
pdata <- df_sav %>%
  with(., table(q_21_4, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Studienkolleg_innen = q_21_4, Geschlecht = q_24)

plot1 <- ggplot(pdata, aes(Studienkolleg_innen, p, fill=Geschlecht)) 
plot1 + geom_bar(stat="identity", position="dodge") + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired") + labs(title="Informationen von Studienkolleg_innen", y="Prozentanteile innerhalb der Geschlechter") 


# Studienkolleg_innen mit facet_grid nach Studienrichtung
pdata <- df_sav %>%
  with(., table(q_21_4, q_24, q_1_a)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24, q_1_a) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Studienkolleg_innen = q_21_4, Geschlecht = q_24)

plot1 <- ggplot(pdata, aes(Studienkolleg_innen, p, fill=Geschlecht)) 
plot1 + geom_bar(stat="identity", position="dodge") + facet_grid(.~ q_1_a) + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired") + labs(title="Informationen von Studienkolleg_innen", y="Prozentanteile innerhalb der Geschlechter") 


