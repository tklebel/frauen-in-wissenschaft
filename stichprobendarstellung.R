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
library(scales)

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
ggplot(df_sav, aes(q_24, q_25)) + geom_violin(adjust=.5, scale= "width")


# Rigorosum
df_sav %>%
  select(q_99) %>%
  describe

# nach geschlecht
by(df_sav$q_99, df_sav$q_24, describe)

# compute data to plot
pdata <- df_sav %>%
  with(., table(q_99, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Rigorosum = q_99, Geschlecht = q_24)

plot1 <- ggplot(pdata, aes(Rigorosum, p)) 
plot1 + geom_bar(stat="identity", position="dodge", aes(fill=Geschlecht)) + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired") + labs(title="Abschluss des Rigorosums", y="Prozentanteile innerhalb der Geschlechter") 
