# Grafiken für den Endbericht

# allgemein
library(ggplot2)
library(foreign)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(magrittr)
library(scales)
library(haven)
library(gridExtra) # arrange mutliple plots with grid.arrange()


# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW-main12_2.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)

df_haven <- read_sav("Data/DATENSATZ_FiW-main12_2.sav")

# link zu den Paletten
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/


# Grafik zu Motiven ------------
motive <- df_sav %>%
  select(., q_6_1:q_6_16)
motive


# prozente für variable 1
Hmisc::describe(motive[,1]) 
Hmisc::describe(motive[,7])

# v sind die prozente für die Variablen
v <- c(61, 35, 4, 0, 42, 25, 23, 10 , 48, 31, 12, 9, 14, 6, 11, 69, 23, 32, 32, 14, 35, 33, 20, 11, 8, 28, 27, 38)
t <- factor(rep(c("trifft zu", "trifft eher zu", "trifft eher nicht zu", "trifft gar nicht zu"), 7), levels=c("trifft zu", "trifft eher zu", "trifft eher nicht zu", "trifft gar nicht zu"))
y <- rep(1:7, each= 4)
test <- data.frame(v, y, t)
labels <- c("Weil mich das Fach interessiert", "Um in Wissenschaft und Forschung\n arbeiten zu können", "Um mich in meinem Beruf weiterzubilden", "Weil ein/e Lehrende/r mir angeboten hat,\n meine Dissertation zu betreuen", "Um am Arbeitsmarkt\n bessere Chancen zu haben", "Um mein Dissertationsthema\n erforschen zu können", "Weil ein Doktor-Titel oft notwendig ist,\n wenn man in der Gesellschaft\n etwas erreichen will")
motivplot <- ggplot(test, aes(y, v, fill=t)) + geom_bar(stat="identity", position="fill", width=.7) + theme_light() + coord_flip() + scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7), labels=labels) + theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=13)) + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Greens")
motivplot

# alternativ in grau
motivplot <- ggplot(test, aes(y, v, fill=t)) + geom_bar(stat="identity", position="fill", width=.7) + theme_light() + coord_flip() + scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7), labels=labels) + theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=13)) + scale_y_continuous(labels = percent_format()) + scale_fill_grey()


# Schwierigkeit, BetreuerIn zu finden ----------
# compute data to plot
pdata <- df_sav %>%
  with(., table(q_9, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Betreuer = q_9, Geschlecht = q_24)

schwierigkeitsplot <- ggplot(pdata, aes(Betreuer, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() + scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette="Paired") +
  labs(y="Prozentanteile innerhalb der Geschlechter", x = "Schwierigkeit, eine/n BetreuerIn zu finden") 
schwierigkeitsplot

rm(pdata)

# Motivationsindizes ----------
# 4 Indizes zur Motivation, gesplittet nach Geschlecht, mit Violin+Boxplot+Punkt für Mittelwert
# Variabennamen: Inst_Einbindung_Motivation, Verlegenheit_Motivation, Wi_Interesse_Motivation, Prestige_Motivation

describe(factor(df_haven$q_24, labels=c("weiblich", "männlich")))

# select data to plot
df_haven %>%
  select(contains("Motivation"), q_24)  %>% 
  as.matrix %>% # get rid of "labelled" class which doesn't work with dplyr right now
  data.frame %>%
  mutate(q_24 = factor(q_24, labels=c("weiblich", "männlich")))  %>% 
  filter(q_24 != "NA") -> pdata # personen rausschmeißen, die als Geschlecht NA haben


# Inst-Einbindung
p1 <- ggplot(pdata, aes(q_24, Inst_Einbindung_Motivation)) +
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Institutionelle Einbindung als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Skalenwert") +
  ylim(c(1, 12.5)) + # extend y scale to range of data
  theme(legend.position = "none") # remove superflous legend

  

# Verlegenheit
p2 <- ggplot(pdata, aes(q_24, Verlegenheit_Motivation)) +
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
  geom_boxplot(width = .08, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 3.5, shape = 4) +
  labs(title = "Verlegenheit als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Skalenwert") +
  ylim(c(1, 12.5)) + 
  theme(legend.position = "none") # remove superflous legend


# Wi_interesse
p3 <- ggplot(pdata, aes(q_24, Wi_Interesse_Motivation)) +
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Wissenschaftliches Interesse als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Skalenwert") +
  ylim(c(1, 12.5)) + 
  theme(legend.position = "none") # remove superflous legend


# Prestige_Motivation
p4 <- ggplot(pdata, aes(q_24, Prestige_Motivation)) +
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Prestige als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Skalenwert") +
  ylim(c(1, 12.5)) + 
  theme(legend.position = "none") # remove superflous legend


# zusammenführen
grid.arrange(p1, p2, p3, p4)


# ToDo
# Farben anpassen