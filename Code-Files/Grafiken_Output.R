#!/usr/bin/env Rscript

# Grafiken für den Endbericht

# working directory für script
setwd("/Users/thomask/Dropbox/Soziologie/Frauen\ in\ der\ Wissenschaft/quantitative\ Erhebung/Arbeitsbereich\ Thomas/Berechnungen")


# allgemein
library(ggplot2)
library(foreign)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(scales)
library(haven)
library(gridExtra) # arrange mutliple plots with grid.arrange()
library(RColorBrewer) # Farbton finden

# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)

df_haven <- read_sav("Data/DATENSATZ_FiW-main12_2.sav")



# #A1D99B für Männer (Grün) als Grundfarbe 
# #4292C6 für Frauen (Blau)
# #EF3B2C für gesamt (Rot)

# Basisgrün: #74C476

# Farben für "scale_fill_manual"
colours <- c(Mann = "#A1D99B", Frau = "#4292C6", gesamt = "#EF3B2C", weiblich = "#4292C6", männlich = "#A1D99B")



############################################ Plots ##################################
# Grafik zu Motiven ------------
# vorgehensweise: long format: alle variablen in eine spalte transferieren

# Farben für skala
colours_skala <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#BAE4B3", "trifft gar nicht zu" = "#EDF8E9")
colours_skala_blue_green <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#9ECAE1", "trifft gar nicht zu" = "#4292C6")
colours_skala_blue_green_sw <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#C6DBEF", "trifft gar nicht zu" = "#9ECAE1")


# variablennamen für den motivationsplot
labels_motivplot <- c("Weil mich das Fach interessiert", 
                      "Um in Wissenschaft und Forschung\n arbeiten zu können", 
                      "Um mich in meinem Beruf weiterzubilden", 
                      "Weil ein/e Lehrende/r mir angeboten hat,\n meine Dissertation zu betreuen", 
                      "Um am Arbeitsmarkt\n bessere Chancen zu haben", 
                      "Um mein Dissertationsthema\n erforschen zu können", 
                      "Weil ein Doktor-Titel oft notwendig ist,\n wenn man in der Gesellschaft\n etwas erreichen will", 
                      "Weil es in meinem Job erwartet wird", 
                      "Weil es mir Lehrende an der Universität\n geraten haben", 
                      "Um länger StudentIn sein zu können", 
                      "Weil ich keinen adäquaten Arbeitsplatz\n gefunden habe", 
                      "Weil ich ein Doktoratsstipendium bekommen habe", 
                      "Weil ich aus familiären Gründen\n nicht erwerbstätig war und die Zeit\n sinnvoll nützen wollte", 
                      "Weil mir wissenschaftliches Arbeiten Spaß macht", 
                      "Weil ich mit einem Doktoratsabschluss\n ein höheres Einkommen erzielen kann", 
                      "Weil ich ein Arbeitsangebot (z.B. Projektmitarbeit)\n an der Universität bekommen habe")


# get number of valid observations for further computation of percentages
cases <- df_haven %>%
  select(., q_6_1:q_6_16)
cases <- colSums(!is.na(cases))

# get counts of first two levels, in order to get order of variables for plot
reihenfolge <- df_sav %>%
  select(q_6_1:q_6_16) %>% 
  summarise_each(., funs(sum(.== "trifft zu" | . == "trifft eher zu", na.rm = T))) %>% # summiere die ausprägungen für die ersten beiden levels
  gather(id, häufigkeit) %>%
  mutate(häufigkeit = häufigkeit / cases) %>% # divide counts by cases for correct percentages
  cbind(., labels_motivplot) 

# select data to plot and gather it in long format, remove NAs
motive <- df_sav %>%
  select(q_6_1:q_6_16) %>%
  gather(., id, variable) %>%
  na.omit

# join datasets
motive <- full_join(motive, reihenfolge, by = "id")

# reorder the levels for the plot
motive$variable <- factor(motive$variable, levels = c("trifft zu", "trifft eher zu", "trifft eher nicht zu", "trifft gar nicht zu"))
motive$labels_motivplot <- factor(motive$labels_motivplot, levels = motive$labels_motivplot[order(motive$häufigkeit)])

# plot data
motivplot <- ggplot(motive, aes(labels_motivplot, fill = variable))  +
  geom_bar(position = "fill", width = .7) +
  coord_flip() +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  theme(legend.position=c(.8, .19), axis.text.y = element_text(size = 11),
        legend.key.size = unit(1.2, "cm"),
        legend.text=element_text(size=11)) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL) # remove labels of axes and legend

ggsave(filename = "Grafiken/Motive_Studienanfang.png", plot = motivplot, dpi = 150, width = 12.8, height = 7.73)



# clean up 
rm(motive, reihenfolge, labels_motivplot, motivplot, cases)


# Motivationsindizes ----------
# 4 Indizes zur Motivation, gesplittet nach Geschlecht, mit Violin+Boxplot+Punkt für Mittelwert
# Variabennamen: Inst_Einbindung_Motivation, Verlegenheit_Motivation, Wi_Interesse_Motivation, Prestige_Motivation

# select data to plot
df_haven %>%
  select(contains("Motivation"), q_24)  %>% 
  as.matrix %>% # get rid of "labelled" class which doesn't work with dplyr right now
  data.frame %>%
  mutate(q_24 = factor(q_24, labels=c("weiblich", "männlich")))  %>% 
  filter(q_24 != "NA") -> pdata # personen rausschmeißen, die als Geschlecht NA haben


# Inst-Einbindung
p1 <- ggplot(pdata, aes(q_24, Inst_Einbindung_Motivation)) +
  theme_bw() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 4, shape = 4) +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 9.5)) + # reduce title size
  labs(title = "Institutionelle Einbindung als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_y_continuous(limits = c(1, 12.5), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# Verlegenheit
p2 <- ggplot(pdata, aes(q_24, Verlegenheit_Motivation)) +
  theme_bw() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .8, width = 1) + 
  geom_boxplot(width = .08, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, shape = 4) +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 9.5)) + # reduce title size
  labs(title = "Verlegenheit als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_y_continuous(limits = c(1, 12.5), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# Wi_interesse
p3 <- ggplot(pdata, aes(q_24, Wi_Interesse_Motivation)) +
  theme_bw() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .4, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 4, shape = 4) +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 9.5)) + # reduce title size
  labs(title = "Wissenschaftliches Interesse als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_y_continuous(limits = c(1, 12.5), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# Prestige_Motivation
p4 <- ggplot(pdata, aes(q_24, Prestige_Motivation)) +
  theme_bw() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .4, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 4, shape = 4) +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 9.5)) + # reduce title size
  labs(title = "Prestige als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_y_continuous(limits = c(1, 12.5), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# zusammenführen
png("Grafiken/Motivationsindizes.png", width = 1600, height = 1200, res = 200)
grid.arrange(p1, p2, p3, p4)
dev.off()

# clean up
rm(pdata, p1, p2, p3, p4)

# Nachgedacht, abzubrechen ------

# select variables and convert them to factor
# q_15_16 hat keine ausprägungen, kann deshalb weggelassen werden
df_haven %>%
  select(q_14:q_15_15, q_15_17, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  filter(q_14 == "Ja") %>%
  tbl_df -> abbruchgedanken

# split for gender
abbruchgedanken %>%
  filter(q_24 == "männlich")  %>%
  select(q_15_1:q_15_17) -> abbruchgedanken_m

abbruchgedanken %>%
  filter(q_24 == "weiblich")  %>%
  select(q_15_1:q_15_17) -> abbruchgedanken_w

# Variablennamen (q_15_1:q_15_17, ohne q_15_16)

# create factor for variable names, in the right order
varname <- c("Unzufriedenheit mit dem Studium",
             "Fehlende Aussicht auf institutionelle\n Einbindung an einer Universität",
             "Probleme bei der Finanzierung des Doktoratsstudiums",
             "Mangelnde Vereinbarkeit mit Berufstätigkeit",
             "Mangelnde Vereinbarkeit\nmit Betreuungspflichten",
             "Attraktive Arbeit gefunden",
             "Erwartungen an meine Leistungen nicht erfüllbar",
             "Interesse verloren",
             "Stillstand bei der Dissertation",
             "Schwierigkeiten eine/n BetreuerIn zu finden",
             "Nur nebenbei studiert",
             "Fehlende Unterstützung durch den/die BetreuerIn",
             "Keine befriedigenden Berufsaussichten\n mit dem Doktoratsabschluss",
             "Doktoratsstudium ist zu schwierig",
             "Doktoratsstudium als zeitliche Überbrückung gedacht",
             "Kind bekommen bzw. werde ein Kind bekommen")
varname <- factor(varname)


## Männer ##
# wie oft wurde "gewählt"? 
abbruchgedanken_m %>%
  summarise_each(funs(sum(. == "Ja"))) %>%
  gather("variable", "anzahl") %>%
  cbind(., varname) %>%
  arrange(anzahl) %>%
  slice(11:16) -> pdata_m

# reorder the levels for the plot, by the frequency of the "anzahl"
pdata_m$varname <- factor(pdata_m$varname, levels = pdata_m$varname[order(pdata_m$anzahl)])

# plot
abbruchplot_männer <- ggplot(pdata_m, aes(varname, anzahl)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "#A1D99B", width = .7) +
  coord_flip() +
  labs(y = "Häufigkeit der Nennung", x = NULL, title = "Männer") +
  scale_y_continuous(breaks = pretty_breaks(8)) +
  theme(axis.text.y = element_text(size = 12)) 


## Frauen ##
# wie oft wurde "gewählt"? 
abbruchgedanken_w %>%
  summarise_each(funs(sum(. == "Ja"))) %>%
  gather("variable", "anzahl") %>%
  cbind(., varname) %>%
  arrange(anzahl) %>%
  slice(12:16) -> pdata_w

# bei Frauen: nach 5 Variablen abschneiden, da 6 und 7 doppelt, bei männern nicht. Insgeamt wurde bei weniger als 4 Nennungen abgeschnitten

# reorder the levels for the plot, by the frequency of the "anzahl"
pdata_w$varname <- factor(pdata_w$varname, levels = pdata_w$varname[order(pdata_w$anzahl)])

# plot
abbruchplot_frauen <- ggplot(pdata_w, aes(varname, anzahl)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "#4292C6", width = .7) +
  coord_flip() +
  labs(y = "Häufigkeit der Nennung", x = NULL, title = "Frauen") +
  scale_y_continuous(breaks = pretty_breaks(8)) +
  theme(axis.text.y = element_text(size = 12)) 

# join plots
png("Grafiken/Nachgedacht_abzubrechen_breit.png", width = 2000, height = 500, res = 150)
grid.arrange(abbruchplot_frauen, abbruchplot_männer, nrow = 1)
dev.off()

png("Grafiken/Nachgedacht_abzubrechen_hoch.png", width = 1100, height = 1000, res = 150)
grid.arrange(abbruchplot_frauen, abbruchplot_männer, nrow = 2)
dev.off()

# clean up
rm(abbruchplot_frauen, abbruchplot_männer, varname, pdata_m, pdata_w, abbruchgedanken, abbruchgedanken_m, abbruchgedanken_w)



# Stichprobendarstellung ------------------
# das Vorgehen ist ein dämlicher Hack. Eigentlich wäre es wohl leichter, gleich mit den Prozenten zu rechen,
# da man sie ja eh für die Grafik dann braucht. 


# Geschlechterverteilung
# Grundgesamtheit erstellen
# BWL
# w = 118
d1 <- data.frame(
  Geschlecht = factor(rep(1, times = 118), levels = c(1, 2), labels = c("weiblich", "männlich")),
  Studienrichtung = factor(rep(1, times = 118), levels = c(1, 2, 3), labels = c("BWL", "SOZ", "VWL")),
  herkunft = factor(rep(1, times = 118), levels = c(1, 2), labels = c("Grundgesamtheit", "Stichprobe"))
)
# m = 140
d2 <- data.frame(
  Geschlecht = factor(rep(2, times = 140), levels = c(1, 2), labels = c("weiblich", "männlich")),
  Studienrichtung = factor(rep(1, times = 140), levels = c(1, 2, 3), labels = c("BWL", "SOZ", "VWL")),
  herkunft = factor(rep(1, times = 140), levels = c(1, 2), labels = c("Grundgesamtheit", "Stichprobe"))
)


# SOZ
# w = 53
d3 <- data.frame(
  Geschlecht = factor(rep(1, times = 53), levels = c(1, 2), labels = c("weiblich", "männlich")),
  Studienrichtung = factor(rep(2, times = 53), levels = c(1, 2, 3), labels = c("BWL", "SOZ", "VWL")),
  herkunft = factor(rep(1, times = 53), levels = c(1, 2), labels = c("Grundgesamtheit", "Stichprobe"))
)
# m = 24
d4 <- data.frame(
  Geschlecht = factor(rep(2, times = 24), levels = c(1, 2), labels = c("weiblich", "männlich")),
  Studienrichtung = factor(rep(2, times = 24), levels = c(1, 2, 3), labels = c("BWL", "SOZ", "VWL")),
  herkunft = factor(rep(1, times = 24), levels = c(1, 2), labels = c("Grundgesamtheit", "Stichprobe"))
)

# VWL
# w = 10
d5 <- data.frame(
  Geschlecht = factor(rep(1, times = 10), levels = c(1, 2), labels = c("weiblich", "männlich")),
  Studienrichtung = factor(rep(3, times = 10), levels = c(1, 2, 3), labels = c("BWL", "SOZ", "VWL")),
  herkunft = factor(rep(1, times = 10), levels = c(1, 2), labels = c("Grundgesamtheit", "Stichprobe"))
)
# m = 23
d6 <- data.frame(
  Geschlecht = factor(rep(2, times = 23), levels = c(1, 2), labels = c("weiblich", "männlich")),
  Studienrichtung = factor(rep(3, times = 23), levels = c(1, 2, 3), labels = c("BWL", "SOZ", "VWL")),
  herkunft = factor(rep(1, times = 23), levels = c(1, 2), labels = c("Grundgesamtheit", "Stichprobe"))
)

# zusammenführen
rbind(d1, d2, d3, d4, d5, d6) %>%
  tbl_df -> Grundgesamtheit

# aufräumen
rm(d1, d2, d3, d4, d5, d6)


# Plotdaten erstellen
pdata <- df_haven %>%
  select(q_1, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  mutate(q_1 = recode(q_1, "1 = 1; 2 = 2; 3 = 3; 4 = 1")) %>%
  mutate(q_1 = labelled(q_1, c(BWL = 1, SOZ = 2, VWL = 3))) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  rename(Studienrichtung = q_1, Geschlecht = q_24) %>%
  mutate(herkunft = factor(rep(2, times = 82), levels = c(1, 2), labels = c("Grundgesamtheit", "Stichprobe"))) # zuordnung zur stichprobe erstellen

pdata <- bind_rows(pdata, Grundgesamtheit) %>%
  mutate(Geschlecht = as.factor(Geschlecht))  %>% # hack around error "unequal levels in factor"
  na.omit

# Daten prozentuieren
pdata <- pdata %>% 
  group_by(Studienrichtung, herkunft, Geschlecht) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

# Plot
Stichprobenplot <- ggplot(pdata, aes(x = herkunft, y = perc, fill = Geschlecht)) +
  geom_bar(stat = "identity", width = .8) +
  facet_wrap(~ Studienrichtung) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colours) +
  labs(x = NULL, y = "Prozentanteile nach Geschlecht") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

# n's zu plot hinzufügen
p <- arrangeGrob(Stichprobenplot, sub = textGrob(c("n = 258", "n = 40", "n = 77", "n = 32", "n = 33", "n = 6"),
                                                 x = c(0.013, .152, .29, .40, .537, .675),
                                                 hjust = -3.2, vjust = -2.8,
                                                 gp = gpar(fontsize = 10, col = "grey40")))


ggsave("Grafiken/Stichprobendarstellung.png", p, width = 11, height = 7.2, dpi = 100)

rm(pdata)

# Informationen über das Studium --------
# Plot analog zu den Motiven

# Farben für skala
colours_skala <- c("mehr als genug" = "#238B45", "ausreichend" = "#74C476", "zu wenig" = "#BAE4B3", "ist für mich nicht wichtig" = "#EDF8E9")
colours_skala_blue_green <- c("mehr als genug" = "#238B45", "ausreichend" = "#74C476", "zu wenig" = "#9ECAE1", "ist für mich nicht wichtig" = "#4292C6")
colours_skala_blue_green_sw <- c("mehr als genug" = "#238B45", "ausreichend" = "#74C476", "zu wenig" = "#C6DBEF", "ist für mich nicht wichtig" = "#9ECAE1")

# variablennamen für den motivationsplot
labels_infoplot <- c("Ablauf und Rahmenbedingungen des\n Doktoratsstudiums (z.B. Studienplan)", 
                     "Verfassen von Forschungsanträgen",
                     "Möglichkeiten für finanzielle Förderungen\n während des Doktoratsstudiums (z.B. Stipendien)", 
                     "Publikationsmöglichkeiten", 
                     "Auslandsaufenthalte im Rahmen des Studiums", 
                     "Informationen zu wissenschaftlichen\n Konferenzen/Tagungen", 
                     "Berufliche Perspektiven in der Wissenschaft", 
                     "Wissenschaftsinterne Abläufe\n (z.B. informeller Informationsaustausch,\n Netzwerkaufbau)")

# get frequencies of first two levels, in order to get order of variables for plot
reihenfolge <- df_sav %>%
  select(q_20_1:q_20_8) %>%
  summarise_each(., funs(sum(.== "mehr als genug" | . == "ausreichend", na.rm = T))) %>% # summiere die ausprägungen für die ersten beiden levels
  gather(id, häufigkeit) %>%
  cbind(., labels_infoplot) 

# select data to plot and gather it in long format, remove NAs
infos <- df_sav %>%
  select(q_20_1:q_20_8) %>%
  gather(., id, variable) %>%
  na.omit

# join datasets
infos <- full_join(infos, reihenfolge, by = "id")

# reorder the levels for the plot
infos$variable <- factor(infos$variable, levels = c("mehr als genug", "ausreichend", "zu wenig", "Ist für mich nicht wichtig"))
infos$labels_infoplot <- factor(infos$labels_infoplot, levels = infos$labels_infoplot[order(infos$häufigkeit)])
levels(infos$variable)[levels(infos$variable)=="Ist für mich nicht wichtig"] <- "ist für mich nicht wichtig" # level des Factors anpassen (Kleinschreibung)

# plot data
infoplot <- ggplot(infos, aes(labels_infoplot, fill = variable))  +
  geom_bar(position = "fill", width = .7) +
  coord_flip() +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = "Haben Sie genügend\nInformationen?") # remove labels of axes and legend
infoplot

ggsave(filename = "Grafiken/Informationen.png", plot = infoplot, dpi = 150, width = 10, height = 4)


# clean up 
rm(infos, reihenfolge, labels_infoplot, infoplot)


# q_23: XXX schon mal gemacht?? ####################################################
# q_23_1:q_23:8

# q_23_1
pdata <- df_haven %>%
  select(q_23_1, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_1, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_1, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_1)$label)

rm(pdata)

# q_23_2
pdata <- df_haven %>%
  select(q_23_2, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_2, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_2, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_2)$label)

rm(pdata)

# q_23_3
pdata <- df_haven %>%
  select(q_23_3, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_3, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_3, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_3)$label)

rm(pdata)

# q_23_4
pdata <- df_haven %>%
  select(q_23_4, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_4, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_4, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_4)$label)

rm(pdata)

# q_23_5
pdata <- df_haven %>%
  select(q_23_5, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_5, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_5, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_5)$label)

rm(pdata)

# q_23_6
pdata <- df_haven %>%
  select(q_23_6, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_6, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_6, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_6)$label)

rm(pdata)

# q_23_7
pdata <- df_haven %>%
  select(q_23_7, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_7, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_7, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_7)$label)

rm(pdata)

# q_23_8
pdata <- df_haven %>%
  select(q_23_8, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_8, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24)


ggplot(pdata, aes(q_23_8, p, fill=studiendauer_2_bis3)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~ Geschlecht) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_8)$label)


# Schwierigkeit, BetreuerIn zu finden ----------------------------------------
# compute data to plot
pdata <- df_sav %>%
  with(., table(q_9, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Betreuer = q_9, Geschlecht = q_24)

schwierigkeitsplot <- ggplot(pdata, aes(Betreuer, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  scale_fill_manual(values = colours) +
  labs(y="Prozentanteile innerhalb der Geschlechter",
       x = "Schwierigkeit, eine/n BetreuerIn zu finden",
       fill = "Geschlecht des/der\nStudierenden") 

ggsave(filename = "Grafiken/Schwierigkeit_betreuer_finden.png", plot = schwierigkeitsplot, dpi = 150, width = 8, height = 5.6)


# clean up
rm(pdata)


## Geschlecht der Studierenden abhängig von Geschlecht BetreuerIn ###########
# compute data to plot
pdata <- df_sav %>%
  with(., table(q_8, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Betreuer = q_8, Geschlecht = q_24)

betreuerplot_1 <- ggplot(pdata, aes(Geschlecht, p, fill=Betreuer)) +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colours) +
  labs(y    = "Prozentanteile innerhalb Geschlecht des/der Studierenden",
       fill = "Geschlecht des/der\nBetreuerIn",
       x    = "Geschlecht des/der Studierenden")

betreuerplot_2 <- ggplot(pdata, aes(Betreuer, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colours) +
  labs(y    = "Prozentanteile innerhalb Geschlecht des/der Studierenden",
       x    = "Geschlecht des/der BetreuerIn",
       fill = "Geschlecht des/der\nStudierenden")


ggsave(filename = "Grafiken/Geschlecht_betreuer_v1.png", plot = betreuerplot_1, dpi = 150, width = 8, height = 6.5)
ggsave(filename = "Grafiken/Geschlecht_betreuer_v2.png", plot = betreuerplot_2, dpi = 150, width = 8, height = 6.5)


rm(pdata, betreuerplot_1, betreuerplot_2)


# copy all graphs and the html documentation to delivery folder
filelist <- list.files("Grafiken", pattern = "png|html", full.names = TRUE)
file.copy(filelist, "../Fertige_Grafiken")




