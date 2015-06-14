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
df_haven_neu <- read_sav("Data/DATENSATZ_FiW-main10-4-2015.sav")


# #A1D99B für Männer (Grün) als Grundfarbe 
# #4292C6 für Frauen (Blau)
# #EF3B2C für gesamt (Rot)

# Basisgrün: #74C476

# Farben für "scale_fill_manual" oder "scale_colour_manual"
colours <- c(Mann = "#A1D99B", Frau = "#4292C6", gesamt = "#EF3B2C",
             weiblich = "#4292C6", männlich = "#A1D99B",
             BWL = "#A1D99B", SOZ = "#4292C6", VWL = "#EF3B2C", WiPäd = "#807DBA")


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
  theme(legend.position = c(.8, .16),
        axis.text = element_text(size = 12.5),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 11),
        legend.background = element_rect(fill = "#F7FBFF")) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL) # remove labels of axes and legend


ggsave(filename = "Grafiken/Motive_Studienanfang.png", plot = motivplot, dpi = 150, width = 11.5, height = 9)



# clean up 
rm(motive, reihenfolge, labels_motivplot, motivplot, cases)


# Motivationsindizes ----------
# 4 Indizes zur Motivation, gesplittet nach Geschlecht, mit Violin+Boxplot+Punkt für Mittelwert
# Variabennamen: Inst_Einbindung_Motivation, Verlegenheit_Motivation, Wi_Interesse_Motivation, Prestige_Motivation

# select data to plot
df_haven %>%
  select(contains("Motivation"), q_24)  %>% 
  mutate(q_24 = factor(q_24, labels = c("weiblich", "männlich")))  %>% 
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
  labs(title = "Institutionelle Einbindung als Motivation",
       x = NULL,
       y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_x_discrete(labels = c("weiblich\n(n = 33)", "männlich\n(n = 46)")) + 
  scale_y_continuous(limits = c(1, 12), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# Verlegenheit
p2 <- ggplot(pdata, aes(q_24, Verlegenheit_Motivation)) +
  theme_bw() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .8, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 4, shape = 4) +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 9.5)) + # reduce title size
  labs(title = "Verlegenheit als Motivation",
       x = NULL,
       y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_x_discrete(labels = c("weiblich\n(n = 33)", "männlich\n(n = 45)")) + 
  scale_y_continuous(limits = c(1, 6), breaks = c(1, 3.5, 6), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# Wi_interesse
p3 <- ggplot(pdata, aes(q_24, Wi_Interesse_Motivation)) +
  theme_bw() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .4, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 4, shape = 4) +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 9.5)) + # reduce title size
  labs(title = "Wissenschaftliches Interesse als Motivation",
       x = NULL,
       y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_x_discrete(labels = c("weiblich\n(n = 33)", "männlich\n(n = 46)")) + 
  scale_y_continuous(limits = c(1, 12), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# Prestige_Motivation
p4 <- ggplot(pdata, aes(q_24, Prestige_Motivation)) +
  theme_bw() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .4, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  stat_summary(fun.y = "mean", geom = "point", size = 4, shape = 4) +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 9.5)) + # reduce title size
  labs(title = "Prestige als Motivation",
       x = NULL,
       y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_x_discrete(labels = c("weiblich\n(n = 33)", "männlich\n(n = 44)")) + 
  scale_y_continuous(limits = c(1, 12), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# zusammenführen
png("Grafiken/Motivationsindizes.png", width = 1600, height = 1200, res = 200)
grid.arrange(p1, p2, p3, p4)
dev.off()

png("../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_5.png", width = 1600, height = 1200, res = 200)
grid.arrange(p1, p2, p3, p4)
dev.off()

# clean up
rm(pdata, p1, p2, p3, p4)

# Nachgedacht, abzubrechen ------

# select variables and convert them to factor
# q_15_16 hat keine ausprägungen, kann deshalb weggelassen werden
df_haven %>%
  select(q_14:q_15_15, q_15_17, q_24) %>%
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
  labs(y = "Häufigkeit der Nennung", x = NULL, title = "Männlich") +
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
  labs(y = "Häufigkeit der Nennung", x = NULL, title = "Weiblich") +
  scale_y_continuous(breaks = pretty_breaks(8)) +
  theme(axis.text.y = element_text(size = 12)) 

# join plots
png("Grafiken/Nachgedacht_abzubrechen.png", width = 1100, height = 1000, res = 150)
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
  facet_wrap(~Studienrichtung) +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colours) +
  labs(x = NULL, y = "Prozentanteile nach Geschlecht") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.text = element_text(size = 12)) 

# n's zu plot hinzufügen
p <- arrangeGrob(Stichprobenplot, sub = textGrob(c("n = 258", "n = 40", "n = 77", "n = 32", "n = 33", "n = 6"),
                                                 x = c(0.013, .145, .285, .395, .528, .665),
                                                 hjust = -2.7, vjust = -2.5,
                                                 gp = gpar(fontsize = 12, col = "grey40")))


ggsave("Grafiken/Stichprobendarstellung.png", p, width = 11, height = 7.2, dpi = 100)
ggsave("../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_1.png", p, width = 11, height = 7.2, dpi = 100)

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
  summarise_each(., funs(sum(. == "mehr als genug" | . == "ausreichend", na.rm = T))) %>% # summiere die ausprägungen für die ersten beiden levels
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
levels(infos$variable)[levels(infos$variable) == "Ist für mich nicht wichtig"] <- "ist für mich nicht wichtig" # level des Factors anpassen (Kleinschreibung)

# plot data
infoplot <- ggplot(infos, aes(labels_infoplot, fill = variable))  +
  geom_bar(position = "fill", width = .7) +
  coord_flip() +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = percent_format()) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 11),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14)) +
  labs(x = NULL, y = NULL, fill = "Haben Sie genügend\nInformationen?") # remove labels of axes and legend

ggsave(filename = "Grafiken/Informationen.png", plot = infoplot, dpi = 150, width = 12, height = 5.2)
ggsave(filename = "../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_9.png", plot = infoplot, dpi = 150, width = 12, height = 5.2)


# clean up 
rm(infos, reihenfolge, labels_infoplot, infoplot)


# q_23: XXX schon mal gemacht?? ####################################################
# q_23_1:q_23:8

labels_haben_sie_schon <- c("einen wissenschaftlichen Text publiziert\n(Zeitschriftenaufsatz, Buchbeitrag, Buch, Projektbericht)?",
                            "ein Buch herausgegeben?",
                            "eine Lehrveranstaltung\nan der Universität abgehalten?",
                            "an einem Projektantrag mitgearbeitet?",
                            "an einer (wissenschaftlichen)\nKonferenz/Tagung teilgenommen?",
                            "bei einer (wissenschaftlichen) Konferenz/Tagung\neinen Vortrag gehalten/ein Poster präsentiert",
                            "eine (wissenschaftliche)\nKonferenz/Tagung organisiert",
                            "an einer extra-curricularen\nWeiterbildungsveranstaltung teilgenommen")

# q_23_1
pdata <- df_haven %>%
  select(q_23_1, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_1, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_1 == "Ja")

p1 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", width = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = labels_haben_sie_schon[1]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")


rm(pdata)

# q_23_2
pdata <- df_haven %>%
  select(q_23_2, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_2, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_2 == "Ja")

p2 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6),
                     labels = percent_format()) +
  labs(title = labels_haben_sie_schon[2]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")


rm(pdata)

# q_23_3
pdata <- df_haven %>%
  select(q_23_3, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_3, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_3 == "Ja")

p3 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = labels_haben_sie_schon[3]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")


rm(pdata)

# q_23_4
pdata <- df_haven %>%
  select(q_23_4, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_4, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_4 == "Ja")

p4 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = labels_haben_sie_schon[4]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")


rm(pdata)

# q_23_5
pdata <- df_haven %>%
  select(q_23_5, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_5, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_5 == "Ja")

p5 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = labels_haben_sie_schon[5]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")


rm(pdata)

# q_23_6
pdata <- df_haven %>%
  select(q_23_6, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_6, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_6 == "Ja")

p6 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = labels_haben_sie_schon[6]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")


rm(pdata)

# q_23_7
pdata <- df_haven %>%
  select(q_23_7, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_7, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_7 == "Ja")

p7 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = labels_haben_sie_schon[7]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")


rm(pdata)

# q_23_8
pdata <- df_haven %>%
  select(q_23_8, studiendauer_2_bis3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_8, studiendauer_2_bis3, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_8 == "Ja")

p8 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = labels_haben_sie_schon[8]) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")
rm(pdata)

png("Grafiken/Haben_Sie_schon/Haben_Sie_schon.png", width = 2280, height = 2800, res = 200)
grid.arrange(p1, p3, p2, p4, p5, p6, p7, p8,
             nrow = 4,
             main = textGrob("Haben Sie schon...?", gp = gpar(cex = 1.7)))
dev.off()

png("../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_10.png", width = 2280, height = 2800, res = 200)
grid.arrange(p1, p3, p2, p4, p5, p6, p7, p8,
             nrow = 4,
             main = textGrob("Haben Sie schon...?", gp = gpar(cex = 1.7)))
dev.off()

rm(p1, p2, p3, p4, p5, p6, p7, p8)

# Schwierigkeit, BetreuerIn zu finden ----------------------------------------
# compute data to plot
pdata <- df_sav %>%
  with(., table(q_9, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Betreuer = q_9, Geschlecht = q_24)

schwierigkeitsplot <- ggplot(pdata, aes(Betreuer, p, fill = Geschlecht)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colours) +
  labs(y = "Prozentanteile innerhalb der Geschlechter", x = "Schwierigkeit, eine/n BetreuerIn zu finden") 
schwierigkeitsplot

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
  rename(., Betreuer = q_8, Geschlecht = q_24) %>%
  ungroup %>% 
  mutate(Geschlecht = factor(Geschlecht, labels = c("weiblich (n = 16)", "männlich (n = 20)")))

betreuerplot_2 <- ggplot(pdata, aes(Betreuer, p, fill = Geschlecht)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(breaks = c("Frau", "Mann"), labels = c("weiblich", "männlich")) +
  scale_fill_manual(values =  c(`weiblich (n = 16)` = "#4292C6", `männlich (n = 20)` = "#A1D99B")) +
  labs(y    = "Prozentanteile innerhalb Geschlecht des/der Studierenden",
       x    = "Geschlecht des/der BetreuerIn",
       fill = "Geschlecht des/der\nStudierenden") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14)) 
  

ggsave(filename = "Grafiken/Geschlecht_betreuer.png",
       plot = betreuerplot_2,
       dpi = 150, width = 8, height = 6.5)

ggsave(filename = "../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_7.png",
       plot = betreuerplot_2,
       dpi = 150, width = 8, height = 6.5)

rm(pdata, betreuerplot_1, betreuerplot_2)


## Zeitliche Aufwendungen pro Woche --------------------------------------

# Aufwand für Studium
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  filter(q_24 != "NA", q_26_1 != "NA") -> pdata

p1 <- ggplot(pdata, aes(q_24, q_26_1)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Studium") +
  labs(x = NULL) + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80)) +
  scale_x_discrete(labels = c("weiblich\n(n = 32)", "männlich\n(n = 43)"))


# Aufwand für Erwerbstätigkeit
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24, q_31) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  filter(q_24 != "NA", q_26_2 != "NA", q_31 == 1) -> pdata

p2 <- ggplot(pdata, aes(q_24, q_26_2)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Erwerbstätigkeit") +
  labs(x = NULL) + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80)) +
  scale_x_discrete(labels = c("weiblich\n(n = 28)", "männlich\n(n = 38)"))



# Aufwand für Haushaltsführung
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  filter(q_24 != "NA", q_26_3 != "NA") -> pdata

p3 <- ggplot(pdata, aes(q_24, q_26_3)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Haushaltsführung") +
  labs(x = NULL) + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80)) +
  scale_x_discrete(labels = c("weiblich\n(n = 32)", "männlich\n(n = 43)"))



# Aufwand für Hobbies und Sport
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  filter(q_24 != "NA", q_26_4 != "NA") -> pdata

p4 <- ggplot(pdata, aes(q_24, q_26_4)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Hobbies und Sport") +
  labs(x = NULL) + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80)) +
  scale_x_discrete(labels = c("weiblich\n(n = 32)", "männlich\n(n = 43)"))


# Aufwand für Betreungspflichten (Kinder/Angehörige)
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  filter(q_24 != "NA", q_26_5 != "NA") -> pdata 

p5 <- ggplot(pdata, aes(q_24, q_26_5)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Betreungspflichten (Kinder/Angehörige)") +
  labs(x = NULL) + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80)) +
  scale_x_discrete(labels = c("weiblich", "männlich"))



png("Grafiken/Zeitaufwand.png", width = 1500, height = 1400, res = 200)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

png("../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_3.png", width = 1500, height = 1400, res = 200)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

## Berufstätigkeit ---------------
# Farben für skala
colours_skala <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#BAE4B3", "trifft gar nicht zu" = "#EDF8E9")
colours_skala_blue_green <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#9ECAE1", "trifft gar nicht zu" = "#4292C6")
colours_skala_blue_green_sw <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#C6DBEF", "trifft gar nicht zu" = "#9ECAE1")


# variablennamen für den motivationsplot
labels_berufstätigkeit_1n <- c("Meine derzeitige Erwerbstätigkeit lässt sich sehr gut\nmit den Inhalten des Doktoratsstudiums/der Dissertation vereinen",
                               "Es ist schwierig, Doktoratsstudium\nund Erwerbstätigkeit zu vereinbaren", 
                               "Meine derzeitige Erwerbstätigkeit ist für den\nweiteren Verlauf meiner beruflichen Laufbahn förderlich",
                               "Ich kann meine Arbeitszeit im Hinblick auf die\nAnforderungen des Doktoratsstudiums frei einteilen",
                               "Ich würde gerne den Umfang meiner Erwerbstätigkeit reduzieren,\num mehr Zeit für das Doktoratsstudium zu haben")


# univariat
# get number of valid observations for further computation of percentages
cases <- df_haven_neu %>%
  select(q_35_1:q_35_5)
cases <- colSums(!is.na(cases))

# get counts of first two levels, in order to get order of variables for plot
reihenfolge <- df_haven_neu %>%
  select(q_35_1:q_35_5) %>% 
  lapply(as_factor) %>% as_data_frame %>% 
  summarise_each(., funs(sum(. == "trifft zu" | . == "trifft eher zu", na.rm = T))) %>% # summiere die ausprägungen für die ersten beiden levels
  gather(id, häufigkeit) %>%
  mutate(häufigkeit = häufigkeit / cases) %>% # divide counts by cases for correct percentages
  cbind(., `labels_berufstätigkeit_1n`) 

# select data to plot and gather it in long format, remove NAs
berufstätigkeit <- df_haven_neu %>%
  select(q_35_1:q_35_5) %>%
  lapply(., as_factor) %>% 
  as_data_frame %>% 
  gather(., id, variable) %>%
  na.omit

# join datasets
berufstätigkeit <- full_join(berufstätigkeit, reihenfolge, by = "id")

# reorder the levels for the plot
berufstätigkeit$variable <- factor(berufstätigkeit$variable,
                                   levels = c("trifft zu",
                                              "trifft eher zu",
                                              "trifft eher nicht zu",
                                              "trifft gar nicht zu"))

berufstätigkeit$labels_berufstätigkeit_1n <- factor(berufstätigkeit$labels_berufstätigkeit_1n,
                                                    levels = berufstätigkeit$labels_berufstätigkeit_1n[order(berufstätigkeit$häufigkeit)])

# plot data
berufsplot <- ggplot(berufstätigkeit, aes(`labels_berufstätigkeit_1n`, fill = variable))  +
  geom_bar(position = "fill", width = .7) +
  coord_flip() +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 11)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL) # remove labels of axes and legend


ggsave(filename = "Grafiken/Berufstätigkeit.png",
       plot = berufsplot,
       dpi = 150, height = 4, width = 10)

ggsave(filename = "../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_2.png",
       plot = berufsplot,
       dpi = 150, height = 4, width = 10)


## Zufriedenheit mit Betreuung ----------
colours_skala_blue_green <- c("sehr zufrieden" = "#238B45", "eher zufrieden" = "#74C476",
                              "eher nicht zufrieden" = "#9ECAE1", "gar nicht zufrieden" = "#4292C6")

labels_betreuung <- c("Häufigkeit der Betreuung",
                      "Fachliche Betreuung",
                      "Unterstützung bei der wissenschaftlichen Laufbahnplanung")


# q_12_1
df_haven_neu %>%
  select(q_12_1, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p1 <- ggplot(pdata, aes(q_24, fill = q_12_1))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_betreuung[1]) +
  coord_flip() +
  theme(axis.text = element_text(size = 12)) + 
  scale_x_discrete(labels = c("weiblich\n(n = 12)", "männlich\n(n = 17)"))

# q_12_2
df_haven_neu %>%
  select(q_12_2, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p2 <- ggplot(pdata, aes(q_24, fill = q_12_2))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_betreuung[2]) +
  coord_flip() +
  theme(axis.text = element_text(size = 12)) + 
  scale_x_discrete(labels = c("weiblich\n(n = 12)", "männlich\n(n = 17)"))

# q_12_3
df_haven_neu %>%
  select(q_12_3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p3 <- ggplot(pdata, aes(q_24, fill = q_12_3))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_betreuung[3]) +
  coord_flip() +
  theme(axis.text = element_text(size = 12)) + 
  scale_x_discrete(labels = c("weiblich\n(n = 13)", "männlich\n(n = 16)"))

png("Grafiken/Betreuungszufriedenheit.png", width = 1400, height = 1200, res = 200)
grid.arrange(p1, p2, p3, nrow = 3,
             main = textGrob("Wie zufrieden sind Sie mit der Betreuung Ihrer Dissertation?",
                             gp = gpar(fontsize = 16)))
dev.off()

png("../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_8.png", width = 1400, height = 1200, res = 200)
grid.arrange(p1, p2, p3, nrow = 3,
             main = textGrob("Wie zufrieden sind Sie mit der Betreuung Ihrer Dissertation?",
                             gp = gpar(fontsize = 16)))
dev.off()



## Bild der Wissenschaft ------------------
colours_skala_blue_green <- c("trifft zu" = "#238B45",
                              "trifft eher zu" = "#74C476",
                              "trifft eher nicht zu" = "#9ECAE1",
                              "trifft gar nicht zu" = "#4292C6")

labels_bild <- c("Frauen haben im universitären Umfeld gleich\nhohe Chancen auf eine erfolgreiche Laufbahn wie Männer",
                 "Es ist schwierig, eine wissenschaftliche\nLaufbahn zeitlich zu planen",
                 "Eine Vereinbarkeit von Familie und Beruf\nist im wissenschaftlichen Berufsfeld gut möglich")


# q_22_2
df_haven_neu %>%
  select(q_22_2, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p1 <- ggplot(pdata, aes(q_24, fill = q_22_2))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_bild[1]) +
  coord_flip() +
  theme(title  = element_text(size = 10)) +
  scale_x_discrete(labels = c("weiblich\n(n = 29)", "männlich\n(n = 34)"))

# q_22_3
df_haven_neu %>%
  select(q_22_3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p2 <- ggplot(pdata, aes(q_24, fill = q_22_3))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_bild[2]) +
  coord_flip() +
  theme(title  = element_text(size = 10)) +
  scale_x_discrete(labels = c("weiblich\n(n = 31)", "männlich\n(n = 38)"))

# q_22_6
df_haven_neu %>%
  select(q_22_6, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p3 <- ggplot(pdata, aes(q_24, fill = q_22_6))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_bild[3]) +
  coord_flip() +
  theme(title = element_text(size = 10)) +
  scale_x_discrete(labels = c("weiblich\n(n = 29)", "männlich\n(n = 38)"))

png("Grafiken/Bild_der_Wissenschaft.png", width = 1400, height = 1200, res = 200)
grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

png("../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_11.png", width = 1400, height = 1200, res = 200)
grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

## Bild der Wissenschaft - Indizes ------------
# select data to plot
df_haven %>%
  select(unterbrechung_index, mobilität_index, engagement_index, q_24)  %>% 
  mutate(unterbrechung_index = 5 - unterbrechung_index,
         mobilität_index = 5 - mobilität_index,
         engagement_index = 5 - engagement_index) %>% 
  mutate(q_24 = factor(q_24, labels = c("weiblich", "männlich")))  %>% 
  filter(q_24 != "NA") -> pdata # personen rausschmeißen, die als Geschlecht NA haben


# alternativ als jitterplot (violinplot ist für die Daten nicht ehrlich)
# unterbrechung_index
p1 <- ggplot(pdata, aes(q_24, unterbrechung_index)) +
  geom_boxplot(width = .6, alpha = .7) +
  geom_jitter(position = position_jitter(height = .1, width = .1),
              aes(colour = q_24),
              size = 4,
              alpha = .7) +
  stat_summary(fun.y = "mean", geom = "point", size = 8, shape = 4) +
  labs(title = "Unterbrechungen haben eine\nhemmende Auswirkungen auf\neine wissenschaftliche Laufbahn",
       x = NULL,
       y = NULL) +
  scale_colour_manual(values = colours) +
  theme_bw() +
  scale_y_continuous(limits = c(.9, 4.1), breaks = c(1, 4),
                     labels = c("Ablehnung", "Zustimmung")) +
  scale_x_discrete(labels = c("weiblich\n(n = 31)", "männlich\n(n = 36)")) +
  theme(axis.text = element_text(size = 12),
        axis.text = element_text(size = 13),
        title = element_text(size = 13)) +
  guides(colour = FALSE) # remove legend

# engagement_index
p2 <- ggplot(pdata, aes(q_24, engagement_index)) +
  geom_boxplot(width = .6, alpha = .7) +
  geom_jitter(position = position_jitter(height = .1, width = .1),
              aes(colour = q_24),
              size = 4,
              alpha = .7) + 
  stat_summary(fun.y = "mean", geom = "point", size = 8, shape = 4) +
  labs(title = "Wissenschaft erfordert\nein überdurchschnittliches\nEngagement",
       x = NULL,
       y = NULL) +
  scale_colour_manual(values = colours) +
  theme_bw() +
  scale_y_continuous(limits = c(.9, 4.1), breaks = c(1, 4),
                     labels = c("Ablehnung", "Zustimmung")) +
  scale_x_discrete(labels = c("weiblich\n(n = 32)", "männlich\n(n = 41)")) +
  theme(axis.text = element_text(size = 12),
        axis.text = element_text(size = 13),
        title = element_text(size = 13)) +
  guides(colour = FALSE) # remove legend


# mobilität_index
p3 <- ggplot(pdata, aes(q_24, mobilität_index)) +
  geom_boxplot(width = .6, alpha = .7) +
  geom_jitter(position = position_jitter(height = .1, width = .1),
              aes(colour = q_24),
              size = 4,
              alpha = .7) +
  stat_summary(fun.y = "mean", geom = "point", size = 8, shape = 4) +
  labs(title = "Für eine wissenschaftliche\nLaufbahn ist räumliche Mobilität\nerforderlich",
       x = NULL,
       y = NULL) +
  scale_colour_manual(values = colours) +
  theme_bw() +
  scale_y_continuous(limits = c(.9, 4.1), breaks = c(1, 4),
                     labels = c("Ablehnung", "Zustimmung")) +
  scale_x_discrete(labels = c("weiblich\n(n = 31)", "männlich\n(n = 39)")) +
  theme(axis.text = element_text(size = 12),
        axis.text = element_text(size = 13),
        title = element_text(size = 13)) +
  guides(colour = FALSE) # remove legend


png("Grafiken/Bild_der_Wissenschaft_Indizes.png", width = 1400, height = 1000, res = 100)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

png("../Fertige_Grafiken/Adaptiere Grafiken/Abbildung_12.png", width = 1400, height = 1000, res = 100)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

## Perspektiven wissenschaftliche Karriere -----------------
# angelehnt an plot "Motive"

labels_wiss_karriere <- c("Ich möchte das Doktoratsstudium abschließen",
                          "Ich möchte im universitären Bereich\nin der Forschung tätig sein",
                          "Ich möchte im außeruniversitären Bereich\nin der Forschung tätig sein",
                          "Ich strebe eine berufliche Laufbahn\naußerhalb der wissenschaftlichen Forschung an",
                          "Ich plane in Zukunft einen Auslandsaufenthalt,\num an einer anderen Universität zu studieren\noder zu arbeiten",
                          "Ich strebe eine Professur an einer Universität an")

colours_skala_blue_green <- c("ja, sicher" = "#238B45",
                              "eher ja" = "#74C476",
                              "eher nein" = "#9ECAE1",
                              "nein, sicher nicht" = "#4292C6")


# get number of valid observations for further computation of percentages
cases <- df_haven %>%
  select(., q_19_1:q_19_6)
cases <- colSums(!is.na(cases))

# get counts of first two levels, in order to get order of variables for plot
reihenfolge <- df_sav %>%
  select(q_19_1:q_19_6) %>% 
  summarise_each(., funs(sum(.== "ja, sicher" | . == "eher ja", na.rm = T))) %>% # summiere die ausprägungen für die ersten beiden levels
  gather(id, häufigkeit) %>%
  mutate(häufigkeit = häufigkeit / cases) %>% # divide counts by cases for correct percentages
  cbind(., labels_wiss_karriere) 

# select data to plot and gather it in long format, remove NAs
wiss_karriere <- df_sav %>%
  select(q_19_1:q_19_6) %>%
  gather(., id, variable) %>%
  na.omit

# join datasets
wiss_karriere <- full_join(wiss_karriere, reihenfolge, by = "id")

# # reorder the levels for the plot
wiss_karriere$variable <- factor(wiss_karriere$variable, levels = c("ja, sicher", "eher ja", "eher nein", "nein, sicher nicht"))
wiss_karriere$labels_wiss_karriere <- factor(wiss_karriere$labels_wiss_karriere, levels = wiss_karriere$labels_wiss_karriere[order(wiss_karriere$häufigkeit)])

# plot data
wiss_karriere_plot <- ggplot(wiss_karriere, aes(labels_wiss_karriere, fill = variable))  +
  geom_bar(position = "fill", width = .7) +
  coord_flip() +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size=11)) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL) # remove labels of axes and legend

ggsave(filename = "Grafiken/Perspektiven_wiss_Karriere.png",
       plot = wiss_karriere_plot,
       dpi = 150, height = 4.7, width = 12.5)



## Indizes wiss. Karrierewunsch ------------
# select data to plot
df_haven_neu %>%
  select(WiKarrierewunsch_Index, q_24)  %>% 
  as.matrix %>% # get rid of "labelled" class which doesn't work with dplyr right now
  data.frame %>%
  mutate(q_24 = factor(q_24, labels=c("weiblich", "männlich")))  %>% 
  filter(q_24 != "NA") -> pdata # personen rausschmeißen, die als Geschlecht NA haben


# karrierewunsch nach geschlecht
wiss_laufbahnorientierung_geschlecht <- ggplot(pdata, aes(q_24, WiKarrierewunsch_Index)) +
  theme_bw() +  
  geom_boxplot(width = .7, alpha = .95) + 
  geom_jitter(position = position_jitter(height = .1, width = .1),
              aes(colour = q_24),
              size = 4,
              alpha = .7)  +  
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(x = "Geschlecht",
       y = NULL,
       title = "Wissenschaftliche Karriereorientierung") +
  theme(legend.position = "none") + # remove superflous legend
  scale_y_continuous(limits = c(.89, 17.11), breaks = seq(1, 17, 2), # breaks an den ungeraden Zahlen
                     labels = c("niedrig", seq(3, 15, 2), "hoch")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        title = element_text(size = 14)) +
  scale_colour_manual(values = colours)

ggsave(filename = "Grafiken/Laufbahnorientierung_Geschlecht.png",
       plot = wiss_laufbahnorientierung_geschlecht,
       dpi = 150, width = 5, height = 7)


# nach studienrichtung
unlabelled_complete <- function(x) {
  attr(x, "label") <- NULL
  attr(x, "class") <- NULL
  attr(x, "labels") <- NULL
  attr(x, "levels") <- NULL
  x
}


df_haven_neu %>%
  select(q_1) %>% 
  mutate(fächer_gesamt = factor(q_1, labels = c("BWL",
                                                "SOZ",
                                                "VWL",
                                                "WiPäd"))) %>% 
  mutate(q_1 = recode(q_1, "1 = 1; 2 = 2; 3 = 3; 4 = 1")) %>%
  mutate(q_1 = factor(q_1, labels = c("BWL",
                                      "SOZ",
                                      "VWL"))) -> pdata_1

df_haven_neu %>%
  select(WiKarrierewunsch_Index) %>% 
  lapply(., unlabelled_complete) %>% # strip labels from vectors for dplyr
  data.frame -> pdata_2

pdata <- bind_cols(pdata_1, pdata_2) %>%
  filter(q_1 != "NA")

wiss_laufbahnorientierung_studienrichtung <- ggplot(pdata, aes(q_1, WiKarrierewunsch_Index)) +
  geom_boxplot(width = .6, alpha = .7) +
  geom_jitter(position = position_jitter(height = .1, width = .1),
              aes(colour = fächer_gesamt),
              size = 4,
              alpha = .7) +
  stat_summary(fun.y = "mean", geom = "point", size = 8, shape = 4) +
  labs(x = "Studienrichtung",
       y = NULL,
       title = "Wissenschaftliche Karriereorientierung") +
  scale_colour_manual(values = colours) +
  theme_bw() +
  scale_y_continuous(limits = c(.89, 17.11), breaks = seq(1, 17, 2), # breaks an den ungeraden Zahlen
                     labels = c("niedrig",
                                seq(3, 15, 2),
                                "hoch")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        title = element_text(size = 14)) +
  annotate("text", x = 1.3, y = 11.2, label = "WiPäd", colour = "#807DBA") +
  guides(colour = FALSE) # remove legend

ggsave(filename = "Grafiken/Laufbahnorientierung_Studienrichtung.png",
       plot = wiss_laufbahnorientierung_studienrichtung,
       dpi = 150, width = 7, height = 6)


# Faktorenanalyse Motivationsindex ------------
# wrapper for plot
fa_parallel <- function(x, main = NULL) {
  ev <- eigen(cor(x, use = "pairwise.complete.obs")) # get eigenvalues
  ap <- parallel(subject = nrow(x), var = ncol(x), rep = 100, cent = 0.05)
  nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
  plotnScree(nS, main = main)
}

# data to plot
motive <- df_sav %>%
  select(., q_6_1:q_6_16) %>%
  data.matrix %>%
  na.omit %>%
  as.data.frame

# plot
png("../Faktorenanalysen/Faktorenanalyse_Motivation.png", width = 1000, height = 800, res = 150)
fa_parallel(motive)
dev.off()


# Faktorenanalyse Zukunftsvorstellungen ---------------
# data to plot
zukunft <- df_sav %>%
  select(., q_19_1:q_19_6) %>%
  data.matrix %>%
  na.omit %>%
  as.data.frame


png("../Faktorenanalysen/Faktorenanalyse_Zukunftsfvorstellungen.png", width = 1000, height = 800, res = 150)
fa_parallel(zukunft)
dev.off()

# copy all graphs and the html documentation to delivery folder
filelist <- list.files("Grafiken", pattern = "png|html", full.names = TRUE)
folderlist <- list.files("Grafiken/Haben_Sie_schon", pattern = "png", full.names = TRUE)
file.copy(filelist, "../Fertige_Grafiken")
file.copy(folderlist, "../Fertige_Grafiken")




