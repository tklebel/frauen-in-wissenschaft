# Grafiken für den Endbericht

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

# link zu den Paletten
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/


# Farbschema erstellen -----
palette_green <- brewer.pal(7, "Greens")
palette_green

palette_blue <- brewer.pal(7, "Blues")
palette_blue

palette_red <- brewer.pal(7, "Reds")
palette_red


# #A1D99B für Männer (Grün) als Grundfarbe 
# #4292C6 für Frauen (Blau)
# #EF3B2C für gesamt (Rot)

# Basisgrün: #74C476

# Farben für "scale_fill_manual"
colours <- c(Mann = "#A1D99B", Frau = "#4292C6", gesamt = "#EF3B2C", weiblich = "#4292C6", männlich = "#A1D99B")


# helper function: strip label from vector
unlabelled <- function(x) {
  attr(x, "label") <- NULL
  x
}



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
motivplot

# fehlt: farbschema überlegen: vielleicht doch blau und grün? dann ist die mitte besser zu erkennen. ist halt für sw schlechter
#         fehler: duplicated factors nachgehen

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
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
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
  stat_summary(fun.y = "mean", geom = "point", size = 3.5, shape = 4) +
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
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
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
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Prestige als Motivation") +
  labs(x = "Geschlecht") + 
  labs(y = "Motivation") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_y_continuous(limits = c(1, 12.5), breaks = c(1, 6.5, 12), labels = c("niedrig", "mittel", "hoch"))  # extend y scale to range of data

# zusammenführen
grid.arrange(p1, p2, p3, p4)

# clean up
rm(pdata, p1, p2, p3, p4)

# ToDo
# man könnte untersuchen, in welchen Studienfächern Prestige stärker als Motiv fungiert (Hypothese: In BWL ist das stärker als in SOZ)


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
  geom_bar(stat = "identity", fill = "#74C476", width = .7) +
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
  geom_bar(stat = "identity", fill = "#74C476", width = .7) +
  coord_flip() +
  labs(y = "Häufigkeit der Nennung", x = NULL, title = "Frauen") +
  scale_y_continuous(breaks = pretty_breaks(8)) +
  theme(axis.text.y = element_text(size = 12)) 

# join plots
grid.arrange(abbruchplot_frauen, abbruchplot_männer, nrow = 1)
# Formatierung ist nicht optimal: bei zwei Spalten sind die plots nicht ausgerichtet, da nicht gleich breit
# bei zeilen sind die männer etwas komprimiert, da sie 6, anstatt 5 variablen haben

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

# kontrollieren
describe(Grundgesamtheit$Geschlecht)
describe(Grundgesamtheit$Studienrichtung)
describe(Grundgesamtheit$herkunft)

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

# absolute Zahlen ##
Stichprobenplot_abs <- ggplot(pdata, aes(x = herkunft, fill = Geschlecht)) +
  geom_bar() +
  facet_wrap(~ Studienrichtung) +
  theme_bw() +
  scale_fill_manual(values = colours)
Stichprobenplot_abs

# oder mit percent
# summarise data
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
Stichprobenplot


# Beschriftung mit den Stichprobengrößen
# Stichprobengrößen kalkulieren
pdata # anschauen -> manuell kalkulieren (dämlich!!!)

# n's zu plot hinzufügen
p <- arrangeGrob(Stichprobenplot, sub = textGrob(c("n = 258", "n = 40", "n = 77", "n = 32", "n = 33", "n = 6"),
                                                 x = c(0.013, .152, .29, .40, .537, .675),
                                                 hjust = -3.2, vjust = -2.8,
                                                 gp = gpar(fontsize = 10, col = "grey40")))
p

# export ratio: 800 x 523

rm(pdata)

# Rücklaufquote
# line plot
# prozente bwl
# frauen
9/118*100
# männer
31/140*100
# gesamt
((9/118*100) + (31/140*100)) / 2

# soz
# frauen
23/53*100
# männer
9/24*100
# gesamt
((23/53*100) + (9/24*100)) / 2

# vwl
# frauen
1/10*100
# männer
5/23*100
# gesamt
((1/10*100) + (5/23*100)) / 2

d1 <- data.frame(Geschlecht = c("weiblich", "männlich", "gesamt", "weiblich", "männlich", "gesamt", "weiblich", "männlich", "gesamt"),
                 Studienrichtung = c("BWL", "BWL", "BWL", "SOZ", "SOZ", "SOZ", "VWL", "VWL", "VWL"),
                 perc = c(9/118, 31/140, ((9/118) + (31/140)) / 2, 23/53, 9/24, ((23/53) + (9/24)) / 2, 1/10, 5/23, ((1/10) + (5/23)) / 2)
)

Rücklauf <- ggplot(d1, aes(x = Studienrichtung, y = perc, group = Geschlecht, colour = Geschlecht)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  theme_bw() +
  scale_fill_manual(values = colours) +
  scale_y_continuous(limits = c(0, .5), labels = percent_format()) +
  labs(y = "Rücklaufquote")
Rücklauf

rm(d1)

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

# fehlt: fehler: duplicated factors nachgehen

# clean up 
rm(infos, reihenfolge, labels_infoplot, infoplot)


# q_23: XXX schon mal gemacht?? ####################################################
# q_23_1:q_23:8

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
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_1)$label) +
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
  scale_y_continuous(breaks = pretty_breaks(n = 6),
                     labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_2)$label) +
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
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_3)$label) +
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
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_4)$label) +
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
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_5)$label) +
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
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_6)$label) +
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
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_7)$label) +
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
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven$q_23_8)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'")
  

rm(pdata)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 4)

p1
p2
p3
p4
p5
p6
p7
p8



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
  theme_bw() + scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colours) +
  labs(y="Prozentanteile innerhalb der Geschlechter", x = "Schwierigkeit, eine/n BetreuerIn zu finden") 
schwierigkeitsplot

# clean up
rm(pdata)


## Geschlecht der Studierenden abhängig von Geschlecht BetreuerIn ------------------------
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

betreuerplot_1
betreuerplot_2

rm(pdata, betreuerplot)


## Zeitliche Aufwendungen pro Woche --------------------------------------

# Aufwand für Studium
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  lapply(., unlabelled) %>% # strip labels from vectors for dplyr
  data.frame %>% 
  filter(q_24 != "NA", q_26_1 != "NA") -> pdata

p1 <- ggplot(pdata, aes(q_24, q_26_1)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Studium") +
  labs(x = "Geschlecht") + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80))


# Aufwand für Erwerbstätigkeit
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  lapply(., unlabelled) %>% # strip labels from vectors for dplyr
  data.frame %>% 
  filter(q_24 != "NA", q_26_2 != "NA") -> pdata

p2 <- ggplot(pdata, aes(q_24, q_26_2)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Erwerbstätigkeit") +
  labs(x = "Geschlecht") + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80))



# Aufwand für Haushaltsführung
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  lapply(., unlabelled) %>% # strip labels from vectors for dplyr
  data.frame %>% 
  filter(q_24 != "NA", q_26_3 != "NA") -> pdata

p3 <- ggplot(pdata, aes(q_24, q_26_3)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Haushaltsführung") +
  labs(x = "Geschlecht") + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80))



# Aufwand für Hobbies und Sport
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  lapply(., unlabelled) %>% # strip labels from vectors for dplyr
  data.frame %>% 
  filter(q_24 != "NA", q_26_4 != "NA") -> pdata

p4 <- ggplot(pdata, aes(q_24, q_26_4)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Hobbies und Sport") +
  labs(x = "Geschlecht") + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80))


# Aufwand für Betreungspflichten (Kinder/Angehörige)
df_haven_neu %>%
  select(q_26_1:q_26_5, q_24) %>%
  mutate(q_24 = as_factor(q_24)) %>%
  lapply(., unlabelled) %>% # strip labels from vectors for dplyr
  data.frame %>% 
  filter(q_24 != "NA", q_26_5 != "NA") -> pdata 

p5 <- ggplot(pdata, aes(q_24, q_26_5)) +
  geom_violin(aes(fill = q_24), trim = T, alpha = .85, adjust = .6, width = 1) + 
  geom_boxplot(width = .12, alpha = .95) +
  theme_bw() +    
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(title = "Betreungspflichten (Kinder/Angehörige)") +
  labs(x = "Geschlecht") + 
  labs(y = "Aufwand [in Stunden]") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  ylim(c(0, 80))


grid.arrange(p1, p2, p3, p4, p5, nrow = 2)


## Berufstätigkeit ---------------

# Farben für skala
colours_skala <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#BAE4B3", "trifft gar nicht zu" = "#EDF8E9")
colours_skala_blue_green <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#9ECAE1", "trifft gar nicht zu" = "#4292C6")
colours_skala_blue_green_sw <- c("trifft zu" = "#238B45", "trifft eher zu" = "#74C476", "trifft eher nicht zu" = "#C6DBEF", "trifft gar nicht zu" = "#9ECAE1")


# variablennamen für den motivationsplot
labels_berufstätigkeit <- c("Meine derzeitige Erwerbstätigkeit\nlässt sich sehr gut mit den Inhalten\ndes Doktoratsstudiums/der Dissertation vereinen",
                            "Es ist schwierig, Doktoratsstudium\nund Erwerbstätigkeit zu vereinbaren", 
                            "Meine derzeitige Erwerbstätigkeit\nist für den weiteren Verlauf\nmeiner beruflichen Laufbahn förderlich",
                            "Ich kann meine Arbeitszeit\nim Hinblick auf die Anforderungen\ndes Doktoratsstudiums frei einteilen",
                            "Ich würde gerne den Umfang meiner\nErwerbstätigkeit reduzieren, um\nmehr Zeit für das Doktoratsstudium zu haben")

labels_berufstätigkeit_1n <- c("Meine derzeitige Erwerbstätigkeit lässt sich sehr gut\nmit den Inhalten des Doktoratsstudiums/der Dissertation vereinen",
                               "Es ist schwierig, Doktoratsstudium und Erwerbstätigkeit zu vereinbaren", 
                               "Meine derzeitige Erwerbstätigkeit ist für den\nweiteren Verlauf meiner beruflichen Laufbahn förderlich",
                               "Ich kann meine Arbeitszeit im Hinblick auf die\nAnforderungen des Doktoratsstudiums frei einteilen",
                               "Ich würde gerne den Umfang meiner Erwerbstätigkeit reduzieren,\nummehr Zeit für das Doktoratsstudium zu haben")


# q_35_1
df_haven_neu %>%
  select(q_35_1, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p1 <- ggplot(pdata, aes(q_24, fill = q_35_1))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_berufstätigkeit[1]) 
# q_35_2
df_haven_neu %>%
  select(q_35_2, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p2 <- ggplot(pdata, aes(q_24, fill = q_35_2))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_berufstätigkeit[2]) 
# q_35_3
df_haven_neu %>%
  select(q_35_3, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p3 <- ggplot(pdata, aes(q_24, fill = q_35_3))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_berufstätigkeit[3]) 
# q_35_4
df_haven_neu %>%
  select(q_35_4, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p4 <- ggplot(pdata, aes(q_24, fill = q_35_4))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_berufstätigkeit[4]) 
# q_35_5
df_haven_neu %>%
  select(q_35_5, q_24) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit -> pdata


p5 <- ggplot(pdata, aes(q_24, fill = q_35_5))  +
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = colours_skala_blue_green) +
  theme_bw() +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL, # remove labels of axes and legend
       title = labels_berufstätigkeit[5]) 

grid.arrange(p1, p2, p3, p4, p5, nrow = 3)



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
  coord_flip()

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
  coord_flip()

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
       coord_flip() 

grid.arrange(p1, p2, p3, nrow = 3, main = "Wie zufrieden sind Sie mit der Betreuung Ihrer Dissertation?")


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
  coord_flip() 

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
  coord_flip() 

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
  coord_flip() 

grid.arrange(p1, p2, p3, nrow = 3)




