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

# link zu den Paletten
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/


# Farbschema erstellen -----
palette_green <- brewer.pal(7, "Greens")
palette_green

palette_blue <- brewer.pal(7, "Blues")
palette_blue

# #A1D99B für Männer (Grün) als Grundfarbe 
# #4292C6 für Frauen (Blau)

# Basisgrün: #74C476

# Farben für "scale_fill_manual"
colours <- c(männlich = "#A1D99B", weiblich = "#4292C6")



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

# get frequencies of first two levels, in order to get order of variables for plot
reihenfolge <- df_sav %>%
  select(q_6_1:q_6_16) %>%
  summarise_each(., funs(sum(.== "trifft zu" | . == "trifft eher zu", na.rm = T))) %>% # summiere die ausprägungen für die ersten beiden levels
  gather(id, häufigkeit) %>%
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
  theme_light() +
  theme(legend.position=c(.8, .18), axis.text.y = element_text(size = 12),
        legend.key.size = unit(1.3, "cm"),
        legend.text=element_text(size=11)) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = percent_format()) +
  labs(x = NULL, y = NULL, fill = NULL) # remove labels of axes and legend
motivplot

# fehlt: farbschema überlegen: vielleicht doch blau und grün? dann ist die mitte besser zu erkennen. ist halt für sw schlechter
#         fehler: duplicated factors nachgehen

# clean up 
rm(motive, reihenfolge, labels_motivplot, motivplot)


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
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
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
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
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
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
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
  theme_light() +  
  geom_violin(aes(fill = q_24), trim = T, alpha = .85) + 
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
varname <- c("Unzufriedenheit mit dem Studium", "Fehlende Aussicht auf institutionelle\n Einbindung an einer Universität", "Probleme bei der Finanzierung des Doktoratsstudiums", "Mangelnde Vereinbarkeit mit Berufstätigkeit", "Mangelnde Vereinbarkeit mit Betreuungspflichten", "Attraktive Arbeit gefunden", "Erwartungen an meine Leistungen nicht erfüllbar", "Interesse verloren", "Stillstand bei der Dissertation", "Schwierigkeiten eine/n BetreuerIn zu finden", "Nur nebenbei studiert", "Fehlende Unterstützung durch den/die BetreuerIn", "Keine befriedigenden Berufsaussichten\n mit dem Doktoratsabschluss", "Doktoratsstudium ist zu schwierig", "Doktoratsstudium als zeitliche Überbrückung gedacht", "Kind bekommen bzw. werde ein Kind bekommen")
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
  theme_light() +
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
  theme_light() +
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


