# allgemein
library(ggplot2)
library(car)
library(plyr)
library(dplyr)
library(tidyr)
library(scales)
library(haven)
library(gridExtra) # arrange mutliple plots with grid.arrange()
library(RColorBrewer) # Farbton finden

# daten einlesen -----------
df_haven_neu <- read_sav("Data/DATENSATZ_FiW-main10-4-2015.sav")


unlabelled <- function(x) {
  attr(x, "label") <- NULL
  x
}

unlabelled_complete <- function(x) {
  attr(x, "label") <- NULL
  attr(x, "class") <- NULL
  attr(x, "labels") <- NULL
  attr(x, "levels") <- NULL
  x
}

# Hypothese: Geschlechterunterschiede sind eher Unterschiede zwischen den Studienrichtungen
# Wenn man die Studienrichtung kontrolliert, verschwinden diese Geschlechterunterschiede wieder


## Indizes wiss. Karrierewunsch ------------
# select data to plot
df_haven_neu %>%
  select(WiKarrierewunsch_Index, q_24, q_1)  %>% 
  mutate(q_1 = as_factor(q_1)) %>% 
  mutate(WiKarrierewunsch_Index = unlabelled_complete(WiKarrierewunsch_Index)) %>% 
  mutate(q_24 = factor(q_24, labels=c("weiblich", "männlich")))  %>% 
  filter(q_24 != "NA", q_1 != "NA") %>% tbl_df -> pdata # personen rausschmeißen, die als Geschlecht NA haben


# karrierewunsch nach geschlecht
wiss_laufbahnorientierung_geschlecht <- ggplot(pdata, aes(q_24, WiKarrierewunsch_Index)) +
  theme_bw() +  
  geom_boxplot(width = .7, alpha = .95) + 
  geom_jitter(position = position_jitter(height = .1, width = .1),
              aes(colour = q_24),
              size = 4,
              alpha = .7)  +
  facet_wrap(~ q_1, nrow = 1) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, shape = 4) +
  labs(x = "Geschlecht",
       y = NULL,
       title = "Wissenschaftliche Karriereorientierung ohne WiPäd") +
  theme(legend.position = "none") + # remove superflous legend
  scale_fill_manual(values = colours) +
  scale_y_continuous(limits = c(1, 17), breaks = c(1, 17),
                     labels = c("niedrig", "hoch")) +
  theme(axis.text.y = element_text(size = 12)) 
wiss_laufbahnorientierung_geschlecht


# -> mehr oder weniger bestätigt hier: kaum Geschlechterunterschiede, wenn nach Studium kontrolliert



# q_23: XXX schon mal gemacht?? ####################################################
# q_23_1:q_23:8

# q_23_1
pdata <- df_haven_neu %>%
  select(q_23_1, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_1, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_1 == "Ja")

p1 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", width = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_1)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_2
pdata <- df_haven_neu %>%
  select(q_23_2, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_2, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_2 == "Ja")

p2 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6),
                     labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_2)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_3
pdata <- df_haven_neu %>%
  select(q_23_3, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_3, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_3 == "Ja")

p3 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_3)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_4
pdata <- df_haven_neu %>%
  select(q_23_4, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_4, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_4 == "Ja")

p4 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_4)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_5
pdata <- df_haven_neu %>%
  select(q_23_5, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_5, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_5 == "Ja")

p5 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_5)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_6
pdata <- df_haven_neu %>%
  select(q_23_6, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_6, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_6 == "Ja")

p6 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_6)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_7
pdata <- df_haven_neu %>%
  select(q_23_7, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_7, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_7 == "Ja")

p7 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_7)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_8
pdata <- df_haven_neu %>%
  select(q_23_8, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_8, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe", q_23_8 == "Ja")

p8 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_8)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

grid.arrange(p1, p2, p3, p4, nrow = 2)
grid.arrange(p5, p6, p7, p8, nrow = 2)


# eine Version nur mit BWL und SOZ

# q_23_1
pdata <- df_haven_neu %>%
  select(q_23_1, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_1, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_1 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p1 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", width = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_1)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_2
pdata <- df_haven_neu %>%
  select(q_23_2, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_2, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_2 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p2 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6),
                     labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_2)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_3
pdata <- df_haven_neu %>%
  select(q_23_3, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_3, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_3 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p3 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_3)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_4
pdata <- df_haven_neu %>%
  select(q_23_4, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_4, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_4 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p4 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_4)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_5
pdata <- df_haven_neu %>%
  select(q_23_5, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_5, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_5 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p5 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_5)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_6
pdata <- df_haven_neu %>%
  select(q_23_6, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_6, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_6 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p6 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_6)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_7
pdata <- df_haven_neu %>%
  select(q_23_7, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_7, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_7 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p7 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_7)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

# q_23_8
pdata <- df_haven_neu %>%
  select(q_23_8, studiendauer_2_bis3, q_24, q_1) %>%
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>%
  with(., table(q_23_8, studiendauer_2_bis3, q_24, q_1)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(studiendauer_2_bis3, q_24, q_1) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Geschlecht = q_24) %>%
  filter(Geschlecht != "keine Angabe",
         q_23_8 == "Ja",
         q_1 %in% c("Betriebswirtschaft", "Soziologie"))

p8 <- ggplot(pdata, aes(studiendauer_2_bis3, p, fill=Geschlecht)) +
  geom_bar(stat="identity", position="dodge", widht = .8) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 6), labels = percent_format()) +
  labs(title = attributes(df_haven_neu$q_23_8)$label) +
  scale_fill_manual(values = colours) +
  labs(x = "Studiendauer in Semestern",
       y = "Prozentanteile für Antwort 'Ja'") +
  facet_wrap(~ q_1, nrow = 1)


rm(pdata)

grid.arrange(p1, p2, p3, p4, nrow = 2)
grid.arrange(p5, p6, p7, p8, nrow = 2)



# Schlussfolgerungen:
# WiPäd und BWL zusammenschmeißen ist keine so gute Idee: es scheinen doch zwei 
# verschiedene Gruppen zu sein.
# kleine Fallzahlen lassen schwierig Schlussfolgerungen zu, aber:
# klar scheint: Männer haben klaren Überhang bei allen Dingen die mit Konferenzen
# in Zusammenhang stehen.
# Es ist auch twl. bei den Fächern unterschiedlich: zb bei "wiss. publiziert"
# Generell: Frauen sind vorne bei Weiterbildungen
# Dass bei BWL Frauen bei wiss. publiziert und LV vorne sind, liegt daran, dass
# das nur eine Frau ist.
# Diese Art der Analyse ist also etwas fragwürdig.
# 
# 
# ### ### ### ### ### ### ### ### #### ### ### ### ### ### ### ### ### ###
# Fehlt: Wie sind die einzelnen Variablen der zentralen AV hinsichtlich
# der Geschlechterunterschiede, wenn man nach Studienrichtung splittet?
# ### ### ### ### ### ### ### ### #### ### ### ### ### ### ### ### ### ###
# 
# 