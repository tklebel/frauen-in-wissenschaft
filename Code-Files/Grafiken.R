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
library(tidyr)

library(RColorBrewer)


# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)


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


# Better computation of graph -----------------------
# calculate the percentages for all variables
# dillentatic method, needs better computation

# N für die einzelnen Items
colSums(!is.na(motive))

# calculate percentages
calc_percentages <- function(x) {
  prop.table(table(x)) %>%
    as.numeric*100
}

motive %>%
  select(q_6_1) %>%
  calc_percentages -> prozente

# do it manually for every variable :/
motive %>%
  select(q_6_16) %>%
  calc_percentages %>%
  append(prozente, .) -> prozente


prozente <- c(61.250000, 35.000000,  3.750000,  0.000000, 41.772152, 25.316456, 22.784810, 10.126582, 47.500000, 31.250000, 12.500000,  8.750000, 13.580247,  6.172840, 11.111111, 69.135802, 22.784810, 31.645570, 31.645570, 13.924051, 35.443038, 32.911392, 20.253165, 11.392405,  7.594937, 27.848101, 26.582278, 37.974684, 20.000000, 15.000000, 22.500000, 42.500000,  7.500000, 15.000000, 12.500000, 65.000000, 8.641975,  8.641975,  9.876543, 72.839506,  2.500000,  8.750000, 10.000000, 78.750000,  8.750000, 2.500000,  1.250000, 87.500000,  1.234568,  6.172840,  2.469136, 90.123457, 53.750000, 35.000000, 8.750000,  2.500000, 15.384615, 29.487179, 37.179487, 17.948718, 23.750000,  3.750000,  6.250000, 66.250000)
prozente <- round(prozente, digits=2)
t <- factor(rep(c("trifft zu", "trifft eher zu", "trifft eher nicht zu", "trifft gar nicht zu"), 16), levels=c("trifft zu", "trifft eher zu", "trifft eher nicht zu", "trifft gar nicht zu"))
y <- rep(1:16, each= 4)
labels <- rep(c("Weil mich das Fach interessiert", "Um in Wissenschaft und Forschung\n arbeiten zu können", "Um mich in meinem Beruf weiterzubilden", "Weil ein/e Lehrende/r mir angeboten hat,\n meine Dissertation zu betreuen", "Um am Arbeitsmarkt\n bessere Chancen zu haben", "Um mein Dissertationsthema\n erforschen zu können", "Weil ein Doktor-Titel oft notwendig ist,\n wenn man in der Gesellschaft\n etwas erreichen will", "8", "9", "10", "11", "12", "13", "14", "15", "16"), each=4)
pdata <- data.frame(prozente, y, t)
motivplot <- ggplot(test, aes(y, prozente, fill=t)) + geom_bar(stat="identity", position="fill", width=.7) + theme_light() + coord_flip() + scale_x_continuous(breaks=rep(1:16), labels=labels) + theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=13)) + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Greens")
motivplot

# reverse colours
colours <-  rev(brewer.pal(name="Greens", n=4))
motivplot <- ggplot(test, aes(y, prozente, fill=t)) + geom_bar(stat="identity", position="fill", width=.7) + theme_light() + coord_flip() + scale_x_continuous(breaks=rep(1:16), labels=labels) + theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=13)) + scale_y_continuous(labels = percent_format()) + scale_fill_manual(values=colours)
motivplot

# arrage df according to percentages
pdata %>%
  spread(y, prozente) 

# take just the first two columns (trifft zu und trifft eher zu)
pdata[1:2,] -> pdata

# strip first column
pdata %>% select(2:17) -> pdata

colSums(pdata)

# combine the steps
pdata %>%
  spread(y, prozente) %>%
  select(2:17) %>%
  .[1:2,] %>%
  colSums -> reihenfolge

# nur die ersten beiden zeilen
pdata %>%
  spread(y, prozente) %>%
  select(2:17) %>%
  .[1:2,] 
##############################
# transpose to columns, then include index, put in bigger df, then arrange
#######################
reihenfolge <- rep(reihenfolge, each = 4)

pdatan

pdatan <- data.frame(pdata, reihenfolge)

pdatan %>%
  arrange(desc(reihenfolge))




# Schwierigkeit, BetreuerIn zu finden ----------
# compute data to plot
pdata <- df_sav %>%
  with(., table(q_9, q_24)) %>% # create table with variables
  as.data.frame %>% # coerce to data.frame -> computes frequencies (Freq)
  group_by(q_24) %>% # group by categorial variable
  mutate(p = Freq/sum(Freq)) %>% # compute grouped percentage
  rename(., Betreuer = q_9, Geschlecht = q_24)

schwierigkeitsplot <- ggplot(pdata, aes(Betreuer, p, fill=Geschlecht))  + geom_bar(stat="identity", position="dodge") + theme_bw() + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired") + labs(y="Prozentanteile innerhalb der Geschlechter", x = "Schwierigkeit, eine/n BetreuerIn zu finden") 
schwierigkeitsplot
