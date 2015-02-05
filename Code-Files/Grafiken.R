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

# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)


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
motivplot <- ggplot(test, aes(y, v, fill=t)) + geom_bar(stat="identity", position="fill", width=.7) + theme_light() + coord_flip() + scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7), labels=c("Weil mich das Fach interessiert", "Um in Wissenschaft und Forschung\n arbeiten zu können", "Um mich in meinem Beruf weiterzubilden", "Weil ein/e Lehrende/r mir angeboten hat,\n meine Dissertation zu betreuen", "Um am Arbeitsmarkt\n bessere Chancen zu haben", "Um mein Dissertationsthema\n erforschen zu können", "Weil ein Doktor-Titel oft notwendig ist,\n wenn man in der Gesellschaft\n etwas erreichen will")) + theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=13)) + scale_y_continuous(labels = percent_format()) + scale_fill_brewer(palette="Paired")
motivplot




