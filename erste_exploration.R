# pakete ----------
# faktorenanalyse
library(psych)
library(nFactors)
library(FactoMineR)

# allgemein
library(ggplot2)
library(foreign)
library(Kmisc)
library(Hmisc)
library(car)
library(plyr)
library(dplyr)
library(magrittr)


# working directory
setwd("/Users/thomask/Dropbox/Soziologie/Frauen\ in\ der\ Wissenschaft/quantitative\ Erhebung/Arbeitsbereich\ Thomas/Berechnungen")

# daten einlesen -----------
df <- read.spss("Data/Datensatz_15-12.sav", use.value.labels=T, to.data.frame=T)

# alternative Daten, direkt aus LimeSurvey
source("Data/survey_49753_R_syntax_file.R", encoding = "UTF-8")

# local data frame erstellen: wird besser dargestellt
df_sav <- tbl_df(df)
df_r <- tbl_df(data)

# nicht komplette Antworten aussortieren:
df_sav <- df_sav %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))
df_r <- df_r %>%
  filter(., id %nin% c(27, 30, 37, 43, 45, 46, 91))



# faktorenplots ----------

# Motive --
motive <- df_sav %>%
  select(., q_6_1:q_6_16) 

# NAs setzen
motive[motive=="weiß nicht"] <- NA

# fälle mit NAs rauswerfen
motive <- motive %>%
  data.matrix %>%
  na.omit 


# factormap
PCA(motive, graph=F) %>%
  plot.PCA(., choix="var")

# parallel test
ev <- eigen(cor(motive, use = "pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject = nrow(motive), var = ncol(motive), rep = 100, cent = 0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

# -> 5 Faktorenlösung angebracht

fit_pca <- principal(motive, nfactors = 5, rotate = "oblimin")
fit_fa <- fa(motive, nfactors = 5, fm = "pa", rotate = "oblimin")
print.psych(fit_pca, cut = 0.15, sort = TRUE)
print.psych(fit_fa, cut = 0.15, sort = TRUE)




# FEHLT ÜBERPRÜFUNG: GIBT ES WEIß NICHT?!!!!!!! ######
# Zukunft
zukunft <- df_r %>%
  select(., q_19_1:q_19_6) %>%
  data.matrix %>%
  na.omit # fälle mit NAs rauswerfen

# factormap
PCA(zukunft, graph=F) %>%
  plot.PCA(., choix="var")

# parallel test
ev <- eigen(cor(zukunft, use = "pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject = nrow(zukunft), var = ncol(zukunft), rep = 100, quantile = 0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

# -> 2 Faktorenlösung
fit_pca <- principal(zukunft, nfactors = 2, rotate = "oblimin")
fit_fa <- fa(zukunft, nfactors = 2, fm = "pa", rotate = "oblimin")
print.psych(fit_pca, cut = 0.15, sort = TRUE)
print.psych(fit_fa, cut = 0.15, sort = TRUE)




# balkendiagramme für abbruch und unterbrechung --------------
# unterbrechung
unterbrecher <- df_r %>%
  filter(q_14 == "Ja") %>%
  select(., q_15_1:q_15_17)

# berechne die Prozente
d <- round(apply(unterbrecher, 2, function(col)sum(col=="Ja")/length(col))*100)



######################
# plotten ############
# setze "Names-Attribut" für barplot
attributes(d)$names <- c("Unzufriedenheit mit dem Studium", "Fehlende Aussicht auf institutionelle Einbindung an einer Universität", "Probleme bei der Finanzierung des Doktoratsstudiums", "Mangelnde Vereinbarkeit mit Berufstätigkeit", "Mangelnde Vereinbarkeit mit Betreuungspflichten", "Attraktive Arbeit gefunden", "Erwartungen an meine Leistungen nicht erfüllbar", "Interesse verloren", "Stillstand bei der Dissertation", "Schwierigkeiten eine/n BetreuerIn zu finden", "Nur nebenbei studiert", "Fehlende Unterstützung durch den/die BetreuerIn", "Keine befriedigenden Berufsaussichten mit dem Doktoratsabschluss", "Doktoratsstudium ist zu schwierig", "Doktoratsstudium als zeitliche Überbrückung gedacht", "Studienförderung läuft aus", "Kind bekommen bzw. werde ein Kind bekommen")

# margins setzen, damit beschriftung platz hat
par(mar=c(5, 18, 4, 2) + 0.1)
d[order(d, decreasing=F)] %>%
  barplot(., horiz=T, las=2, cex.names=.65, col="aquamarine3")
title(main="Gründe für Unterbrechungsgedanken", xlab="Angaben in Prozent")

# standard wiederherstellen
par(mar=c(5, 4, 4, 2) + 0.1)
#############################


#################
# versuch mit ggplot2
bla <- data.frame(c(d[,1]), rep(1:17))
ggplot(bla, aes(x=bla[,2], y=bla[,1])) + geom_bar(stat="identity") + coord_flip()
###############

# abbruch
# muss eher qualitativ gemacht werden, sind ja nicht so viele, oder?
describe(df_r$q_16)
# hm, sind 18. sollte man sich durchaus anschauen

# Anzahl der Semester
describe(df_r$q_17)

# Gründe
df_r$q_18
