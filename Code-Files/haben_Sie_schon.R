# Kreuztabellen zu den Fragen q_23_1:q_23_8 

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
library(gmodels)

# daten einlesen -----------
df <- read.spss("Data/DATENSATZ_FiW.sav", use.value.labels=T, to.data.frame=T)
df_sav <- tbl_df(df)

df_haven <- read_sav("Data/DATENSATZ_FiW-main12_2.sav")


daten <- df_haven %>%
  select(q_23_1:q_23_8, studiendauer_2_bis3, q_24) %>%
  mutate(q_24 = labelled(q_24, c(weiblich = 1, männlich = 2))) %>% # bug in as_factor umgehen: geschlecht hat 3 ausprägungen, es kommen aber nur 2 vor
  lapply(., as_factor) %>%
  data.frame %>%
  na.omit %>% # Fälle mit fehlenden Werte raus: es gibt eh nur fehlende Werte in der Studiendauer, die braucht man sowieso immer
  tbl_df

# stichprobenzahl
tally(daten)

# q_23_1
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_1, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))
 
daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_1, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))

  
# 2
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_2, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))

daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_2, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))

# 3
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_3, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))

daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_3, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))

# 4
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_4, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))

daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_4, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))

# 5
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_5, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))

daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_5, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))

# 6
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_6, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))

daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_6, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))

# 7
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_7, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))

daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_7, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))

# 8
daten %>%
  filter(q_24 == "weiblich") %>%
  with(., CrossTable(x = q_23_8, y = studiendauer_2_bis3, prop.r = F, sresid = T, prop.t = F, prop.chisq = F, format = "SPSS"))

daten %>%
  filter(q_24 == "männlich") %>%
  with(., CrossTable(x = q_23_8, y = studiendauer_2_bis3, prop.r = F, prop.t = F, prop.chisq = F, sresid = T, format = "SPSS"))
  
