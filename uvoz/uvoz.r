# 2. faza: Uvoz podatkov
library(readr)
library(dplyr)
library(XML)
library(tidyverse)
library(tidyr)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")
##Slovarček držav##
poimenovanje_drzav = tibble(
  drzava_anglesko = c("European Union - 27 countries (from 2020)","Austria", "Belgium","Bulgaria", "Czech Republic", "Czechia", "Denmark", "Finland", "France", "Germany","Germany (until 1990 former territory of the FRG)",
                      "Greece", "Hungary", "Ireland",
                      "Italy", "Luxembourg", "Netherlands", "Poland", "Portugal","Romania", "Slovak Republic","Slovakia", "Spain", "Sweden", "Slovenia",
                      "Estonia", "Latvia", "Lithuania","Malta","Cyprus","Croatia"),
  drzave = c("EU","Avstrija", "Belgija","Bolgarija", "Češka","Češka", "Danska", "Finska", "Francija", "Nemčija","Nemčija", "Grčija", "Madžarska", "Irska",
             "Italija", "Luksemburg", "Nizozemska", "Poljska", "Portugalska","Romunija", "Slovaška","Slovaška", "Španija", "Švedska", "Slovenija",
             "Estonija", "Latvija", "Litva","Malta","Ciper","Hrvaška")
);

############Prva tabela: Indeks števila potovanj na osebo###########

#število potovanj# vrži kodo od spola
spol <- readHTMLTable("podatki/potovanja_spol.html")
st_potovanj <- spol[[1]]
st_potovanj <- pivot_longer(st_potovanj,
                            cols = colnames(st_potovanj)[-1],
                            names_to = "leto", 
                            values_to = "st.potovanj" )


colnames(st_potovanj)[1] <- "Država"
match(st_potovanj$Država, poimenovanje_drzav$drzava_anglesko)
st_potovanj$Država = unlist(poimenovanje_drzav[match(st_potovanj$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub("\\(b\\)", "", x)}))
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub(":", NA, x)}))
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub("\\(e\\)", "", x)}))
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub(",", "", x)}))


st_potovanj$st.potovanj <- as.numeric(st_potovanj$st.potovanj)
#število prebivalcev# še za EU
st_prebivalcev <- read.csv("podatki/st_prebivalcev_eu.csv")
st_prebivalcev <- st_prebivalcev %>% select(Država = Country.Name, "2012" = "X2012..YR2012.", "2013" = "X2013..YR2013.",
"2014" = "X2014..YR2014.", "2015" = "X2015..YR2015.", "2016" = "X2016..YR2016.", "2017" = "X2017..YR2017.",
"2018" = "X2018..YR2018.", "2019" = "X2019..YR2019.")

match(st_prebivalcev$Država, poimenovanje_drzav$drzava_anglesko)
st_prebivalcev$Država = unlist(poimenovanje_drzav[match(st_prebivalcev$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])
st_prebivalcev <- st_prebivalcev[-c(28:32), ]

st_prebivalcev <- pivot_longer(st_prebivalcev,
                            cols = colnames(st_prebivalcev)[-1],
                            names_to = "leto", 
                            values_to = "število.prebivalcev" 
)

oboje_skupaj <- st_prebivalcev %>%
  left_join(st_potovanj, by = c("leto", "Država"))

sumLeto <- aggregate(st_prebivalcev$število.prebivalcev, by= list(leto=st_prebivalcev$leto), FUN = sum)
#Končna tabela#
St_potovanj_na_osebo <- oboje_skupaj %>% 
  mutate(indeks = oboje_skupaj$st.potovanj/oboje_skupaj$število.prebivalcev)

average_indeks <- aggregate(St_potovanj_na_osebo$indeks, by= list(Država=St_potovanj_na_osebo$Država), FUN = mean)

average_indeks <- average_indeks%>%add_row(Država = "EU", x = mean(average_indeks[!(average_indeks$Država == "Švedska"),]$x))
##########Drugi del: gospodarske značilnosti, BDP#########

#########Minimalna mesečna plača v evrih##############EU kaj narediti mi je
mesecna_placa <- read.csv("podatki/earn_mw_cur_1_Data.csv")
mesecna_placa <- mesecna_placa %>% select(Država = GEO, leto = TIME, vsota = Value)

match(mesecna_placa$Država, poimenovanje_drzav$drzava_anglesko)
mesecna_placa$Država = unlist(poimenovanje_drzav[match(mesecna_placa$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])

mesecna_placa <- data.frame(lapply(mesecna_placa, function(x) {gsub("S1","", x)}))
mesecna_placa <- data.frame(lapply(mesecna_placa, function(x) {gsub(":",NA, x)}))
mesecna_placa <- data.frame(lapply(mesecna_placa, function(x) {gsub(",","", x)}))
mesecna_placa$vsota <- as.numeric(mesecna_placa$vsota)



###########Brezposelnost############ indeks

brezposelnost <- readHTMLTable("podatki/brezposelnostEU_v_tisoč.html")
brezposelnost <- brezposelnost[["NULL"]]

brezposelnost <- brezposelnost %>% select(Država = TIMEGEO, "2012" = "2012Q1", "2013" = "2013Q1",
                                          "2014" = "2014Q1", "2015" = "2015Q1",
                                          "2016" = "2016Q1", "2017" = "2017Q1", "2018" = "2018Q1", "2019" = "2019Q1")
match(st_potovanj$Država, poimenovanje_drzav$drzava_anglesko)

brezposelnost$Država = unlist(poimenovanje_drzav[match(brezposelnost$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])

brezposelnost <- data.frame(lapply(brezposelnost, function(x) {gsub("\\(b\\)", "", x)}))


brezposelnost <- pivot_longer(brezposelnost,
                                cols = colnames(brezposelnost)[-1],
                                names_to = "leto", 
                                values_to = "brezposelnost.v.1000" 
)

brezposelnost <- data.frame(lapply(brezposelnost, function(x) {gsub("X","", x)}))
brezposelnost <- data.frame(lapply(brezposelnost, function(x) {gsub(",","", x)}))

brezposelnost$brezposelnost.v.1000 <- as.numeric(brezposelnost$brezposelnost.v.1000)

#št.brezposelnih na št. prebivalcev (večji index,manj potovanj??)
združenje_za_indeks <- brezposelnost %>%
  left_join(st_prebivalcev, by = c("leto", "Država"))
indeks.bp <- združenje_za_indeks %>% mutate(indeks.bp = združenje_za_indeks$brezposelnost.v.1000 * 1000 /združenje_za_indeks$število.prebivalcev)%>%
  select(Država,leto,indeks.bp)

##########BDP############ kljukca

bdp <- readHTMLTable("podatki/bdpEU_mio.html")
bdp <- bdp[["NULL"]]

bdp <- bdp %>% select(Država = TIMEGEO, "2012" = "2012Q1", "2013" = "2013Q1",
                                      "2014" = "2014Q1", "2015" = "2015Q1",
                      "2016" = "2016Q1", "2017" = "2017Q1", "2018" = "2018Q1", "2019" = "2019Q1")

match(bdp$Država, bdp$drzava_anglesko)
bdp$Država = unlist(poimenovanje_drzav[match(bdp$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])


bdp <- data.frame(lapply(bdp, function(x) {gsub("\\(p\\)", "", x)}))
bdp <- data.frame(lapply(bdp, function(x) {gsub("\\(b\\)", "", x)}))
bdp <- data.frame(lapply(bdp, function(x) {gsub("\\,","", x)}))
bdp <- data.frame(lapply(bdp, function(x) {gsub("\\.","", x)}))


bdp <- pivot_longer(bdp,
                              cols = colnames(bdp)[-1],
                              names_to = "leto", 
                              values_to = "BDP" 
)
bdp <- data.frame(lapply(bdp, function(x) {gsub("X","", x)}))
bdp$BDP <- as.numeric(bdp$BDP)

bdp <- bdp %>% left_join(st_prebivalcev, by = c("leto","Država"))
bdppc <- bdp$BDP * 1000000/bdp$število.prebivalcev
bdp <-bdp %>% mutate(bdppc) 

###GOSPODARSKE ZNAČILNOSTI (BDP, BREZPOSELNOST, MINIMALNA PLAČA)
Gospodarske_značilnosti <- mesecna_placa %>% left_join(bdp, by = c("leto", "Država")) %>%
  left_join(indeks.bp, by = c("leto", "Država"))%>%left_join(St_potovanj_na_osebo, by = c("leto","Država"))
Gospodarske_značilnosti$leto <- as.numeric(Gospodarske_značilnosti$leto)

###############tretji del: demografske značilnosti.

#####Starost#### brez švedske ker ni podatkov za 2 leti (odstrani jo)

stran_starosti <- readHTMLTable("podatki/potovanja_po_starosti.html")
starost_15_24 <- stran_starosti[[5]]
starost_15_24 <- pivot_longer(starost_15_24,
                               cols = colnames(starost_15_24)[-1],
                               names_to = "leto", 
                               values_to = "15-24" 
)
starost_25_44 <- stran_starosti[[9]]
starost_25_44 <- pivot_longer(starost_25_44,
                               cols = colnames(starost_25_44)[-1],
                               names_to = "leto", 
                               values_to = "25-44" 
)
starost_45_64 <- stran_starosti[[13]]
starost_45_64 <- pivot_longer(starost_45_64,
                               cols = colnames(starost_45_64)[-1],
                               names_to = "leto", 
                               values_to = "45-64" 
)
starost_65 <- stran_starosti[[17]]
starost_65 <- pivot_longer(starost_65,
                               cols = colnames(starost_65)[-1],
                               names_to = "leto", 
                                values_to = "+65" 
)

Starost <- starost_15_24 %>% left_join(starost_25_44, by = c("leto", "TIMEGEO")) %>%
  left_join(starost_45_64, by = c("leto", "TIMEGEO")) %>% left_join(starost_65, by = c("leto", "TIMEGEO"))
colnames(Starost)[1] <- "Država"

match(Starost$Država, poimenovanje_drzav$drzava_anglesko)
Starost$Država = unlist(poimenovanje_drzav[match(Starost$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])
Starost <- subset(Starost, leto != "2009" & leto != "2010" & leto != "2011")
Starost <- data.frame(lapply(Starost, function(x) {gsub("\\(b\\)", "", x)}))
Starost <- data.frame(lapply(Starost, function(x) {gsub("\\(u\\)", "", x)}))
Starost <- data.frame(lapply(Starost, function(x) {gsub(",", "", x)}))
colnames(Starost)[3] <- "15-24"
colnames(Starost)[4] <- "25-44"
colnames(Starost)[5] <- "45-65"
colnames(Starost)[6] <- "+65"
Starost <- Starost %>% pivot_longer(cols ="15-24":"+65", names_to = "starost",values_to = "stevilo")
Starost <- Starost[order(Starost$starost),]
Starost$stevilo <- as.numeric(Starost$stevilo)

Starost<-Starost[!(Starost$Država=="Švedska"),]


sumStarost <- aggregate(Starost$stevilo, by= list(leto=Starost$leto, starost=Starost$starost), FUN = sum)

##indekse še za leta na prebivalca


#####Spol#####

spol <- readHTMLTable("podatki/potovanja_spol.html")
spol_skupaj <- spol[[1]]
spol_skupaj <- pivot_longer(spol_skupaj,
                              cols = colnames(spol_skupaj)[-1],
                              names_to = "leto", 
                              values_to = "st.potovanj" 
)
 spol_m <- spol[[5]]
spol_m <- pivot_longer(spol_m,
                              cols = colnames(spol_m)[-1],
                              names_to = "leto", 
                              values_to = "m" 
)
spol_z <- spol[[9]]
spol_z <- pivot_longer(spol_z,
                              cols = colnames(spol_z)[-1],
                              names_to = "leto", 
                              values_to = "z" 
)

Spol <- spol_skupaj %>% left_join(spol_m, by = c("leto", "TIMEGEO")) %>%
  left_join(spol_z, by = c("leto", "TIMEGEO"))
colnames(Spol)[1] <- "Država"
match(Spol$Država, poimenovanje_drzav$drzava_anglesko)
Spol$Država = unlist(poimenovanje_drzav[match(Spol$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])
Spol <- subset(Spol, leto != "2009" & leto != "2010" & leto != "2011")
Spol <- data.frame(lapply(Spol, function(x) {gsub("\\(b\\)", "", x)}))
Spol <- data.frame(lapply(Spol, function(x) {gsub(":", NA, x)}))
Spol <- data.frame(lapply(Spol, function(x) {gsub("\\(e\\)", "", x)}))
Spol <- data.frame(lapply(Spol, function(x) {gsub("\\,", "", x)}))
EU_Spol <- Spol %>%filter(Spol$Država=="EU")%>% pivot_longer(cols = m:z, names_to = "spol", values_to = "stevilo")
EU_Spol <- EU_Spol[order(EU_Spol$spol),]
EU_Spol$spol <- as.factor(EU_Spol$spol)
EU_Spol$stevilo <- as.numeric(EU_Spol$stevilo)

