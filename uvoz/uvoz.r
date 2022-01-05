# 2. faza: Uvoz podatkov
library(readr)
library(dplyr)
library(XML)
library(tidyverse)

##Slovarček držav##
poimenovanje_drzav = tibble(
  drzava_anglesko = c("Austria", "Belgium","Bulgaria", "Czech Republic", "Czechia", "Denmark", "Finland", "France", "Germany","Germany (until 1990 former territory of the FRG)",
                      "Greece", "Hungary", "Ireland",
                      "Italy", "Luxembourg", "Netherlands", "Poland", "Portugal","Romania", "Slovak Republic","Slovakia", "Spain", "Sweden", "Slovenia",
                      "Estonia", "Latvia", "Lithuania","Malta","Cyprus","Croatia"),
  drzave = c("Avstrija", "Belgija","Bolgarija", "Češka","Češka", "Danska", "Finska", "Francija", "Nemčija","Nemčija", "Grčija", "Madžarska", "Irska",
             "Italija", "Luksemburg", "Nizozemska", "Poljska", "Portugalska","Romunija", "Slovaška","Slovaška", "Španija", "Švedska", "Slovenija",
             "Estonija", "Latvija", "Litva","Malta","Ciper","Hrvaška")
);


############Prva tabela: Indeks števila potovanj na osebo###########

#število potovanj# 

st_potovanj <- readHTMLTable("podatki/st_potovanj_skupaj_m_z.html")
st_potovanj <- st_potovanj[["NULL"]]

st_potovanj <- st_potovanj %>% select(Država = TIMEGEO, "2012", "2013",
                                      "2014", "2015", "2016", "2017", "2018", "2019")

match(st_potovanj$Država, poimenovanje_drzav$drzava_anglesko)
st_potovanj$Država = unlist(poimenovanje_drzav[match(st_potovanj$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])

str_replace_all(st_potovanj$"2012", "(b)", "")
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub("b", "", x)}))
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub("\\(","", x)}))
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub("\\)","", x)}))
st_potovanj <- data.frame(lapply(st_potovanj, function(x) {gsub("\\,","", x)}))

st_potovanj <- st_potovanj %>% select(Država, "2012" = X2012, "2013" = X2013,
                                      "2014" = X2014, "2015" = X2015, "2016" = X2016, "2017" = X2017, "2018" = X2018, "2019" = X2019)


#število prebivalcev# men se zdi kr uredu , odlicno
st_prebivalcev <- read.csv("podatki/st_prebivalcev_eu.csv")
st_prebivalcev <- st_prebivalcev %>% select(Država = Country.Name, "2009" = "X2009..YR2009.",
"2010" = "X2010..YR2010.", "2011" = "X2011..YR2011.", "2012" = "X2012..YR2012.", "2013" = "X2013..YR2013.",
"2014" = "X2014..YR2014.", "2015" = "X2015..YR2015.", "2016" = "X2016..YR2016.", "2017" = "X2017..YR2017.",
"2018" = "X2018..YR2018.", "2019" = "X2019..YR2019.")

match(st_prebivalcev$Država, poimenovanje_drzav$drzava_anglesko)
st_prebivalcev$Država = unlist(poimenovanje_drzav[match(st_prebivalcev$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])
st_prebivalcev <- st_prebivalcev[-c(28:32), ]


#indeks št.potovanj na prebivalca# nekak skupi po komponentah deliš al kaj



##########Drugi del: gospodarske značilnosti, BDP#########






#########Letna plača (povprečna)##############

letna_placa <- read.csv("podatki/letna_placa.csv")
letna_placa <- letna_placa %>% select(Država = Country, Leto = Time, valuta = Unit, vsota = Value)

# leta v vrstici ygoraj, pretvorba valut, dodam nov stolpec da bo samo eur,
#zbrišem valute

match(letna_placa$Država, poimenovanje_drzav$drzava_anglesko)

letna_placa$Država = unlist(poimenovanje_drzav[match(letna_placa$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])


#pretvornik valut (nevem zakaj kompliciram zivljenje s tem)

#Czech Koruna 0,040 evro
#Danish Krone 0,13 evro
#Forint 0,0027 evro
#Zloty 0,22 evro
#Swedish Krona 0,097 evro

pretvornik_valut = c("Euro", "Czech Koruna", "Danish Krone","Forint", "Zloty", "Swedish Krona")


###########Brezposelnost############ klukca

brezposelnost <- readHTMLTable("podatki/brezposelnost_v_tisocih.html")
brezposelnost <- brezposelnost[["NULL"]]

brezposelnost <- brezposelnost %>% select(Država = TIMEGEO, "2012"="2012", "2013"="2013",
                                      "2014"="2014", "2015"="2015", "2016"="2016", "2017"="2017", "2018"="2018", "2019"="2019")
match(st_potovanj$Država, poimenovanje_drzav$drzava_anglesko)

brezposelnost$Država = unlist(poimenovanje_drzav[match(brezposelnost$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])

brezposelnost <- data.frame(lapply(brezposelnost, function(x) {gsub("b", "", x)}))
brezposelnost <- data.frame(lapply(brezposelnost, function(x) {gsub("\\(","", x)}))
brezposelnost <- data.frame(lapply(brezposelnost, function(x) {gsub("\\)","", x)}))

brezposelnost <- pivot_longer(brezposelnost,
                                cols = colnames(brezposelnost)[-1],
                                names_to = "leto", 
                                values_to = "brezposelnost" 
)


##########BDP############ kljukca

bdp <- readHTMLTable("podatki/BDP_drzave_eu.html")
bdp <- bdp[["NULL"]]

bdp <- bdp %>% select(Država = TIMEGEO, "2012" = "2012Q1", "2013" = "2013Q1",
                                      "2014" = "2014Q1", "2015" = "2015Q1",
                      "2016" = "2016Q1", "2017" = "2017Q1", "2018" = "2018Q1", "2019" = "2019Q1")

match(bdp$Država, bdp$drzava_anglesko)
bdp$Država = unlist(poimenovanje_drzav[match(bdp$Država, poimenovanje_drzav$drzava_anglesko), "drzave"])

bdp <- data.frame(lapply(bdp, function(x) {gsub("p", "", x)}))
bdp <- data.frame(lapply(bdp, function(x) {gsub("b", "", x)}))
bdp <- data.frame(lapply(bdp, function(x) {gsub("\\(","", x)}))
bdp <- data.frame(lapply(bdp, function(x) {gsub("\\)","", x)}))
bdp <- data.frame(lapply(bdp, function(x) {gsub("\\.","", x)}))

bdp <- bdp %>% select(Država, "2012" = X2012, "2013" = X2013,
                                      "2014" = X2014, "2015" = X2015, "2016" = X2016, "2017" = X2017, "2018" = X2018, "2019" = X2019)

bdp <- pivot_longer(bdp,
                              cols = colnames(bdp)[-1],
                              names_to = "leto", 
                              values_to = "BDP" 
)


###############tretji del: demografske značilnosti. Spol, kako nej to nardim, spet indeks?!

#####Starost####

stran_starosti <- readHTMLTable("podatki/potovanja_po_starosti.html")
starost_15_do_24 <- stran_starosti[[5]]









