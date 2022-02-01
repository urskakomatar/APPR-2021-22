# 3. faza: Vizualizacija podatkov
library(ggplot2)
library(gganimate)
library(tidyverse)
library(tmap)
library(plotly)
library(sf)
######Število potovanj glede na spol EU#####


g1 <- ggplot(EU_Spol) + geom_line(aes(x = leto, y = stevilo/10^6, color = spol, group = spol))+ labs(
  x = "Leto",
  y = "Število ljudi v mio",
  title = sprintf("Število potovanj glede na spol"))
 
 
#####Število potovanj glede na starost EU###### ok 

g2 <- sumStarost %>% ggplot() + geom_col(data = sumStarost, aes(x = leto, y = x/10^6, fill = starost)) + labs(
  x = "Leto",
  y = "Število potovanj v mio",
  title = sprintf("Število potovanj glede na starost"))



#######indeks v primerjavi z državami####osnovni graf nekako
g3 <- average_indeks %>%
  ggplot(
    mapping = aes(x = reorder(Država,-x), y = x) 
  ) +
  geom_bar(stat = "identity") + labs(
    x = "Država",
    y = "Št. potovanj na osebo",
    title = sprintf("Število potovanj na osebo v EU")
  ) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

g3 <- g3 + average_indeks%>%filter(Država == "EU") %>% geom_bar(
  mapping = aes(x = Država, y = x) ,
  stat = "identity",
  fill = "red"
)
g3 <- g3 + average_indeks%>%filter(Država == "Slovenija") %>% geom_bar(
  mapping = aes(x = Država, y = x) ,
  stat = "identity",
  fill = "green"
)

#########min.plača, bdppc, št.preb. in velikost indeksa
gosp.zn. <- Gospodarske_značilnosti
colnames(gosp.zn.)[8] <- "št.prebivalcev"
g4 <- gosp.zn.[1:27,] %>%
  ggplot(
    mapping = aes(x = vsota, y = bdppc)
  ) +
  geom_count(
    aes(color = indeks,size = št.prebivalcev)
  ) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(title=sprintf("Povezava z gospodarskimi značilnostmi"))
g4 <- ggplotly(g4,tooltip = "št.prebivalcev")
g4

####bdpcc, brezposelnost indeks in vse države
g5 <- Gospodarske_značilnosti[1:27,] %>%
  ggplot(
    mapping = aes(x =bdppc ,y =indeks.bp,  size = indeks , color = Država)
  ) + labs(
    x = "BDP per capita",
    y = "indeks brezposelnosti",
    title = sprintf("Št.potovanj v odvisnosti od brezposelnosti in BDPpc")
  )+
  geom_point() + theme(legend.position = "none")
g5 <- ggplotly(g5,tooltip = "Država")
g5

######zemljevid#########
###tukaj bo še tabelica indeksa, kjer bodo popravljena imena nazaj v angl. da lahko mergamo##
data("World")
ang_indeks <-  average_indeks
match(ang_indeks$Država, poimenovanje_drzav$drzave)
ang_indeks$Država = unlist(poimenovanje_drzav[match(ang_indeks$Država, poimenovanje_drzav$drzave), "drzava_anglesko"])
ang_indeks$Država[ang_indeks$Država == "Czech Republic" ] <- "Czech Rep."
colnames(ang_indeks)[1] <- "name"
podatki <- ang_indeks %>% left_join(World, by = "name")
podatki <- podatki[!(podatki$name == "Cyprus"| podatki$name == "Malta"| podatki$name == "Slovak Republic"|
  podatki$name == "European Union - 27 countries (from 2020)"),]



st_potovanj_graf <- function() {
  podatki <- st_sf(ang_indeks %>% left_join(World, by = "name"))
  podatki <- podatki[!(podatki$name == "Cyprus"| podatki$name == "Malta"| podatki$name == "Slovak Republic"|
                         podatki$name == "European Union - 27 countries (from 2020)"),]
  evropa <- tm_shape(podatki) + tm_polygons('x')
  tmap_mode('view')
  return(evropa)
} 

mapa1 <- st_potovanj_graf()
tmap_mode("view")





