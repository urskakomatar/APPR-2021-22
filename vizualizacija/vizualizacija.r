# 3. faza: Vizualizacija podatkov
library(ggplot2)
library(gganimate)
library(tidyverse)
library(tmap)
library(plotly)
library(sf)
######Število potovanj glede na spol EU#####

EU_Spol%>% ggplot(mapping = aes(x = leto, y = stevilo ,color = spol, group=1))+ geom_line()  + geom_point()

ggplot(EU_Spol) + geom_line(aes(x = leto, y = stevilo, color = spol, group = spol))
 
 
#####Število potovanj glede na starost EU###### ok 

sumStarost %>% ggplot() + geom_col(data = sumStarost, aes(x = leto, y = x, fill = starost)) + labs(
  x = "Leto",
  y = "Število potovanj",
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

g3 + average_indeks%>%filter(Država == "EU") %>% geom_bar(
  mapping = aes(x = Država, y = x) ,
  stat = "identity",
  fill = "red"
)


#########min.plača, bdppc, št.preb. in velikost indeksa
g4 <- Gospodarske_značilnosti[1:27,] %>%
  ggplot(
    mapping = aes(x = vsota, y = bdppc)
  ) +
  geom_count(
    aes(color = indeks,size = število.prebivalcev.y)
  ) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
#g4 <- ggplotly(g4,tooltip = "Država")
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

g6 <- Gospodarske_značilnosti[190:216,] %>%
  ggplot(
    mapping = aes(x =bdppc ,y =indeks.bp,  size = indeks , color = Država)
  ) +
  geom_point()

######zemljevid#########
###tukaj bo še tabelica indeksa, kjer bodo popravljena imena nazaj v angl. da lahko mergamo##
ang_indeks <-  average_indeks
match(ang_indeks$Država, poimenovanje_drzav$drzave)
ang_indeks$Država = unlist(poimenovanje_drzav[match(ang_indeks$Država, poimenovanje_drzav$drzave), "drzava_anglesko"])
ang_indeks$Država[ang_indeks$Država == "Czech Republic" ] <- "Czech Rep."
colnames(ang_indeks)[1] <- "name"
podatki <- ang_indeks %>% left_join(evropa, by = "name")
podatki <- podatki[!(podatki$name == "Cyprus"| podatki$name == "Malta"| podatki$name == "Slovak Republic"|
  podatki$name == "European Union - 27 countries (from 2020)"),]
data("World")


st_potovanj_graf <- function() {
  podatki <- st_sf(ang_indeks %>% left_join(evropa, by = "name"))
  podatki <- podatki[!(podatki$name == "Cyprus"| podatki$name == "Malta"| podatki$name == "Slovak Republic"|
                         podatki$name == "European Union - 27 countries (from 2020)"),]
  evropa <- tm_shape(podatki) + tm_polygons('x')
  tmap_mode('view')
  return(evropa)
} 

st_potovanj_graf()





