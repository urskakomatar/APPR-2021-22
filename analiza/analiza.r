# 4. faza: Napredna analiza podatkov

#za leto 2019

library(ggalt)
library(cluster)
library(sp)
library(rgdal)
library(raster) # funkcija crop
library(rgeos)



 average_indeks1 <- average_indeks[-27,]
 graf_poskusni <- average_indeks1%>%ggplot()+
   geom_point(
   mapping = aes(x = x, y = x),
   size = 2
 ) + geom_label(
   mapping = aes(x = x, y = x, label = Država),
 ) + theme_classic()
 
 graf_poskusni
 
 
 dendrogram = average_indeks1[, -1] %>%
   dist() %>%
   hclust()
 
 plot(
   dendrogram,
   labels = average_indeks1$Država,
   ylab = "indeks",
   main = NULL
 )
 
 skupine.2 = dendrogram %>% cutree(k = 2) %>% as.ordered()

 
 library(dplyr)
 
 diagram.skupine = function(podatki, oznake, skupine, k) {
   podatki = podatki %>%
     bind_cols(skupine) %>%
     rename(skupina = ...3)
   
   d = podatki %>%
     ggplot(
       mapping = aes(
         x = x, y = x, color = skupina
       )
     ) +
     geom_point() + labs(x = "indeks", y = "indeks", title = sprintf("Razvrstitev držav v 2 skupini"))+
     geom_label(label = oznake, size = 2) +
     scale_color_hue() +
     theme_classic()
   
   for (i in 1:k) {
     d = d + geom_encircle(
       data = podatki %>%
         filter(skupina == i)
     )
   }
   d
 }
 k <- 2
diagram <- diagram.skupine(average_indeks1, average_indeks1$Država, skupine.2, k)
# 
# average_indeks1 %>%
#   bind_cols(skupine.2) %>%
#   rename(skupina = ...3)
# 
# 
# rlang::last_error()




###Lin.regr. gledamo odvisnost bdppc od min.place
Gosp_zn_19 <- Gospodarske_značilnosti[190:216,]
g_12 <- ggplot(Gospodarske_značilnosti, aes(x=bdppc, y=vsota)) + geom_point() + 
  labs(x = "BDPpc", y = "minimalna plača", title = sprintf("Linearna regresija"))
g_12 + geom_smooth(method="lm")
linearna_regresija <- g_12 + xlim(0,100000) + geom_smooth(method="lm", fullrange=TRUE)
linearna_regresija 
# 



