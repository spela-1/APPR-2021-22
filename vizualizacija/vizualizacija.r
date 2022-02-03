# 3. faza: Vizualizacija podatkov


library(tidyverse)
library(ggplot2)
library(dplyr)

source("uvoz/uvoz.r", encoding="UTF-8")

#torej mam tri glavne tabele s katerimi želim risati grafe:
#zlocinci, tabela_r in tabela_o



g1 <- tabela_r %>% filter(regija!="SLOVENIJA") %>%
  ggplot(mapping = aes(x = leto, y = obsojeni, color = regija))+
  labs(x = "leto",
    y = "Število obsojenih na 1000 prebivalcev",
    title = "Število obsojenih po slovenskih regijah") + 
  geom_line() + geom_point() 
  


            

g2 <- tabela_r %>% filter(regija!="SLOVENIJA") %>%
  ggplot(
    mapping = aes(x = zadovoljstvo, y = obsojeni, color= revscina , size= stanovanje)
  ) +
  labs(y = "Število obsojenih na 1000 prebivalcev",
       x = "Samoocena splošnega zadovoljstva z življenjem",
       title = "Število obsojenih glede na socioekonomska stanja",
       color = "Stopnja revščine",
       size =  "Delež slabih stanovanj") + 
  geom_point(position = position_jitter(width = 0.10))     
             



g3 <- zlocinci %>% filter(dejanje == "KAZNIVO DEJANJE - SKUPAJ") %>%
  filter(starost != "skupaj") %>%
  ggplot(
    mapping = aes(x = leto, y = obsojeni ,  fill = spol)
  )  +
  labs(x = "Leto", y = "Število obsojenih", title = "Število obsojenih po spolu", fill = "Spol") + 
  geom_col(
    position = position_dodge()
  )
             



g4 <- zlocinci %>% filter(starost != "Starost - SKUPAJ") %>%
  group_by(starost) %>%
  ggplot(
    mapping = aes(x = leto, y = obsojeni ,  fill = starost)
  )  +
  labs(x = "Leto", y = "Število obsojenih", title = "Število obsojenih po starostnih skupinah", fill = "Starostna skupina")+ 
  geom_col()





g5 <- tabela2 %>%filter(leto == 2020)%>% 
  ggplot(
    mapping = aes(x = starost, y = obsojeni, fill=dejanje)
  ) +
  labs(y = "Število obsojenih", x = "Starost zločincev", fill = "KAZNIVA DEJANJA ZOPER",
       title = "Deset najpogostejših zločinov glede na spol in starost zločincev")+
  geom_col() + facet_wrap(.~ spol,ncol = 5)






g6 <- tabela_o %>% filter(obcina!="SLOVENIJA") %>% filter(leto >2010)%>%
  ggplot(
    mapping = aes(x = placa, y = obsojeni)
  ) +
  labs(y = "Število obsojenih na 1000 prebivalcev", x = "Povprečna plača",
       title  = "Število obsojenih glede na povprečno plačo po letih") + 
  scale_y_log10() +
  geom_point(position = position_jitter(width = 0.10)) +
  facet_wrap(.~ leto,ncol = 5)


# zdi se kot da spremenljivki nista najbolj povezani, zato bo opustila analiz med njima

#--------------------------------------------------------------------------------
############################### ZEMLJEVID ######################################

tabela <- tabela_o %>% filter(leto =="2015") %>% filter(obcina != "SLOVENIJA" ) 

library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(tmap)

source("lib/uvozi.zemljevid.r")
obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                          pot.zemljevida="OB", encoding="Windows-1250")

tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)
obcine$OB_UIME <- factor(obcine$OB_UIME)


lvls <- levels(obcine$OB_UIME)
obsojeni.zemljevid <- unique(tabela$obcina) %>% sort()

razlicni <- lvls!= obsojeni.zemljevid

primerjava <- data.frame(obcina.zemljevid = parse_character(lvls),
                         obcina.obsojeni = obsojeni.zemljevid)[razlicni,]

obsojeni.na.zemljevidu <- tabela%>% 
  left_join(primerjava, by = c("obcina" = "obcina.obsojeni")) %>%
  mutate(obcina = ifelse(is.na(obcina.zemljevid), obcina, obcina.zemljevid) %>% factor()) %>%
  dplyr::select(-obcina.zemljevid)

obcine.obsojeni.zemljevid <- merge(obcine, obsojeni.na.zemljevidu,
                                by.x = "OB_UIME", by.y = "obcina")


tmap_mode("view")
map <- tm_shape(obcine.obsojeni.zemljevid) +
  tm_polygons("obsojeni", popup.vars = c("Število obsojenih na 1000 preb: " = "obsojeni")) + tm_legend(show=FALSE)


