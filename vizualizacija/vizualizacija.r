# 3. faza: Vizualizacija podatkov


library(tidyverse)
library(ggplot2)
library(dplyr)

source("uvoz/uvoz.r", encoding="UTF-8")

#torej mam tri glavne tabele s katerimi želim risati grafe:
#zlocinci, tabela_r in tabela_o



g1 <- tabela_r %>% filter(regija!="SLOVENIJA") %>%
  ggplot(
    mapping = aes(x = leto, y = obsojeni, color = regija)
  ) +
  ylab("Število obsojenih na 1000 prebivalcev") + 
  geom_line() + geom_point()
  

print(g1)
            

g2 <- tabela_r %>% filter(regija!="SLOVENIJA") %>%
  ggplot(
    mapping = aes(x = zadovoljstvo, y = obsojeni, color= revscina , size= stanovanje)
  ) +
  ylab("Število obsojenih na 1000 prebivalcev")  +
  xlab("Samoocena splošnega zadovoljstva z življenjem") + 
  geom_point(position = position_jitter(width = 0.10))      
             
print(g2)



g3 <- zlocinci %>% filter(dejanje == "KAZNIVO DEJANJE - SKUPAJ") %>%
  filter(starost != "skupaj") %>%
  ggplot(
    mapping = aes(x = leto, y = obsojeni ,  fill = spol)
  )  +
  ylab("Število obsojenih") + 
  geom_col(
    position = position_dodge()
  )
             
print(g3)


g4 <- zlocinci %>% filter(starost != "Starost - SKUPAJ") %>%
  group_by(starost) %>%
  ggplot(
    mapping = aes(x = leto, y = obsojeni ,  fill = starost)
  )  +
  ylab("Število obsojenih") + 
  geom_col()

print(g4)

########################################## ZEMLJEVIDI #########################################################
tabela <- tabela_o %>% filter(leto =="2013") %>% filter(obcina != "SLOVENIJA" ) 

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

tm_shape(obcine.obsojeni.zemljevid) +
  tm_polygons("obsojeni", popup.vars = c("Število obsojenih na 1000 preb: " = "obsojeni")) + tm_legend(show=FALSE)
tmap_mode("view") 






