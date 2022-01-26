# 2. faza: Uvoz podatkov

library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(rvest)
library(XML)
library(stringr)

sl <- locale("sl", decimal_mark=".", grouping_mark=",")

#Statistična znamenja	Pomen
#-	ni pojava
#...	ni podatka
#z	statistično zaupno
#M	manj zanesljiva ocena – previdna uporaba
#N	za objavo premalo zanesljiva ocena

#----------------------------------------------------------------------------------
#######izkazalo se je da te tabele ne bomo uporabili, ker je naslednja tabela iz sursa zanimivejša

## uvoz tabele zločina po vrstah zločina na 100 000 prebivalcev
## HTML


#zlocin1<- readHTMLTable("podatki/zlocin.html")
#zlocin1 <- zlocin1[["NULL"]]

# prevedem imena stolpcev
 
#colnames(zlocin1) <-  c("leto", "nameren umor", "poskus umora", "napad", "ugrabitev", "spolno nasilje", "posilstvo", "spolni napad", "rop", "vlom", "vlom v privatno posest", "kraja", "kraja prevoznaga sredstva", "zločin povezan z drogami")

#------------------------------------------------------------------------------------

#Polnoletni storilci zoper katere je bil kazenski postopek pri državnem tožilstvu končan 
#po kaznivem dejanju, spolu, starosti ob storitvi kaznivega dejanja in vrsti odločbe, Slovenija, letno

zlocinci  <- read_delim("podatki/zlocinci.csv",
                                     delim=";", col_names=TRUE, col_select=c(-2), na=c("...", 'M', 'N', 'z'), skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"))

# izberem samo glavne kategorije:

vzorec1 <- "\\d\\.[IVX][IVX]*\\s.*|[:alpha:].*"  
#stevilka.[IVX][IVX]*poljubnokrat\\s presleden.* nekaj poljubnokrat
zlocini2 <- unlist(str_extract_all(zlocinci$"KAZNIVO DEJANJE", vzorec1))
zlocinci <- zlocinci %>% filter(zlocinci$"KAZNIVO DEJANJE" %in% zlocini2)


#obrnem:
zlocinci <- pivot_longer(zlocinci, cols = colnames(zlocinci)[-1], 
                           names_to = 'spol.starost.leto',
                           values_to =  'stevilo obsodb')

#razbijem stolpec spol.starost.leto

zlocinci <- zlocinci %>%
  tidyr::extract(
    col = spol.starost.leto,
    into = c("spol.starost", "leto"),
    regex = "^(.*)\\s+([2][0][0-9]{2})$"
  )

zlocinci <- zlocinci %>%
  tidyr::extract(
    col = spol.starost,
    into = c("spol", "starost"),
    regex = "^([MoškiŽenske]*)\\s+(.*)$"
  )

colnames(zlocinci) <-c("dejanje", "spol", "starost", "leto", "obsojeni")



vzorcek = "([-]+)([0-9]*)"
zlocinci$obsojeni = str_replace_all(zlocinci$obsojeni , vzorcek,"0")

#a <- data.frame(zlocinci$starost)
#a <- data.frame(lapply(a, function(x) {gsub("Starost - SKUPAJ", "skupaj", x)}))
#zlocinci$starost <- a


#summary(zlocinci)
zlocinci[["spol"]] <- parse_factor(zlocinci[["spol"]], c("Moški", "Ženske"))
zlocinci$obsojeni <- parse_double(zlocinci$obsojeni)
zlocinci$leto <- parse_double(zlocinci$leto)
zlocinci$starost <- parse_factor(zlocinci[["starost"]], levels = c("Starost - SKUPAJ", "do 20 let", "od 21 do 30 let", "od 31 do 40 let", "od 41 do 50 let", "od 51 do 60 let", "nad 60 let"))


#-----------------------------------------------------------------------------------
## uvoz tabele obsojenih na 1000 prebivalcev po regijah

  
obsojeni_r <- read_delim("podatki/obsojeni.csv",
                         delim=";", col_names=TRUE, na=c("...", 'M', 'N', 'z'), skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"))




obsojeni_r <- pivot_longer(obsojeni_r, cols = colnames(obsojeni_r)[-1], 
                         names_to = 'leto',
                         values_to =  'obsojeni')


#transponiram :
#obsojeni_r <- pivot_wider(obsojeni_r,
#                        names_from = 'STATISTIČNA REGIJA',
#                        values_from = 'obsojeni')


# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(obsojeni_r$leto, vzorec))
obsojeni_r$leto <- leta

colnames(obsojeni_r)[1] <- "regija"


#vn zmečemo tujino
obsojeni_r<-obsojeni_r[!(obsojeni_r$`regija`=="Tujina"),]

obsojeni_r$obsojeni<- as.double(obsojeni_r$obsojeni)
#--------------------------------------------------------------------------------------
## uvoz tabele samoocene splošnega zadovoljstva
##(Samoocena splošnega zadovoljstva z življenjem po statističnih regijah, Slovenija, letno POVPREČJE)



zadovoljstvo <- read_delim("podatki/zadovoljstvo.csv",
                          delim=";", col_names=TRUE, na=c("...", 'M', 'N', 'z'), skip=2, locale=locale(encoding = "Windows-1250"))



zadovoljstvo <- pivot_longer(zadovoljstvo, cols = colnames(zadovoljstvo)[-1], 
                           names_to = 'leto',
                           values_to =  'zadovoljstvo')

colnames(zadovoljstvo)[1] <- "regija"
# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(zadovoljstvo$leto, vzorec))
zadovoljstvo$leto <- leta

#--------------------------------------------------------------------------------------
## uvoz Stopnja tveganja revščine, statistične regije, Slovenija, letno %

revscina <- read_delim("podatki/revscina.csv",
                      delim=";", col_names=TRUE, col_select=c(-1), na=c("...", 'M', 'N', 'z'), skip=2, locale=locale(encoding = "Windows-1250"))
    



revscina <- pivot_longer(revscina, cols = colnames(revscina)[-1], 
                             names_to = 'leto',
                             values_to =  'revscina')

colnames(revscina)[1] <- "regija"
#----------------------------------------------------------------------------------------
## uvoz Stanovanjske razmere, statistične regije, Slovenija, letno

stanovanje <- read_delim("podatki/stanovanje.csv",
                        delim=";", col_names=TRUE, na=c("...", 'M', 'N', 'z'), skip=2, locale=locale(encoding = "Windows-1250"))


stanovanje <- pivot_longer(stanovanje, cols = colnames(stanovanje)[-1], 
                             names_to = 'leto',
                             values_to =  'stanovanje')

colnames(stanovanje)[1] <- "regija"

# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(stanovanje$leto, vzorec))
stanovanje$leto <- leta

##-------------------------------------------------------------------------------------
##TABELA PO REGIJAH: tabele 'Samoocene splošnega zadovoljstva', 'Stopnja tveganja revščine',
##                      'Stanovanjske razmere' in 'Obsojenih na 1000 prebivalcev po regija'
##          združim v skupno tabelo

# v tabeli 'Samoocene splošnega zadovoljstva' je najstarejši podatek iz leta 2012,
# zato starejše podatke ostalih tabel izpustim:

obsojeni_r <- obsojeni_r %>% filter(leto>=2012)
revscina <- revscina %>% filter(leto>=2012)
stanovanje <- stanovanje %>% filter(leto>=2012)


tabela_r <- obsojeni_r %>% left_join(zadovoljstvo, by=c("regija","leto")) %>%
            left_join(revscina, by=c("regija", "leto")) %>% 
            left_join(stanovanje, by=c("regija", "leto"))
            
tabela_r$leto<- as.double(tabela_r$leto)
##--------------------------------------------------------------------------------------
## uvoz tabele obsojenih na 1000 prebivalcev po občinah


obsojeni_o <- read_delim("podatki/obsojenio.csv", 
                         delim=";", col_names=TRUE, na=c("...", 'M', 'N', 'z'), skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"))



obsojeni_o <- pivot_longer(obsojeni_o, cols = colnames(obsojeni_o)[-1], 
                           names_to = 'leto',
                           values_to =  'obsojeni')

obsojeni_o <- data.frame(lapply(obsojeni_o, function(x) {gsub("-", 0, x)}))

colnames(obsojeni_o)[1] <- "obcina"

# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(obsojeni_o$leto, vzorec))
obsojeni_o$leto <- leta

#vn zmečemo tujino
obsojeni_o<-obsojeni_o[!(obsojeni_o$`obcina`=="Tujina"),]
##--------------------------------------------------------------------------------
## uvoz tabele povprečne plače po občinah 
#(za 2021 je ze narejen stolpec, vendar vrednosti niso označene kot NA -> vn uzemi stolpec)

placa <- read_delim("podatki/placao.csv", 
                    delim=";", col_names=TRUE, na=c("...", 'M', 'N', 'z'),col_select=c(-1, -"2021"), skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"),
                    col_types = cols(
                      .default = col_guess(), 
                      "2020" = col_double()
                    )
                    )


placa <- data.frame(lapply(placa, function(x) {gsub("-", 0, x)}))


placa <- pivot_longer(placa, cols = colnames(placa)[-1], 
                           names_to = 'leto',
                           values_to =  'placa')

colnames(placa)[1] <- "obcina"

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(placa$leto, vzorec))
placa$leto <- leta


##--------------------------------------------------------------------------------
##  TABELA PO OBČINAH: tabeli ' obsojenih na 1000 prebivalcev po občinah' in 'povprečne plače po občinah'
##                      združim v skupno tabelo

#ker je najstarejši podatek za plače iz 2008:

o <- obsojeni_o %>% filter(leto>=2008)

tabela_o <- o %>% left_join(placa, by=c("obcina","leto")) 

tabela_o$leto<- as.double(tabela_o$leto)
tabela_o$obsojeni<- as.double(tabela_o$obsojeni)
tabela_o$placa<- as.double(tabela_o$placa)
##--------------------------------------------------------------------------------
# tabela1 ki nam bo dala najbolj pogoste zločine -- dodala kasneje
# s tabela2 nato rišem graf, ker ima urejene vrednosti

tabela1 <- zlocinci %>% filter(starost == "Starost - SKUPAJ") %>% filter(dejanje != "KAZNIVO DEJANJE - SKUPAJ") %>%
  group_by(dejanje) %>% summarise(obsojeni =sum(obsojeni)) %>% arrange(desc(obsojeni)) %>% head(10)
tabela2 <- zlocinci%>% filter(starost != "Starost - SKUPAJ")%>% filter(dejanje %in% tabela1$dejanje)

tabela2$starost = str_replace_all(tabela2$starost ,"do 20 let" ,"do 20")
tabela2$starost = str_replace_all(tabela2$starost ,"od 21 do 30 let" ,"21-30")
tabela2$starost = str_replace_all(tabela2$starost ,"od 31 do 40 let" ,"31-40")
tabela2$starost = str_replace_all(tabela2$starost ,"od 41 do 50 let" ,"41-50")
tabela2$starost = str_replace_all(tabela2$starost ,"od 51 do 60 let" ,"51-60")
tabela2$starost = str_replace_all(tabela2$starost ,"nad 60 let" ,"od 60")
tabela2$starost <- parse_factor(tabela2[["starost"]], levels = c("do 20", "21-30", "31-40", "41-50", "51-60", "od 60"))
