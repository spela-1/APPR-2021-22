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
## uvoz tabele zločina po vrstah zločina na 100 000 prebivalcev
## HTML


zlocin1<- readHTMLTable("podatki/zlocin.html")
zlocin1 <- zlocin1[["NULL"]]

# prevedem imena stolpcev
 
colnames(zlocin1) <-  c("leto", "nameren umor", "poskus umora", "napad", "ugrabitev", "spolno nasilje", "posilstvo", "spolni napad", "rop", "vlom", "vlom v privatno posest", "kraja", "kraja prevoznaga sredstva", "zločin povezan z drogami")

#------------------------------------------------------------------------------------

#Polnoletni storilci zoper katere je bil kazenski postopek pri državnem tožilstvu končan 
#po kaznivem dejanju, spolu, starosti ob storitvi kaznivega dejanja in vrsti odločbe, Slovenija, letno

zlocinci <- obsojeni_r <- read_delim("podatki/zlocinci.csv",
                                     delim=";", col_names=TRUE, col_select=c(-2), na="...", skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"))

# izberem samo glavne kategorije:
vzorec1 <- "\\d\\.[IVX][IVX]*\\s.*"  
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

# ne zanimajo me podatki po starostnih skupinah

zlocinci <- zlocinci %>% filter(zlocinci$"starost" == "Starost - SKUPAJ") %>% select(-"starost")

#-----------------------------------------------------------------------------------
## uvoz tabele obsojenih na 1000 prebivalcev po regijah

  
obsojeni_r <- read_delim("podatki/obsojeni.csv",
                         delim=";", col_names=TRUE, na="...", skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"))


#transponiram :

obsojeni_r <- pivot_longer(obsojeni_r, cols = colnames(obsojeni_r)[-1], 
                         names_to = 'leto',
                         values_to =  'Stevilo obsojenih na 1000 prebivalcev')

#obsojeni_r <- pivot_wider(obsojeni_r,
#                        names_from = 'STATISTIČNA REGIJA',
#                        values_from = 'Stevilo obsojenih na 1000 prebivalcev')

# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(obsojeni_r$leto, vzorec))
obsojeni_r$leto <- leta


# ročno izbrišem stolpec tujina, ker je povsod NA
# obsojeni_r$'Tujina' <- NULL   ko je bila tabela transponirana

#--------------------------------------------------------------------------------------
## uvoz tabele samoocene splošnega zadovoljstva
##(Samoocena splošnega zadovoljstva z življenjem po statističnih regijah, Slovenija, letno POVPREČJE)



zadovoljstvo <- read_delim("podatki/zadovoljstvo.csv",
                          delim=";", col_names=TRUE, na="-", skip=2, locale=locale(encoding = "Windows-1250"))



zadovoljstvo <- pivot_longer(zadovoljstvo, cols = colnames(zadovoljstvo)[-1], 
                           names_to = 'leto',
                           values_to =  'Samoocene splošnega zadovoljstva')


#transponiram :
#zadovoljstvo <- pivot_wider(zadovoljstvo,
#                         names_from = 'STATISTIČNA REGIJA',
#                          values_from = 'Samoocene splošnega zadovoljstva')

# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(zadovoljstvo$leto, vzorec))
zadovoljstvo$leto <- leta

#--------------------------------------------------------------------------------------
## uvoz Stopnja tveganja revščine, statistične regije, Slovenija, letno %

revscina <- read_delim("podatki/revscina.csv",
                      delim=";", col_names=TRUE, col_select=c(-1), na="-", skip=2, locale=locale(encoding = "Windows-1250"))
    



revscina <- pivot_longer(revscina, cols = colnames(revscina)[-1], 
                             names_to = 'leto',
                             values_to =  'Stopnja tveganja revščine')


#transponiram :
#revscina <- pivot_wider(revscina,
#                            names_from = 'STATISTIČNA REGIJA',
#                            values_from = 'Stopnja tveganja revščine')

#pri tej tabeli so leta že urejena

#----------------------------------------------------------------------------------------
## uvoz Stanovanjske razmere, statistične regije, Slovenija, letno

stanovanje <- read_delim("podatki/stanovanje.csv",
                        delim=";", col_names=TRUE, na="-", skip=2, locale=locale(encoding = "Windows-1250"))


stanovanje <- pivot_longer(stanovanje, cols = colnames(stanovanje)[-1], 
                             names_to = 'leto',
                             values_to =  'Stanovanjske razmere')

#transponiram :
#stanovanje <- pivot_wider(stanovanje,
#                            names_from = 'STATISTIČNA REGIJA',
#                            values_from = 'Stanovanjske razmere')



# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(stanovanje$leto, vzorec))
stanovanje$leto <- leta

##-------------------------------------------------------------------------------------
##TABELA3: tabele 'Samoocene splošnega zadovoljstva', 'Stopnja tveganja revščine' in 'Stanovanjske razmere'
##          združim v skupno tabelo

# v tabeli 'Samoocene splošnega zadovoljstva' je najstarejši podatek iz leta 2012,
# zato starejše podatke iz drugih dveh tabel izpustim:
#     tega ubistvu ni treba, ker pri združevanju tabel že sam naredi

#revscina <- revscina %>% filter(leto>=2012)
#stanovanje <- stanovanje %>% filter(leto>=2012)

tabela3 <- zadovoljstvo %>% left_join(revscina, by=c("STATISTIČNA REGIJA", "leto")) %>% left_join(stanovanje, by=c("STATISTIČNA REGIJA", "leto"))
##--------------------------------------------------------------------------------------
## uvoz tabele obsojenih na 1000 prebivalcev po občinah


obsojeni_o <- read_delim("podatki/obsojenio.csv", 
                         delim=";", col_names=TRUE, na="-", skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"))



#transponiram :

obsojeni_o <- pivot_longer(obsojeni_o, cols = colnames(obsojeni_o)[-1], 
                           names_to = 'leto',
                           values_to =  'x')

obsojeni_o <- pivot_wider(obsojeni_o,
                          names_from = 'OBČINE',
                          values_from = 'x')

# uredim leta:

vzorec <- "[2][0][0-9]{2}"
leta <- unlist(str_extract_all(obsojeni_o$leto, vzorec))
obsojeni_o$leto <- leta




##--------------------------------------------------------------------------------
## uvoz tabele povprečne plače po občinah 
#(za 2021 je ze narejen stolpec, vendar vrednosti niso označene kot NA -> vn uzemi stolpec)

placa <- read_delim("podatki/placao.csv", 
                    delim=";", col_names=TRUE, na="-",col_select=c(-1, -"2021"), skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"),
                    col_types = cols(
                      .default = col_guess(), 
                      "2020" = col_double()
                    )
                    )


#transponiram :

placa <- pivot_longer(placa, cols = colnames(placa)[-1], 
                           names_to = 'leto',
                           values_to =  'x')

placa <- pivot_wider(placa,
                          names_from = 'OBČINE',
                          values_from = 'x')



# uredim leta:

vzorec <- "[2][0][0-9]{2}"

leta <- unlist(str_extract_all(placa$leto, vzorec))

placa$leto <- leta

