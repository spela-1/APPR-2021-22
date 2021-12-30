# 2. faza: Uvoz podatkov

library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(rvest)
library(XML)

sl <- locale("sl", decimal_mark=".", grouping_mark=",")



#----------------------------------------------------------------------------------
## uvoz tabele zločina po vrstah zločina na 100 000 prebivalcev
## HTML


zlocin<- readHTMLTable("podatki/zlocin.html")
zlocin <- zlocin[["NULL"]]



#-----------------------------------------------------------------------------------
## uvoz tabele obsojenih na 1000 prebivalcev po regijah

  
obsojeni_r <- read_delim("podatki/obsojeni.csv", 
                         delim=";", col_names=TRUE, na="-", skip=2, locale=locale(decimal_mark= ".", encoding = "Windows-1250"))


#transponiram :

obsojeni_r <- pivot_longer(obsojeni_r, cols = colnames(obsojeni_r)[-1], 
                         names_to = 'leto',
                         values_to =  'x')

obsojeni_r <- pivot_wider(obsojeni_r,
                        names_from = 'STATISTIČNA REGIJA',
                        values_from = 'x')

#--------------------------------------------------------------------------------------
## uvoz tabele samoocene splošnega zadovoljstva
##(Samoocena splošnega zadovoljstva z življenjem po statističnih regijah, Slovenija, letno POVPREČJE)



zadovoljstvo <- read_delim("podatki/zadovoljstvo.csv",
                          delim=";", col_names=TRUE, na="-", skip=2, locale=locale(encoding = "Windows-1250"))


#transponiram :

zadovoljstvo <- pivot_longer(zadovoljstvo, cols = colnames(zadovoljstvo)[-1], 
                           names_to = 'leto',
                           values_to =  'x')

zadovoljstvo <- pivot_wider(zadovoljstvo,
                          names_from = 'STATISTIČNA REGIJA',
                          values_from = 'x')

#--------------------------------------------------------------------------------------
## uvoz Stopnja tveganja revščine, statistične regije, Slovenija, letno %

revscina <- read_delim("podatki/revscina.csv",
                      delim=";", col_names=TRUE, col_select=c(-1), na="-", skip=2, locale=locale(encoding = "Windows-1250"))
    

#transponiram :

revscina <- pivot_longer(revscina, cols = colnames(revscina)[-1], 
                             names_to = 'leto',
                             values_to =  'x')

revscina <- pivot_wider(revscina,
                            names_from = 'STATISTIČNA REGIJA',
                            values_from = 'x')

#----------------------------------------------------------------------------------------
## uvoz Stanovanjske razmere, statistične regije, Slovenija, letno

stanovanje <- read_delim("podatki/stanovanje.csv",
                        delim=";", col_names=TRUE, na="-", skip=2, locale=locale(encoding = "Windows-1250"))


#transponiram :

stanovanje <- pivot_longer(stanovanje, cols = colnames(stanovanje)[-1], 
                             names_to = 'leto',
                             values_to =  'x')

stanovanje <- pivot_wider(stanovanje,
                            names_from = 'STATISTIČNA REGIJA',
                            values_from = 'x')


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





