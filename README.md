# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 


## Analiza kriminala v Sloveniji

V svojem projektu bom analizirala število obsojenih za kriminalna dejanja v Sloveniji glede na socialno stanje gospodinjstev. Rada bi ugotovila, če le te vplivajo na kriminal. Rast/padanje kriminala želim primerjati z spreminjanjem povprečne samoocene splošnega zadovoljstva z življenjem, stopnjo tveganja revščine in pa s stanovanjskimi razmerami ljudi.
Podatke, ki bom vključila v projektno naloga sem našla na stani Statističnega urada republike Slovenije in Eurostatu.

Podatke bom uredila v tabele:
  

Tabela 1: Kazniva dejanja glede na kategorije
* vrsta kaznivega dejanja
* spol
* starostna skupina
* leto
* Število obsojenih

Tabela 2: Obsojeni glede na statistične regije
* Statistične regije
* Leto
* Število obsojenih na 1000 prebivalcev
* Povprečna samoocena zadovoljstva
* Stopnja tveganja revščine
* Stanovanjske razmere


Tabela 3: Obsojeni glede na občine
* občina
* leto (2008-2020)
* število obsojenih na 1000 prebivalcev
* povprečna plača


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
