# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 


##Število obsojenih za kriminalna dejanja v Sloveniji glede na socio-ekonomsko stanje

V svojem projektu bom analizirala število obsojenih za kriminalna dejanja v Sloveniji glede na socio-ekonomsko stanje gospodinjstev. Rada bi ugotovila, če le te vplivajo na kriminal. Rast/padanje kriminala želim primerjati z spreminjanjem povprečne samoocene splošnega zadovoljstva z življenjem, stopnjo tveganja revščine in pa s stanovanjskimi razmerami ljudi.
Podatke, ki bom vključila v projektno naloga sem našla na stani Statističnega urada republike Slovenije in Eurostatu.

Podatke bom uredila v tabele:

Tabela 1: Kazniva dejanja glede na kategorije
* Kategorije
* Leto
* Število obsojenih [prvotni podatki na 100 000 prebivalcev]

Tabela 2: Obsojeni glede na statistične regije
* Statistične regije
* Leto
* Število obsojenih [prvotni podatek na 1000 prebivalcev]

Tabela 3: Socio-ekonomske razmere prebivalcev
* Statistične regije
* Leto
* Povprečna samoocena splošnega zadovoljstva z življenjem
* Stopnja tveganja revščine [odstotek oseb, ki živijo v gospodinjstvih z ekvivalentnim razpoložljivim dohodkom, nižjim od praga tveganja revščine]
* Delež ljudi, ki stanuje v stanovanju, ki je »v slabem stanju« 


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
