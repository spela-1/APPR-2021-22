# 4. faza: Napredna analiza podatkov

library(ggalt)
library(dplyr)

source("uvoz/uvoz.r", encoding="UTF-8")

###################### RAZVRŠČANJE V SKUPINE ###########################

tabela3.norm <- tabela3[c(2:7)] %>% as.matrix()%>% scale()
rownames(tabela3.norm) <- tabela3$dejanje

#dendogramom

dendrogram  <- dist(tabela3.norm) %>% hclust(method = "ward.D")
#plot(dendrogram, hang=-0.5, cex=0.01)



######### v koliko skupin je treba razdeliti?

hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }

  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}


# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr:: select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

r = hc.kolena(dendrogram)

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}


kolena <- diagram.kolena(r)


### dendogram razrežemo na 5 delov
plot(dendrogram, hang=-0.5, cex=0.01)
rect.hclust(dendrogram,k=5,border="red")
p <- cutree(dendrogram, k=5)



##########################   NAPOVEDNI MODEL   ###########################  

library(ggplot2)
library(GGally)
library(dplyr)

ggpairs(tabela_r %>% dplyr::select(obsojeni, zadovoljstvo, revscina, stanovanje))
#upoštevala bom vse tri parametre

# kasnjene ugotovila da je bolje če upoštevam tudi leta in regije

podatki.n <- tabela_r


#  prečno preverjanje:

napaka <- function(k, formula){
  
  set.seed(1)
  n <- nrow(podatki.n)
  
  # v je vektor premešanih števim od 1 do n:
  v <-sample(1:n)
  #razrez vzame vektor 1:n in ga razreže v k delov (1:n elementom da cifre od 1 do k)
  razrez <- cut(1:n, k, labels=FALSE)
  # vektor v razbije glede na razrez
  razbitje <- split(v, razrez)
  #naredi n dolg vektor elementov 0
  pp.napovedi <- rep(0, n)
  
  for (i in (1:k)){
    #ucni podatki:
    ucenje <- podatki.n[-razbitje[[i]],]
    #testni podatki:
    test <- podatki.n[razbitje[[i]],]
    model <- lm(data=ucenje, formula=formula)
    #napovemo za testne podatke
    napovedi<- predict(model, newdata=test)
    pp.napovedi[razbitje[[i]]] <- napovedi
  }

    napaka = mean((pp.napovedi - podatki.n$obsojeni)^2)
    
    return (napaka)

}

napaka.ln <- napaka(3, obsojeni ~ .)

# puskusimo še z naključnimi gozdovi
  
library(ranger)

napaka.ng <- function(k, formula){
  
  set.seed(1)
  n <- nrow(podatki.n)
  
  v <-sample(1:n)
  razrez <- cut(1:n, k, labels=FALSE)
  razbitje <- split(v, razrez)
  pp.napovedi <- rep(0, n)
  
  for (i in (1:k)){
    ucenje <- podatki.n[-razbitje[[i]],]
    test <- podatki.n[razbitje[[i]],]
    model <- ranger(formula, data=ucenje)
    napovedi<- predict(model, test)$predictions
    pp.napovedi[razbitje[[i]]] <- napovedi
  }
  
  napaka = mean((pp.napovedi - podatki.n$obsojeni)^2)
  
  return (napaka)
  
}

napaka.ng <- napaka.ng(3, obsojeni ~ .)


####### MOČ POSAMEZNE SPREMENLJIVKE

library(iml)

# podatki ki so uporabljeni zastrojno učenej zgoraj: podatki.n 


model <- lm(formula=obsojeni~., data=podatki.n)

X = podatki.n %>% dplyr:: select(!obsojeni)

pfun = function(model, newdata) {
  predict(model,newdata = newdata)
}

lm.pred = Predictor$new(model= model, data = X, y = podatki.n$obsojeni, predict.function = pfun)

lm.moci = FeatureImp$new(lm.pred, loss = "mse")

moc <- plot(lm.moci)

