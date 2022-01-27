# 4. faza: Napredna analiza podatkov

library(ggalt)


# RAZVRŠČANJE V SKUPINE


tabela3$starost <- parse_double(tabela3[["starost"]])
tabela3$spol <- (tabela3$spol=="Moški")*1
tabela3.norm <- tabela3 %>%dplyr:: select(-dejanje)


tabela3.norm <- tabela3.norm%>% 
  mutate(spol = scale(spol),
         starost = scale(starost),
         obsojeni = scale(obsojeni))


# z dendogramom

dendrogram  <- dist(tabela3.norm) %>% hclust(method = "ward.D")
plot(dendrogram, hang=-5, cex=0.1)
rect.hclust(dendrogram,k=3,border="red")
p <- cutree(dendrogram, k=3)



#razvrsti vrstice v 3 skupine :

skupine = tabela3.norm %>%   
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()




# nariše diagram:


podatki <-  tabela3.norm %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  
d = podatki %>%ggplot(
      mapping = aes(
        x = starost, y = obsojeni, color = skupina)
      ) +
    geom_point() +
    geom_label(label = tabela3$dejanje, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:7) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }

print(d)  

