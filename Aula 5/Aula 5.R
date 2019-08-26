## Aula 5 -- Equações em diferenças--Dataframe e Funções
gc()

n = 100

tamanho = round(5 + 995*runif(n))
va = 300000*runif(n)
inova = round(runif(n))

head(inova)

firmas = data.frame(tamanho, va, inova)

head(firmas)
View(firmas)
summary(firmas)

prod = firmas$va/firmas$tamanho

frimas = firmas %>% mutate(prod = va/tamanho)
head(frimas)

frimas_inovadores = frimas %>% filter(inova==1)

descriptivas = frimas %>% filter(inova == 0) %>% summarise(media_va = mean(va), 
                                                           media_prod = mean(prod),
                                                           n = n())

descriptivas2 = frimas %>% group_by(inova) %>% summarise(media_va = mean(va), 
                                                         media_prod = mean(prod), 
                                                         n = n()) %>%
ungroup()

descriptivas2

med.geom = function(x, y){
  resultado = (x*y)^(1/2)
  return(resultado)
}

med.geom(10, 3)

z = 3
w = 4

med.geom(z, w)
med.geom(c(1, 2), c(2,4))

# Funções e as equações em diferença

tmax = 20
y0 = 2
a0 = 3/4

eq.ordem1 = function(tmax, y0, a0) {
  
}

ggplot(firmas, aes(x = tamanho, y = prod, color = inova)) + theme_light() +
  geom_point()

ggplot(firmas, aes(x = tamanho, y = prod, color = inova)) + 
  #geom_point() + 
  geom_boxplot(notch = TRUE) + theme_light()
