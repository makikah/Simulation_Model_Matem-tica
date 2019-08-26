library(dplyr)

gc()

n = 100

tamanho = round( 5 + 995*runif(n) )
va = 300000*runif(n)
inova = round(  runif(n)  )

firmas = data.frame( tamanho, va, inova  )

head(firmas)

View(firmas)

summary(firmas)

#firmas$prod = firmas$va/firmas$tamanho


firmas <- firmas %>% mutate( prod = va/tamanho  )

head(firmas)  

firmas_inovadoras <- firmas %>% filter(inova==1)

descritivas <- firmas %>% filter( inova == 0 ) %>% 
                summarise( media_va = mean(va),
                           media_prod = mean(prod),
                           n = n()) 
  
  
descritivas2 <- firmas %>% group_by(inova) %>% 
                summarise( media_va = mean(va),
                           media_prod = mean(prod),
                           n = n()) %>%
  
                ungroup()


descritivas2


med.geom <- function(x, y){
  
  resultado <- (x*y)^(1/2)
  
  return(resultado)
}


med.geom(10, 3)

z = 3
w = 4

med.geom(z,w)

med.geom( c(1,2), c(2,4) )


tmax = 20
y0 = 2
a0 = 3/4

y = rep(0, tmax)
y[1] = y0

for (t in 2:tmax){
  
  y[t] = a0*y[t-1]

}

eq.ordem1 <- function(t_max, y_0, a_0){
  
  y = rep(0, t_max)
  y[1] = y_0
  
  for (t in 2:t_max){
    
    y[t] = a_0*y[t-1]
    
  }

  return( y )
} # fim eq.ordem1

tmax = 20
y0 = 2
a0 = 3/4

serie1 <- eq.ordem1(tmax, y0, a0)
serie2 <- eq.ordem1(tmax, y0, 0.1)
serie3 <- eq.ordem1(tmax, 5, 1.25)

print(eq.ordem1)

  
  
  
  
  