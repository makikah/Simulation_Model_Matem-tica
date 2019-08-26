## Equações em diferenças no R

# Equações em diferença de ordem 1 - caso homogêneo

library(ggplot2)
library(tidyr)
library(dplyr)

library(plyr)
library(scales)

# diferentes coeficientes para a equacao linear
a0 <- c(2, -2, 1/3, -1/3, 1, -1)
# numero de iteracoes - periodos de tempo
iter <- 20
# vetor simulando os diferenes periodos de tempo
t <- 1:iter
# condicao inicial
y0 <- 0.1 
# produzindo seis series de tempo diferentes
y1 <- (-a0[1])^t * y0
y2 <- (-a0[2])^t * y0
y3 <- (-a0[3])^t * y0
y4 <- (-a0[4])^t * y0
y5 <- (-a0[5])^t * y0
y6 <- (-a0[6])^t * y0

# armazenando as series temporais em um dataframe
series <- data.frame(t, y1, y2, y3, y4, y5, y6)
print(series)

## Gráfico básico
ggplot(mapping = aes(x = t, y = y1), data = series) + 
  geom_line()

## Pour priviléger l'esthétique
ggplot(mapping = aes(x = t, y = y1), data = series) +
  geom_line(col = "red", size = 2) + theme_minimal()

# a forma preguicosa de visualizar varias series
ggplot(mapping = aes(x = t, y = y1), data = series) +
  geom_line() +
  geom_line(aes(y = y2), col = "blue") + 
  geom_line(aes(y = y3), col = "red") + 
  geom_line(aes(y = y4), col = "green")

# a forma mais eficiente
# gather = juntar, agregar, arrumar, arranjar, reordenar

series_gathered = series %>% gather(key = "variavel", value = "valor", -t)
head(series)
head(series_gathered)

ggplot(series_gathered, aes(x = t, y = valor, color = variavel)) + 
  geom_line(size = 1) + theme_minimal()

ggplot(series_gathered, aes(x = t, y = valor, color = variavel)) + 
  geom_line(size = 1) + theme_minimal() + ylim(-10, 10) + xlim(0, 10)

ggplot(series_gathered, aes(x = t, y = valor, color = variavel)) + 
  geom_line(size = 1) + 
  facet_wrap(~variavel, ncol = 2) +
  theme_minimal() 

ggplot(series_gathered, aes(x = t, y = valor, color = variavel)) + 
  geom_line(size = 1) + 
  facet_wrap(~variavel, ncol = 2, scales = "free") +
  theme_minimal() 

## Para salvar o gráfico
png(filename = "Gráfico_series.png")
grafico = ggplot(series_gathered, aes(x = t, y = valor, color = variavel)) +
  geom_line(size = 1) + 
  facet_wrap(~variavel, ncol = 2, scales = "free") + 
  theme_minimal()
grafico
dev.off()

## Equações em diferença de ordem 1 - caso não homogêneo
t = 1:20
y = rep(0, length(t))
yh = rep(0, length(t))

y[1] = 1
a0 = -1.1
b = 2

for (time in 2:max(t)) {
  y[time] = a0*y[time-1] + b
  yh[time] = a0*y[time-1]
}

series2 = data.frame(t, y, yh)

series2.gathered = series2 %>% gather(key = "variavel", value = "valor", -t)

ggplot(series2.gathered, mapping = aes(x = t, y = valor, linetype = variavel)) +
  geom_line() +
  theme_minimal()
######################################

## Mon apprentissage

ggplot(mapping = aes(x = t, y = y1), data = series) + 
  geom_line()

ggplot(data = series, mapping = aes(x = t, y = y1)) + geom_point()

ggplot(data = series, mapping = aes(x = t, y = y1)) + geom_area()

ggplot(data = series, mapping = aes(x = t, y = y1)) + geom_point(alpha = 0.1,
                                                                 color = "red")

ggplot(data = series, mapping = aes(x = t, y = y1)) + 
  geom_boxplot()

ggplot(data = series, mapping = aes(x = t, y = y1)) + 
  geom_boxplot(alpha = 0) + geom_jitter(alpha = 0.3, color = "tomato")

### Data Diamonds

diamonds

qplot(diamonds$carat, diamonds$price)
qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds,
      colour=clarity)
qplot(carat, price, data = diamonds,
      geom  = c("point", "smooth"), method = lm)
qplot(carat, data = diamonds,
      geom = "histogram")
qplot(carat, data = diamonds,
      geom = "histogram", binwidth = 100)
