2 + 5 
2^(1/4)
2^3

a = 2
b = 5

print(a + b)
print(a - b)

AB = "Piracicaba"
AB

X1 = -3

abs(X1)
log(-X1)
exp(X1)
print(X1)

x2 = c(3, 2, 1, -6, -2, 0)
classes = c("A", "B", "C")
exp(x2)

round(x2, 2)
round(x2, 4)

id = c(1, 2, 3, 4)

municipio = c(2345, 4333, 9543, 2001)

nome = c("Maria", "Joao", "Pedro", "Mateo")

sexo = c("F", "M", "M", "M")

renda_med = c(1.5, 2.3, 4, 2.9)

base_dados = data.frame( id, municipio, nome, sexo, renda_med)
base_dados

names(base_dados)
str(base_dados)

base_dados$nome
base_dados[, "nome"]






plot(x = TSBRA$YEAR, y = TSBRA$PTF, 
     xlim = c(1890-10, 2013+10),
     type = "l",
     main = "Produtividade Total de Fatores-BRA",
     xlab = "anos",
     ylab = "ylab")
grid()
