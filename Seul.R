
tailles <- c(167, 192, 173, 174, 172, 167, 171, 185, 163, 170)
print(tailles)

c(144, 168, 179, 175, 182, 188, 167, 152, 163, 145, 176, 155, 156, 164, 167, 155,
  157, 185, 155, 169, 124, 178, 182, 195, 151, 185, 159, 156, 184, 172)

print(tailles + 20)
print(tailles^2)
tailles/100

tailles <- c(167, 192, 173, 174, 172, 167, 171, 185, 163, 170)
poids <- c(86, 74, 83, 50, 78, 66, 66, 51, 50, 55)
tailles.m <- tailles/100
imc <- poids/(tailles.m^2)
imc

length(tailles)
mean(tailles)
var(tailles)
sqrt(tailles)
min(tailles)
max(tailles)
sd(tailles) #ecart-type
library(ade4)
install.packages("")

install.packages("ade4", dep = TRUE)
install.packages("questionr", dep = TRUE)

mtcars

str(mtcars)
summary(mtcars)
head(mtcars)
as.data.frame(mtcars)

class(12.5)
class(3L)
class("abc")

rep(3, 15)
rep(c("a", "b"), 4)
seq(1, 10)
seq(2, 20, by = 2)
seq(10, 0)
seq(100, 0, by = -10)

LETTERS
letters
length(letters)
month.name
month.abb
