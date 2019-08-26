A = matrix(c(6, 2, 3, 2, 4, 2, 1, 1, 8), 3, 3)

### if
if(det(A)!= 0){A_inv <- inv(A)} else {print("Matriz singular")}   ## end if

### for
x = 2
print( x^(1/3) )
x = 3
print( x^(1/3) )
x = 4
print( x^(1/3) )
x = 5
print( x^(1/3) )
x = 6
print( x^(1/3) )

## Usando comando for 
for (x in 1:5) {
  print(x^(1/3))
}

## Ou 
n = length(x)
raiz_cubica = rep(0, n)

x <- c(1, 2, 3, 4, 5 )
for (i in 1:5) {
  print(x[i]^(1/3))
  raiz_cubica[i] = x[i]^(1/3)
}


A <- matrix( c(1, 5, 0, 0, 4, 2, 0, 0, 2) , nrow = 3, ncol = 3)
print(A)
det(A)
diag(A)
prod(diag(A))

A <- matrix(c(6,2,3, 2, 4, 2, 1, 1, 8), 3, 3)
print(A)

det(A)
det(t(A))

F = A
F[1,] = rep(0, 3)
det(F)
t(F)

## Teorema de Binet
nlin = 10
vmax = 2
vmin = -vmax

v1 <- round( 2*vmax*runif(nlin^2)-vmin )
v2 <- round( 2*vmax*runif(nlin^2)-vmin )
A <- matrix(v1, nrow=nlin)
B <- matrix(v2, nrow=nlin)

det(A)
det(B)

C = A %*% B
det(C)

## teorema de Binet
det(A)*det(B)

### Autovalores e Autovetores
A <- matrix( c(1,-1,1,0,0,1,2,1,2) , 3, 3)

autovv <- eigen(A)
autovalores = autovv$values
autovalores

autovetores = autovv$vectors
autovetores

## Veja que os autovetores fornecidos são vetores unitários
sqrt(crossprod(autovetores[,1]))

zapsmall(autovetores, digits = 1)

## Outra forma de achar as raizes do polinomio caracteristico
## usar o comando charpoly para n < 100

library(pracma)
install.packages("pracma")

