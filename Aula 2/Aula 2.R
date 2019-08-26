v1 = c(5, 8, 9, 6.25, 7, 7)
v2 = c(7, 5, 10, 3, 3, 4)
print(v1 + v2)

## Matrice A
A = matrix(seq(1:6), nrow = 3)
A

##Dimensão de matrice
dim(A)
nrow(A)
ncol(A)
det(A)

set.seed(1)

v = round(10*rnorm(10))
v

B = matrix(v, ncol = 3)
B

B = matrix(v, ncol = 3, byrow = TRUE)
B

# Matrix nula
matrix(0, nrow = 2, ncol = 2)

# matriz unitária
matrix(1, nrow = 2, ncol = 2)

# Matriz diagonal
diag(c(1, 2, 3))

# Matriz identidade
diag(1, 3)

# Diagonal principal
diag(B)

A = matrix(c(1, 3, 2, 2, 8, 9))
B = matrix(1:6, ncol = 2)
C = matrix(c(5, 8, 4, 2), ncol = 2)

A + B
A - B

A %*% C ## multiplicador matricial
2*A

A[1, 2]
A[,2]

library("matbib")
install.packages("matlib")

A = matrix(c(1, 2, 3, -1, 2, 1), 3, 2)
B = c(2, 1, 6)

shoEqn(A, B)

A = matrix(c(6, 2, 3, 2, 4, 2, 1, 1, 8), 3, 3)
A
B = c(7, 7, 13)

C = rbind(A, B)

Ahat = rbind(A, B)
Ahat

## Cáculo do posto de matriz
posto_A = R(A)
P_A = qr(A)$rank
P_A

library(limSolve)

A = matrix(c(2, 4, 1, 2), ncol = 2)
B = matrix(c(5, 10))


sols_possiveis = xsample(E = A, F = B, iter = 500)$X
plot(sols_possiveis[, 1])

q()
