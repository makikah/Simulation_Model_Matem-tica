

library(matlib)
library(limSolve)
library(MASS)
library(ggplot2)
library(deSolve)


## y_t - 0.8y_1 + 2.4y_2 = 100

n = 100
y = rep(0, n)

y[1] = 2
y[2] = -2

a0 = 1
a1 = -0.8
a2 = 2.4

coeffs = c(a0, a1, a2)
print(coeffs)
roots = polyroot(coeffs)
print(roots)

# Calcul de déterminant

delta = a1^2 - 4*a2*a0
delta
# Notre racine est complexe, nous allons avoir besoin de théorème d'Euler et de 
# Moivre pour trouver la solution homogène. 

R = Mod(roots[1])
theta = Arg(roots[1])

# yt = R^t*(A3*cos(theta*t) + A4*sin(theta*t))
# y0 = A3
# y1 = R*(A3*cos(theta) + A4*sin(theta))
# y1 = A3*R*cos(theta) + A4*R*sin(theta)

A = matrix(c(1, R*cos(theta), 0, R*sin(theta)), 2)
B = c(y[1], y[2])
X = ginv(A) %*% B

A3 = X[1]
A4 = X[2]

for (t in 3:n) {
  y[t] = R^t*(A3*cos(theta*t) + A4*sin(theta*t))
}

dseries = data.frame(t <- 1:n, y = y)
print(dseries)

ggplot(dseries, aes(x = t, y = y)) + geom_point(color = "black") +
  geom_line(color = "blue") + theme_classic()



# y_2 - 3y_1 + 2y_t = 0 ; y_0 = 2, y_1 = 3

n = 10
y = rep(0, n)

a0 = 2
a1 = -3
a2 = 1
# condition initiale
y[1] = 2
y[2] = 3

coefs = c(a0, a1, a2)
delta1 = a1^2 - 4*a2*a0

roots1 = polyroot(coefs)
roots1

if(delta1 > 0) {
  print("Raíses reais e diferentes")
  
  # y[t] = A1*lamda1^t + A2*lamda2^t
  # y0 = A1 + A2
  # y1 = A1*lamda1 + A2*lamda2
  
  lamda1 = Re(roots1[1])
  lamda2 = Re(roots1[2])
  
  A = matrix(c(1, lamda1, 1, lamda2), 2)
  B = c(y[1], y[2])
  X = ginv(A) %*% B
  X
  
  A[1] <- X[1]
  A[2] <- X[2]
  
  for (t in 3:n) {
    y[t] = A[1]*lamda1^t + A[2]*lamda2^t
  }
  
}

serie = data.frame(t = 1:n, y)
print(serie)

ggplot(serie, aes(x = t, y = y)) + geom_point(color = "black") +
  geom_line(color = "Royalblue") + theme_classic()

