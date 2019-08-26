## Équation en différence d'ordre 2

library(limSolve)
library(deSolve)

## Exercice 1

def = function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    M = sqrt(1/3*(1/2*dy^2 + 1/2*y^2))
    dz = M*z # dz/dt
    ddy = -3*M*dy - y # ddy/dt
    
    list(c(dy, ddy, dz))
  })
}

state = c(y = 1, dy = -0.1, z = 1)

times = seq(0, 100, length.out = 10001)
sol = ode(func = def, y = state, times = times, parms = NULL)

y = sol[, "y"]
dy = sol[, "dy"]
z = sol[, "z"]

M = sqrt(1/3*(1/2*dy^2 + 1/2*y^2))

plot(times, z, col = "red", ylim = c(-1, 18), type = "l")
lines(times, y, col = "blue")
lines(times, M, col = "green")
grid()

## Exercice 2

### L'équation mathématique voir Karline Soetaert, Thomas Petzoldt and R. Woodrow
### Setzer (2010), page 7. (y1 = 2, y2 = 0)
## Initial Value Problems (IVP), Ordinary Differential Equations (ODE),
## Stochastic differential equations (SDE), Initial value partial differential equations
## (PDE), Boundary value problems (BVP) of ordinary differential equations, 
## Initial value differential algebraic equations (DAE).

diff = function(t, y, mu) {
  list(c(y[2], mu * (1 - y[1]^2) * y[2] - y[1]))
}

library(deSolve)

Yiff = c(y1 = 2, y2 = 0)

tiff = ode(y = Yiff, func = diff, times = 0:3000, parms = 1000)

nontiff <- ode(y = Yiff, func = diff, times = seq(0, 30, by = 0.01),
               parms = 1)

head(tiff, n = 3)
head(nontiff, n = 3)

## Figures are generated using the S3 plot method for objects of class deSolve:

plot(tiff, type = "l", which = "y1", lwd = 2, ylab = "y",
     main = "IVP ODE, tiff")

plot(nontiff, type = "l", which = "y2", lwd =  2, ylab = "y", 
     main = "IVP ODE, nontiff")

### A boundary value ODE

## Exercice 3 (Voir page 9)

prob14 = function(x, y, xi) {
  list(c(y[2], 1/xi * (y[1] - (xi * pi^2 + 1) * cos(pi*x))
         ))
}

library(bvpSolve)

x <- seq(-1, 1, by = 0.01)

shoot <- bvpshoot(yini = c(0, NA), yend = c(0, NA), x = x, parms = 0.01,
                  func = Prob14)
twp <- bvptwp(yini = c(0, NA), yend = c(0, NA), x = x, parms = 0.0025,
              func = Prob14)
coll <- bvpcol(yini = c(0, NA), yend = c(0, NA), x = x, parms = 1e-04,
               func = Prob14)

xi <- 0.0025

analytic <- cos(pi * x) + exp((x - 1)/sqrt(xi)) + exp(-(x + 1)/sqrt(xi))

max(abs(analytic - twp[, 2]))

plot(shoot[, 1], shoot[, 2], type = "l", lwd = 2, ylim = c(-1, 1), col = "blue",
     xlab = "x", ylab = "y", main = "BVP ODE")

lines(twp[, 1], twp[, 2], col = "red", lwd = 2)

lines(coll[, 1], coll[, 2], col = "green", lwd = 2)

#legend("topright", legend = c("0.01", "0.0025", "0.0001"), 
       #col = c("blue", "red", "green"), title = expression(xi), lwd = 2)


## Differential algebraic equations
### The so called “Rober problem” describes an autocatalytic reaction (Robertson, 
### 1966) between three chemical species, y 1 , y 2 and y 3 . The problem can be
### formulated either as an ODE (Mazzia and Magherini, 2008), or as a DAE (pour 
### les équations voir page 9).

# Implemented in R

daefun <- function(t, y, dy, parms) {
  res1 <- - dy[1] - 0.04 * y[1] + 1e4 * y[2] * y[3]
  res2 <- - dy[2] + 0.04 * y[1] - 1e4 * y[2] * y[3] - 3e7 * y[2]^2
  res3 <- y[1] + y[2] + y[3] - 1
  list(c(res1, res2, res3),
       error = as.vector(y[1] + y[2] + y[3]) - 1)
}

yini <- c(y1 = 1, y2 = 0, y3 = 0)
dyini <- c(-0.04, 0.04, 0)
times <- 10 ^ seq(-6,6,0.1)

print(system.time(out <-daspk(y = yini, dy = dyini, times = times, res = daefun,
                              parms = NULL)))

# An S3 plot method can be used to plot all variables at once:
  
plot(out, ylab = "conc.", xlab = "time", type = "l", lwd = 2, log = "x")

mtext("IVP DAE", side = 3, outer = TRUE, line = -1)



## Partial differential equations

# In partial differential equations (PDE), the function has several independent variables 
# (e.g. time and depth) and contains their partial derivatives.
# Many partial differential equations can be solved by numerical approximation (finite 
# differencing) after rewriting them as a set of ODEs (see Schiesser,1991; 
# LeVeque, 2007; Hundsdorfer and Verwer, 2003).
# Functions tran.1D, tran.2D, and tran.3D from R package ReacTran (Soetaert and Meysman, 
# 2010) implement finite difference approximations of the diffusive-advective transport 
# equation which, for the 1-D case, is: page 10.


library(ReacTran)

Grid = setup.grid.1D(N = 1000, L = 10)

pde1D <-function(t, C, parms) {
  tran <- tran.1D(C = C, D = D, C.down = Cext, dx = Grid)$dC
  list(tran - Q) # return value: rate of change
}

## The Model Parameters are :

D <- 1
Q <- 1
Cext <- 20

library(rootSolve)

print(system.time(std <- steady.1D(y = runif(Grid$N), func = pde1D, 
                                   parms = NULL, nspec = 1)))

par(mfrow = c(1, 1))
plot (Grid$x.mid, std$y, type = "l", lwd = 2, main = "steady-state PDE",
      xlab = "x", ylab = "C", col = "red")

# The analytical solution compares well with the numerical approximation:

analytical <- Q/2/D*(Grid$x.mid^2 - 10^2) + Cext

max(abs(analytical - std$y))

# Next the model is run dynamically for 100 time units using deSolve function ode.1D, 
# and starting with a uniform concentration:

require(deSolve)

times <- seq(0, 100, by = 1)

system.time(out <- ode.1D(y = rep(1, Grid$N), times = times, func = pde1D,
                          parms = NULL, nspec = 1))

# Here, out is a matrix, whose 1 st column contains the output times, and the next 
# columns the values of the state variables in the different boxes; we print the
# first columns of the last three rows of this matrix:

tail(out[, 1:4], n = 3)

# We plot the result using a blue-yellow-red color scheme, and using deSolve’s S3 
# method image. Figure 6 shows that, as time proceeds, gradients develop
# from the uniform distribution, until the system almost reaches steady-state at the 
# end of the simulation

image(out, xlab = "time, days", ylab = "Distance, cm", main = "PDE", 
      add.contour = TRUE)

##################################################################################
##################################################################################

# Ordinary Differential Equations (ODE)

## First order ODE

library(deSolve)

parameters    <- c(a = -8/3, b = -10, c =  28)

initial.state <- c(X = 1, Y = 1, Z = 1)

Lorenz<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    dX <- a*X + Y*Z
    dY <- b * (Y-Z)
    dZ <- -X*Y + c*Y - Z
    
    list(c(dX, dY, dZ)) # return the rate of change
  })
}

# Then we apply the model. For that we need to know what are the timestamps used:

times <- seq(0, 100, by = 0.01)

# Finally we apply all into the ODE solver:

out <- ode(y = initial.state, times = times, func = Lorenz, 
           parms = parameters)

# Visualizing the results:

head(out)
summary(out)

par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[, "Z"], pch = ".")
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)

# Plotting in 3D:

library(plot3D)

par(mfrow = c(1, 1))
points3D(out[, "X"], out[, "Y"], out[, "Z"], pch = '.', colkey = F, 
         colvar = out[, "Y"]) # color using Y values


# zooming
out.zoom <- subset(out, select = c("X","Y","Z"), subset = Y < 8 & X > 10 & X < 30)

points3D(out.zoom[, "X"], out.zoom[, "Y"], out.zoom[, "Z"], pch = '.', 
         colkey = F, colvar = out.zoom[, "Y"]) # color using Y values

## Second order ODE

vdpol <- function (t, y, mu) {
  list(c(y[2], mu * (1 - y[1]^2) * y[2] - y[1]))
}

y.init <- c(y1 = 2, y2 = 0)

out <- ode(y = y.init, func = vdpol, times = seq(0, 30, 0.01), parms = 1)

head(out)

plot(out, xlab = "time", ylab = "-")

plot(out[, "y1"], out[, "y2"], pch = ".")

# Partial Differential Equations (PDE)

## A partial differential equation is a differential equation that contains unknown 
## multivariable functions and their partial derivatives.
## The functions to use are ode.1D, ode.2D, and ode.3D for problems in these respective 
## dimensions.

parameters = list(D = 0.3,       # diffusion rate; m^2/day
                  r = 0.01,      # net growth rate; day^-1
                  numboxes = 60, # number of boxes
                  delx = 1)      # thickness of each box; m

Aphid <- function(t, N, parameters) {
  with(parameters,{
    deltax  <- c(0.5, rep(1, numboxes - 1), 0.5)
    Flux    <- -D * diff(c(0, N, 0)) / deltax
    dN      <- -diff(Flux) / delx + N * r
    
    list(dN)
  })
}

# initial condition

N <- rep(0, times =  parameters$numboxes)
N[30:31] <- 1
state <- c(N = N) # initialise state variables

# let's run for 300 days
times <- seq(0, 300, by = 1)

out <- ode.1D(state, times, Aphid, parms = parameters, nspec = 1, 
              names = "Aphid")

head(out[,1:5])
summary(out)

image(out, method = "filled.contour", 
      grid = seq(from = 0.5, by = parameters$delx, length.out = parameters$numboxes),
      xlab = "time, days", ylab = "Distance on plant, meters",
      main = "Aphid density on a row of plants")

# Differential algebraic equations (DAE)

## A differential-algebraic equation is an equation involving an unknown function and 
## its derivatives.

f <- function(t, y, dy, parameters) {
  res1 <- dy[1] + y[1] - y[2]
  res2 <- y[2] * y[1] - t
  
  list(c(res1, res2))
}

yini  <- c(2, 0) # initial conditions
dyini <- c(1, 0)

times <- seq(0, 20, 0.1)

out <- daspk(y = yini, dy = dyini, times = times, res = f, parms = 0)

matplot(out[,1], out[,2:3], type = "l", lwd = 2, col = c("red","blue"), lty = 1, 
        main = "DAE", xlab = "time", ylab = "ys")

legend("bottomright", legend = c("y1","y2"), col = c("red","blue"), 
       lty = 1, lwd = 2)


