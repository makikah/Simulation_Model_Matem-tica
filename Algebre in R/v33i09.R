###################################################
### preliminaries
###################################################
par(mfrow=c(1,1))
library("deSolve")
options(prompt = "  ")
options(width=70)


###################################################
### 0-D Prey-Consumer model
###################################################
## 1) model function
LVmod0D <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    IngestC <- rI * P * C
    GrowthP <- rG * P * (1 - P/K)
    MortC   <- rM * C

    dP    <- GrowthP - IngestC
    dC    <- IngestC * AE - MortC

    return(list(c(dP, dC)))
  })
}

## 2) parameters, start values, times, simulation
pars    <- c(rI = 0.2,    # /day, rate of ingestion
             rG = 1.0,    # /day, growth rate of prey
             rM = 0.2 ,   # /day, mortality rate of consumer
             AE = 0.5,    # -, assimilation efficiency
             K  = 10)     # mmol/m3, carrying capacity

yini    <- c(P = 1, C = 2)
times   <- seq(0, 200, by = 1)
print(system.time(
  out     <- ode(func = LVmod0D, y = yini,
                 parms = pars, times = times)))
head(out, n=3)

## 3) plot simulation results
matplot(out[,"time"], out[,2:3], type = "l", xlab = "time", ylab = "Conc",
        main = "Lotka-Volterra", lwd = 2)
legend("topright", c("prey", "consumer"), col = 1:2, lty = 1:2)



###################################################
### 0-D Prey-Consumer model with a root
###################################################
rootfun <- function(Time, State, Pars) {
  dstate <- unlist(LVmod0D(Time, State, Pars))
  sum(abs(dstate)) - 1e-4
}

out <- lsodar(func = LVmod0D, y = yini, parms = pars,
              times = times, rootfun = rootfun)
tail(out, n = 2)


###################################################
### 1-D Prey-Consumer model
###################################################
LVmod1D <- function (time, state, parms, N, Da, dx) {
  with (as.list(parms), {

    P <- state[1:N]
    C <- state[-(1:N)]

    ## Dispersive fluxes; zero-gradient boundaries
    FluxP <- -Da * diff(c(P[1], P, P[N]))/dx
    FluxC <- -Da * diff(c(C[1], C, C[N]))/dx

    ## Biology: Lotka-Volterra dynamics
    IngestC  <- rI * P * C
    GrowthP  <- rG * P * (1- P/K)
    MortC    <- rM * C

    ## Rate of change = -Flux gradient + Biology
    dP   <- -diff(FluxP)/dx + GrowthP - IngestC
    dC   <- -diff(FluxC)/dx + IngestC * AE - MortC

    return (list(c(dP, dC)))
  })
}

R  <- 20                    # total length of surface, m
N  <- 1000                  # number of boxes
dx <- R/N                   # size of box in x-direction
Da <- 0.05                  # m2/d, dispersion coefficient

yini    <- rep(0, 2*N)
yini[500:501] <- yini[1500:1501] <- 10

times  <-seq(0, 200, by = 1)
print(system.time(
  out    <- ode.1D(y = yini, times = times, func = LVmod1D,
                   parms = pars, nspec = 2,
                   N = N, dx = dx, Da = Da)
))

P   <- out[,2:(N + 1)]
filled.contour(x = times, z = P, y = seq(0, R, length=N),
               color = gray.colors,
               xlab = "Time, days", ylab= "Distance, m",
               main = "Prey density")
# or try:
# image(out, col=gray.colors(100), main = c("Prey", "Consumer"))


###################################################
### 2-D Prey-Consumer model
###################################################
LVmod2D <- function (time, state, parms, N, Da, dx, dy) {
  P <- matrix(nr = N, nc = N, state[1:NN])
  C <- matrix(nr = N, nc = N, state[-(1:NN)])

  with (as.list(parms), {
    dP    <- rG * P *(1 - P/K) - rI * P *C
    dC    <- rI * P * C * AE - rM * C

    zero  <- numeric(N)

    ## Fluxes in x-direction; zero fluxes near boundaries
    FluxP <- rbind(zero, -Da * (P[-1,] - P[-N,])/dx, zero)
    FluxC <- rbind(zero, -Da * (C[-1,] - C[-N,])/dx, zero)

    dP    <- dP - (FluxP[-1,] - FluxP[-(N+1),])/dx
    dC    <- dC - (FluxC[-1,] - FluxC[-(N+1),])/dx

    ## Fluxes in y-direction
    FluxP <- cbind(zero, -Da * (P[,-1] - P[,-N])/dy, zero)
    FluxC <- cbind(zero, -Da * (C[,-1] - C[,-N])/dy, zero)

    dP    <- dP - (FluxP[,-1] - FluxP[,-(N+1)])/dy
    dC    <- dC - (FluxC[,-1] - FluxC[,-(N+1)])/dy

    return(list(c(as.vector(dP), as.vector(dC))))
  })
}

R  <- 20                      # total length of surface, m
N  <- 50                      # number of boxes
dx <- R/N                     # size of box in x-direction
dy <- R/N                     # size of box in y-direction
Da <- 0.05                    # m2/d, dispersion coefficient

NN <- N * N
yini     <- rep(0, 2 * N * N)
cc       <- c((NN/2):(NN/2 + 1) + N/2, (NN/2):(NN/2 + 1) - N/2)
yini[cc] <- yini[NN + cc] <- 10

times  <- seq(0, 200, by = 1) # output wanted at these time intervals

print(system.time(
  out <- ode.2D(y = yini, times = times, func = LVmod2D,
                parms = pars, dimens = c(N, N), N = N,
                dx = dx, dy = dy, Da = Da, ynames = FALSE, lrw = 440000)
))

# plot 2-D image
P   <- out[,2:(N + 1)]
par(mfrow=c(2,2))
par(oma=c(0,0,2,0))
xx <- seq(0, R, dx)
yy <- seq(0, R, dy)
Col <- gray.colors
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[1,-1]),  zlim=c(0,10), col=(Col(100)),
  main="initial", xlab="x", ylab="y")
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[21,-1]), zlim=c(0,10), col=(Col(100)),
 main="20 days", xlab="x", ylab="y")
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[31,-1]), zlim=c(0,10), col=(Col(100)),
  main="30 days", xlab="x", ylab="y")
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[41,-1]), zlim=c(0,10), col=(Col(100)),
  main="40 days", xlab="x", ylab="y")
mtext(side=3, outer=TRUE, cex=1.25, 
  "Lotka-Volterra Prey concentration on 2-D grid")

# or try:
# image(out, main = c("Prey", "Consumer"), ask = FALSE)

###################################################
### Differential algebraic eqns, chemistry model
###################################################
Res_DAE <- function (t, y, yprime, pars, K) {
  with (as.list(c(y, yprime, pars)), {

    ## residuals of lumped rates of changes
    res1 <- -dD - dA + prod
    res2 <- -dB + dA - r*B

    ## and the equilibrium equation
    eq   <- K*D - A*B

    return(list(c(res1, res2, eq),
                  CONC = A + B + D))
  })
}

times <- seq(0, 100, by = 2)
pars  <- c(r = 1, prod = 0.1)
K     <- 1

## Initial conc; D is in equilibrium with A,B
yini  <- c(A = 2, B = 3, D = 2 * 3/K)

## Initial rate of change
dyini <- c(dA = 0, dB = 0, dD = 0)

## DAE model solved with daspk
DAE <- daspk(y = yini, dy = dyini, times = times, res = Res_DAE,
             parms = pars, atol = 1e-10, rtol = 1e-10, K = 1)

par(mar=c(4,4,4,1)+.1)
plot(DAE, type = "l", lwd = 2)


###################################################
### 0-D Prey-Consumer model in Fortran and C
###################################################
# Compile the Fortran and C-code
if (is.loaded("initmod"))
  dyn.unload(paste("LVmod0D",.Platform$dynlib.ext,sep=""))
system("R CMD SHLIB LVmod0D.f")
system("R CMD SHLIB LVmod0D.c")


# load the .dll or .o file
dyn.load(paste("LVmod0D", .Platform$dynlib.ext, sep = ""))

# solve the model
pars  <- c(rI = 0.2, rG = 1.0, rM = 0.2, AE = 0.5, K = 10)
yini  <- c(P = 1, C = 2)
times <- seq(0, 200, by = 1)

print(system.time(
  out  <-  ode(func = "derivs", y = yini, parms = pars, times = times,
    dllname = "LVmod0D", initfunc = "initparms", nout = 1,
    outnames = c("total"))
))

dyn.unload(paste("LVmod0D", .Platform$dynlib.ext, sep = ""))


###################################################
### Figure 3
###################################################

## figure
P   <- out[,2:(N + 1)]
par(mfrow=c(2,2))
par(oma=c(0,0,2,0))
xx <- seq(0, R, dx)
yy <- seq(0, R, dy)
Col <- gray.colors
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[1,-1]),  zlim=c(0,10), col=(Col(100)),main="initial",xlab="x",ylab="y")
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[21,-1]), zlim=c(0,10), col=(Col(100)),main="20 days",xlab="x",ylab="y")
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[31,-1]), zlim=c(0,10), col=(Col(100)),main="30 days",xlab="x",ylab="y")
image(x=xx, y=yy, z=matrix(nr=N,nc=N,out[41,-1]), zlim=c(0,10), col=(Col(100)),main="40 days",xlab="x",ylab="y")
mtext(side=3,outer=TRUE,cex=1.25,"Lotka-Volterra Prey concentration on 2-D grid")

## legend

opar <- par(las=1, mar=c(4,4,1,1))
par(cex=3.5)
image(matrix(nr=1,nc=100,seq(0,10,length=100)),
  x=c(0,1), y=seq(0,10,length=100), zlim=c(0,10),
  col=(Col(100)),main="",xlab="",ylab="",
  axes = FALSE)
abline(h=0:10)
mtext("Prey concentration", side=2, line=2.1, las=0, cex=3.5)
axis(2)
