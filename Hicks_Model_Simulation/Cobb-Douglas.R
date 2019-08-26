#FONCTION COBB-DOUGLAS SIMULATION

# Y = A K^alpha x L^(1-alpha)

library(mosaic)
library(ade4)
library(manipulateWidget)

plotFun(A * (L^0.7) * (K^0.3) ~ L, K = 20, A = 5, ylim = range(-5,101),
        xlim = range(-1, 21))

plotFun(A * (L^0.7) * (K^0.3) ~ L, K = 20, A = 5, ylim = range(-5, 151), 
        xlim = range(-1, 21))

plotFun(A * (L^0.7) * (K^0.3) ~ L, K = 40, A = 5, ylim = range(-5, 151), 
        xlim = range(-1, 21), lty = 2, add = TRUE)

plotFun(A * (L^0.7) * (K^0.3) ~ L & K, A = 5, filled = FALSE, 
        xlim = range(0, 21), ylim = range(0, 100))

plotFun(A * (L^0.7) * (K^0.3) ~ L & K, A = 5, filled = FALSE,
        xlim = range(0, 21), ylim = range(0, 100), surface = TRUE)




