# Ordinary differential equations describing the abalone WS-RLO system

rm(list=ls(all=TRUE)) #clears workspace

## Install these packages if you haven't already
# install.packages("Rtools")
# install.packages("deSolve")

## Load deSolve package
library(deSolve)

## Create a SIP function
sip <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dE2 <- alpha*E1 - phi*E2 - alpha*E2
    dE3 <- alpha*E2 - phi*E3 - alpha*E3
    dE4 <- alpha*E3 - phi*E4 - alpha*E4
    dE5 <- alpha*E4 - phi*E5 - alpha*E5
    dE6 <- alpha*E5 - phi*E6 - alpha*E6
    
    return(list(c(dE2, dE3, dE4, dE5, dE6)))
  })
}

### Set parameters
init       <- c(E2 = 5000000, E3 = 2500000, E4 = 1250000, E5 = 600000, E6 = 300000)
## b: reproduction; beta: transmission; mu: natural mortality; r: WS mortality; c: parasite shedding; gamma: parasite loss
parameters <- c(E1 = 11000000, alpha = 0.99, phi = 0.5)
## Time frame
times      <- seq(0, 10, by = 0.0027)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sip, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
plot(times,out$E6)
