### R code from vignette source 
### '/media/andy/8dc68dbd-7a34-4fcd-86fd-d8dbe36fb1c5/WISC17/exercises/exercise-jags-1/ex-1.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: libs
###################################################
options(width = 65) 

library(coda)
library(rjags)

<<<<<<< HEAD
=======

>>>>>>> a18ff2883d752993ba160c7f21d77609efa7b68f
###################################################
### code chunk number 2: ex-1.Rnw:226-233
###################################################
set.seed(1)

n <- 100
beta.0 <- 0
sigma.sq <- 5

y <- rnorm(n, beta.0, sqrt(sigma.sq))


###################################################
### code chunk number 3: ex-1.Rnw:252-256
###################################################
data <- list("y" = y, "n" = n)
inits <- list(beta.0 = 0, sigma.sq = 1)

jags.m <- jags.model(file="ex-1.jag", data = data, inits = inits, n.chains = 3, n.adapt = 100)


###################################################
### code chunk number 4: samps
###################################################
params <- c("beta.0", "sigma.sq")

samps <- coda.samples(jags.m, params, n.iter=2000)

plot(samps)


###################################################
### code chunk number 5: samps-table
###################################################
burn.in <- 1000 
summary(window(samps, start=burn.in))

round(summary(window(samps, start=burn.in))$quantiles[,c(3,1,5)],2)


###################################################
### code chunk number 6: diag
###################################################
gelman.diag(samps)
gelman.plot(samps)


###################################################
### code chunk number 7: ex-1.Rnw:301-315
###################################################
n.0 <- 25

y[1:n.0] <- NA

data <- list("y" = y, "n" = n)
inits <- list(beta.0 = 0, sigma.sq = 1)

jags.m <- jags.model(file="ex-1.jag", data = data, inits = inits, n.chains = 3, n.adapt = 100)

params <- c("beta.0", "sigma.sq", "y")

samps <- coda.samples(jags.m, params, n.iter=2000)

round(summary(window(samps[,paste("y[",1:25,"]",sep="")], start=burn.in))$quantiles[,c(3,1,5)],2)

