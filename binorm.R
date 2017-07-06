rm(list = ls())
library(mvtnorm)
library(MASS)
library(coda)
source("http://blue.for.msu.edu/WISC17/examples/binorm-util.R")
set.seed(2)

########################################################
## Simulate some data from a bivariate normal distribution
########################################################
mu.1 <- 5
mu.2 <- 5
var.1 <- 100
var.2 <- 5
rho <- 0.75
cov.12 <- rho * sqrt(var.1) * sqrt(var.2)

mu <- c(mu.1, mu.2)
sigma <- matrix(c(var.1, cov.12, cov.12, var.2), 2, 2)

n <- 100
y <- mvrnorm(n, mu, sigma)

## y_i ~ MVN(mu, sigma), where mu = (mu_1, \mu_2)'
## and sigma is the 2x2 covariance matrix, for i=1,2,..,n.

## Here we will assume sigma is know and we want to sample from mu. 
## We could of course also sample estimate sigma,
## but for simplicity we only concentrate on mu.

########################################################
## Call the Metropolis-Hastings algorithm 
## (note, more efficient sampling 
## algorithms are available for this model)
########################################################

## The Metropolis-Hastings (MH) algorithm also
## constructs a Markov Chain, but does not
## necessarily care about full conditionals.
## So, it's more general and extremely powerful, 
## and often easier to code.

## Priors
n.samples <- 2000

theta.tuning <- c(0.2, 0.2)

## Run four chains with different starting values
theta.starting <- c(0, 0)
run.1 <- metrop(theta.starting, theta.tuning, n.samples)

theta.starting <- c(10, 0)
run.2 <- metrop(theta.starting, theta.tuning, n.samples)

theta.starting <- c(10, 10)
run.3 <- metrop(theta.starting, theta.tuning, n.samples)

theta.starting <- c(0, 10)
run.4 <- metrop(theta.starting, theta.tuning, n.samples)

########################################################
## Posterior summary
########################################################
## Plot the chains in parameter space
plot(run.1$samples, typ="l", 
     xlim = c(-2, 12), ylim = c(-2, 12),
     xlab = "mu.1", ylab = "mu.2", axes = FALSE)
points(run.2$samples, typ = "l", col = "blue")
points(run.3$samples, typ = "l", col = "red")
points(run.4$samples, typ = "l", col = "green")
axis(1); axis(2)

## Now using the coda package utilities for plotting MCMC samples
samples <- mcmc.list(mcmc(run.1$samples), 
                     mcmc(run.2$samples),
                     mcmc(run.3$samples), 
                     mcmc(run.4$samples))

plot(samples)
summary(samples)
