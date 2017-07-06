## Assume the data is distributed y_i ~ N(theta, sigma.sq) for i=1,2,...,n. 
## We can estimate theta given a sample of size n and prior on theta ~ N(mu, tau.sq). 
## For this illustration, we will assume sigma.sq is known.

norm.prior.data <- function(n, theta, sigma.sq = 1, mu, tau.sq){
  
  y <- rnorm(n, theta, sqrt(sigma.sq))
  
  post.var <- 1 / (1 / tau.sq + n / sigma.sq)
  post.mean <- (mu / tau.sq + sum(y) / sigma.sq) / 
    (1 / tau.sq + n / sigma.sq)
  
  L.var <- sigma.sq / n
  L.mean <- mean(y)
  
  ## get plot extents
  x.min <- min(L.mean - 3 * sqrt(L.var), 
               mu - 3 * sqrt(tau.sq), 
               post.mean - 3 * sqrt(post.var))
  x.max <- max(L.mean + 3 * sqrt(L.var), 
               mu + 3 * sqrt(tau.sq), 
               post.mean + 3 * sqrt(post.var))
  x <- seq(x.min, x.max, length.out = 1000)
  
  y.max <- max(dnorm(x, L.mean, sqrt(L.var)),
               dnorm(x, post.mean, sqrt(post.var)),
               dnorm(x, mu, sqrt(tau.sq)))
  
  curve(dnorm(x, L.mean, sqrt(L.var)), 
        from = x.min, to = x.max, 
        xlim = c(x.min, x.max), ylim = c(0, y.max),
        ylab = "Density", xlab = expression(mu), 
        lwd = 2, axes = FALSE)
  curve(dnorm(x, mu, sqrt(tau.sq)), 
        col = "red", lwd = 2, add = TRUE)
  curve(dnorm(x, post.mean, sqrt(post.var)), 
        col = "blue", lwd = 2, add = TRUE)
  abline(v = theta, col = "green")
  legend("topleft", c("Likelihood", "Prior", "Posterior", "True mean"), 
         col = c("black", "red", "blue", "green"), lwd = 3, bty = "n")
  axis(1); axis(2)
}

## Think about these scenarios:

## Small data & informative priors
## Small data & non-informative priors
## Large data & informative priors
## Large data & non-informative priors

## data mean and variance
theta <- 3
sigma.sq <- 1

## small sample and informative prior
n <- 15
mu <- 0
tau.sq <- 0.1

norm.prior.data(n, theta, sigma.sq, mu, tau.sq)

## small sample and non-informative prior
mu <- 1
tau.sq <- 10

norm.prior.data(n, theta, sigma.sq, mu, tau.sq)

## large sample and informative prior
n <- 100
mu <- 0
tau.sq <- 0.1

norm.prior.data(n, theta, sigma.sq, mu, tau.sq)

## large sample and non-informative prior
mu <- 10
tau.sq <- 10

norm.prior.data(n, theta, sigma.sq, mu, tau.sq)
