\name{jagsplot-package}
\alias{jagsplot-package}
\alias{jagsplot}
\docType{package}
\title{
Plots for R2jags output
}
\description{
Diagnostic and inferential plots for R2jags output.  Possibly works with rjags output, but I don't know yet.
}
\details{
\tabular{ll}{
Package: \tab jagsplot\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-06-03\cr
License: \tab GPL-2\cr
}
Run a JAGS model using jags() in R2jags, and pass in the output.  Highlights are jags.trace(), jags.dens(), and jags.hist().  All use the same basic syntax, with argument which.param coming from a run of jags.names().
}
\author{
Matt Tyers

Maintainer: Matt Tyers <matt.tyers@alaska.gov>

}
\examples{
# ------------- example model ------------ #
library(R2jags)
N <- 1000
x <- 1:N
epsilon <- rnorm(N, 0, 1)
y <- x + epsilon

cat('model {
  for (i in 1:N){
    y[i] ~ dnorm(y.hat[i], tau)
  	y.hat[i] <- a + b * x[i]
	}
	a ~ dnorm(0, .0001)
	b ~ dnorm(0, .0001)
	tau <- pow(sigma, -2)
	sigma ~ dunif(0, 100)
}', file="test.jags")

test.data <- list(x=x,y=y,N=N)
test.jags.out <- jags(model.file="test.jags", data=test.data, 
      parameters.to.save=c("a","b","tau","sigma"), n.chains=3, n.iter=1000)

# ------------- calling jags.trace() ------------ #
par(mfrow=c(3,2))
jags.trace(test.jags.out)

# say we're only interested in parameters a and b
jags.names(test.jags.out)
par(mfrow=c(2,1))
jags.trace(test.jags.out,which.param=1:2)

# -------- jags.dens() and jags.hist() ----------- #
par(mfrow=c(3,2))
jags.dens(test.jags.out)
jags.hist(test.jags.out)
}
