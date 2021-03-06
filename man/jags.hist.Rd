% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jagshist.R
\name{jags.hist}
\alias{jags.hist}
\title{Histograms of selected parameters from an R2jags object.}
\usage{
jags.hist(jags.out, which.param = NULL)
}
\arguments{
\item{jags.out}{A jags output object from R2jags}

\item{which.param}{A vector of the parameter indices we're interested in plotting.  Calling jags.names() first can help with this.}
}
\description{
Histograms of selected parameters from an R2jags object.
}
\examples{
# ------------ a sample R2jags model ----------- #

\dontrun{
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

# ------------- calling jags.hist() ------------ #
par(mfrow=c(3,2))
jags.hist(test.jags.out)

# say we're only interested in parameters a and b
jags.names(test.jags.out)
par(mfrow=c(2,1))
jags.hist(test.jags.out,which.param=1:2)
}
}
\author{
Matt Tyers
}

