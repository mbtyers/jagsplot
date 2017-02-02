#' Extracts parameter names from an R2jags object.
#' 
#' @param jags.out A jags output object from R2jags
#' @param max The maximum number of parameter names to print (defaults to 100).
#' @author Matt Tyers
#' @examples 
#' # ------------ a sample R2jags model ----------- #
#' 
#' \dontrun{
#' library(R2jags)
#'N <- 1000
#'x <- 1:N
#'epsilon <- rnorm(N, 0, 1)
#'y <- x + epsilon
#'
#'cat('model {
#'  for (i in 1:N){
#'    y[i] ~ dnorm(y.hat[i], tau)
#'    y.hat[i] <- a + b * x[i]
#'  }
#'	a ~ dnorm(0, .0001)
#'	b ~ dnorm(0, .0001)
#'	tau <- pow(sigma, -2)
#'	sigma ~ dunif(0, 100)
#'}', file="test.jags")
#'
#'test.data <- list(x=x,y=y,N=N)
#'test.jags.out <- jags(model.file="test.jags", data=test.data, 
#'      parameters.to.save=c("a","b","tau","sigma"), n.chains=3, n.iter=1000)
#'
#' # ------------- calling jags.names() ------------ #
#' jags.names(test.jags.out)
#' par(mfrow=c(2,1))
#' jags.dens(test.jags.out,which.param=1:2)
#' }

#' @export
jags.names <- function(jags.out,max=100) {
  # extracts parameter names from bugs output
  
  ar <- jags.out$BUGSoutput$sims.array
  if(length(dimnames(ar)[[3]])<max) max<-length(dimnames(ar)[[3]])
  cat("Input parameters:",'\n')
  print(jags.out$parameters.to.save)
  #print("")
  cat('\n',"Output parameters (first",max,"of",length(dimnames(ar)[[3]]),"):",'\n')
  print(dimnames(ar)[[3]][1:max])
}