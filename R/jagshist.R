#' Histograms of selected parameters from an R2jags object.
#' 
#' @param jags.out A jags output object from R2jags
#' @param which.param A vector of the parameter indices we're interested in plotting.  Calling jags.names() first can help with this.
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
#'  	y.hat[i] <- a + b * x[i]
#'	}
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
#' # ------------- calling jags.hist() ------------ #
#'par(mfrow=c(3,2))
#'jags.hist(test.jags.out)
#'
#' # say we're only interested in parameters a and b
#' jags.names(test.jags.out)
#' par(mfrow=c(2,1))
#' jags.hist(test.jags.out,which.param=1:2)
#' }

#' @export
jags.hist <- function(jags.out,which.param=NULL) {
  ar <- jags.out$BUGSoutput$sims.array   
  if(is.null(which.param)) which.param <- 1:dim(ar)[3]
  for(i in which.param) {    
    hist(ar[,,i],main=dimnames(ar)[[3]][i],xlab="")    
  }  
}