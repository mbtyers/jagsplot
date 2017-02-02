#' Trace plots of selected parameters from an R2jags object.
#' 
#' @param jags.out A jags output object from R2jags
#' @param which.param A vector of the parameter indices we're interested in plotting.  Calling jags.names() first can help with this.
#' @author Matt Tyers
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics hist
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom stats density
#' @importFrom stats quantile
#' @importFrom stats sd
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
#'		y.hat[i] <- a + b * x[i]
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
#' # ------------- calling jags.trace() ------------ #
#'par(mfrow=c(3,2))
#'jags.trace(test.jags.out)
#'
#' # say we're only interested in parameters a and b
#' jags.names(test.jags.out)
#' par(mfrow=c(2,1))
#' jags.trace(test.jags.out,which.param=1:2)
#' }

#' @export
jags.trace <- function(jags.out,which.param=NULL) {   
  ar <- jags.out$BUGSoutput$sims.array  
  if(is.null(which.param)) which.param <- 1:dim(ar)[3]
  iter <- seq((jags.out$BUGSoutput$n.burnin+jags.out$BUGSoutput$n.thin),jags.out$BUGSoutput$n.iter,by=jags.out$BUGSoutput$n.thin)
  for(i in which.param) {    
    plot(c(jags.out$BUGSoutput$n.burnin,jags.out$BUGSoutput$n.iter),c(min(ar[,,i]),max(ar[,,i])),col="white",main=dimnames(ar)[[3]][i],xlab="iteration",ylab="value")    
    for(j in 1:dim(ar)[2]) {      
      lines(iter,ar[,j,i],col=j)      
    }    
  }  
}  