#' Returns and/or plots relative precision of different confidence levels from jags output.
#' 
#' @param jags.out A jags output object from R2jags
#' @param plot Whether or not to produce a plot of precision levels.  Defaults to TRUE.
#' @param conf A vector of confidence levels to report.  Defaults to c(.5,.9,.95)
#' @param legend Whether or not to produce a legend, if plot==T.
#' @param return.output Whether or not to produce an output table.  Defaults to FALSE.
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
#'y <- 5 + x + epsilon
#'
#'cat('model {
#'  for (i in 1:N){
#'    y[i] ~ dnorm(y.hat[i], tau)
#'    y.hat[i] <- a + b * x[i]
#'  }
#'  a ~ dnorm(0, .0001)
#'	b ~ dnorm(0, .0001)
#'	tau <- pow(sigma, -2)
#'	sigma ~ dunif(0, 100)
#'}', file="test.jags")
#'
#'test.data <- list(x=x,y=y,N=N)
#'test.jags.out <- jags(model.file="test.jags", data=test.data, 
#'      parameters.to.save=c("a","b","tau","sigma"), n.chains=3, n.iter=1000)
#'
#' # ------------- calling jags.prec() ------------ #
#' jags.names(test.jags.out)
#' jags.prec(test.jags.out,plot=T,return.output=T)
#' jags.prec(test.jags.out,plot=T,return.output=F,conf=c(.5,.9),which.param=c(1:2,4:5))
#' }

#' @export
jags.prec <- function(jags.out,plot=T,conf=c(.5,.9,.95),legend=T,return.output=F,which.param=NULL) {
  # returns and/or plots relative precision of different confidence levels from jags output
  # plot, legend, and return.output are true or false as needed
  # conf is a vector of the desired confidence levels
  
  ar <- jags.out$BUGSoutput$sims.array
  if(is.null(which.param)) which.param <- 1:dim(ar)[3]
  alpha <- 1-conf
  param <- dimnames(ar)[[3]][which.param]
  cv <- 0
  precs <- matrix(NA,nrow=length(param),ncol=2*length(alpha))
  
  mat.i <- 1
  for(i in which.param) {    
    cv[mat.i] <- sd(ar[,,i])/mean(ar[,,i])
    for(j in 1:length(alpha)){
      precs[mat.i,((2*j)-1)] <- (-mean(ar[,,i])+quantile(ar[,,i],(alpha[j]/2)))/mean(ar[,,i])
      precs[mat.i,(2*j)] <- (-mean(ar[,,i])+quantile(ar[,,i],(1-(alpha[j]/2))))/mean(ar[,,i])
    }
    mat.i <- mat.i+1
  } 
  #out <- data.frame(param,cv,lo50,hi50,lo90,hi90,lo95,hi95)
  if(plot==T){
    min<-min(precs)
    max<-max(precs)
    plot(c(0,length(param)),c(min,max),col="white",las=1,xlab="",ylab="relative dist from mean",xaxt='n')
    axis(side=1,at=1:length(param),labels=param)
    for(i in 1:length(param)) {
      for(j in 1:length(alpha)) {
        lines(rep(i,2),c(precs[i,((2*j)-1)],precs[i,(2*j)]),lwd=2*(length(alpha)-j+1),lend=2)
      }
    }
    abline(h=seq(-1,1,.1),lty=3)
    if(legend==T) {
      legend(par("usr")[1],par("usr")[4],legend=conf,lwd=2*(length(alpha):1))
    }
    #par(las=0)
  }
  #output <- data.frame(precs)
  #row.names(output) <- param
  #for(j in 1:length(alpha)){
  #  names(output)[(2*j-1)]<-cat("lo",conf[j])
  #  names(output)[(2*j)]<-cat("hi",conf[j])
  #}
  if(return.output==T) {
    output <- data.frame(precs)
    return(output)
  }
}

#jags.prec(mixl_logitd_2.jags.out,plot=T,return.output=T,which.param=c(2,4:7)
#jags.prec(mixl_logitd_2.jags.out,conf=c(.5,.9),which.param=1:7)