#' Line plots of selected parameters from multiple models.
#' 
#' @param postlist A single, or list of, R2jags output objects or mcmc.list objects.  The legend will be created from the names of the list objects.
#' @param years1 An optional vector of years to use for plotting labels, possibly useful for random effects
#' @param years2 An optional vector of years to use for plotting labels, possibly useful for random effects
#' @param pnames A character list of which parameters to plot
#' @author Matt Tyers
#' @examples 
#' # ------------ two competing R2jags models ----------- #
#' 
#' \dontrun{
#' library(R2jags)
#'N <- 1000
#'x <- 1:N
#'yreff <- rnorm(10,0,10)
#'yr <- rep(1:20, each=50)
#'epsilon <- rnorm(N, 0, 1)
#'y <- x + yreff[yr] + epsilon
#'
#'cat('model {
#'  for (i in 1:N){
#'    y[i] ~ dnorm(mu[i], tau)
#'    mu[i] <- b0 + b1*x[i] + a.yr[yr[i]]
#'	}
#'	b0 ~ dnorm(0, .0001)
#'	b1 ~ dnorm(0, .0001)
#'	tau <- pow(sigma, -2)
#'	sigma ~ dunif(0, 100)
#'	for(j in 1:Nyr) {
#'	  a.yr[j] ~ dnorm(0, tau.a)
#'	}
#'	tau.a <- pow(sigma.a, -2)
#'	sigma.a ~ dunif(0, 100)
#'}', file="test.jags")
#'
#'test.data <- list(x=x,y=y,N=N,yr=yr,Nyr=max(yr))
#'test.jags.out1 <- jags(model.file="test.jags", data=test.data, 
#'      parameters.to.save=c("b0","b1","a.yr","sigma","sigma.a"), n.chains=3, n.iter=10000)
#'
#'cat('model {
#'  for (i in 1:N){
#'    y[i] ~ dnorm(mu[i], tau)
#'    mu[i] <- b0 + b1*x[i] + a.yr[yr[i]]
#'	}
#'	b0 ~ dnorm(0, .0001)
#'	b1 ~ dnorm(0, .0001)
#'	tau <- pow(sigma, -2)
#'	sigma ~ dunif(0, 100)
#'	for(j in 1:Nyr) {
#'	  a.yr[j] ~ dnorm(0, 0.0001)
#'	}
#'}', file="test.jags")
#'
#'test.data <- list(x=x,y=y,N=N,yr=yr,Nyr=max(yr))
#'test.jags.out2 <- jags(model.file="test.jags", data=test.data, 
#'      parameters.to.save=c("b0","b1","a.yr","sigma"), n.chains=3, n.iter=10000)
#'
#' # ------------- calling comparepost() ------------ #
#'par(mfrow=c(3,2))
#'comparepost(postlist=list(hierarchical=test.jags.out1,non_hier=test.jags.out2),years1=1998:2017)
#'
#' ### or just one posterior...
#'comparepost(test.jags.out1,years1=1998:2017)
#'}

#' @export
comparepost <- function(postlist, years1=NA, years2=NA, pnames=NULL) {
  if(class(postlist)=="list") {
    if(class(postlist[[1]])=="rjags") postlist <- lapply(postlist, coda::as.mcmc)
    if(class(postlist[[1]])!="mcmc.list") stop("Must be mcmc.list or rjags object")
  }
  if(class(postlist)=="rjags") postlist <- list(as.mcmc(postlist))
  if(class(postlist)=="mcmc.list") postlist <- list(postlist)
  if(class(postlist)!="list") stop("Must be mcmc.list or rjags object")
  
  post_df <- list()
  len <- length(postlist)
  if(is.null(names(postlist))) names(postlist) <- 1:len
  cols <- adjustcolor(2:(len+1), red.f=.8, blue.f=.8, green.f=.8)
  for(i in 1:len) {
    post_df[[i]] <- postlist[[i]][[1]]
    if(length(postlist[[i]])>1) {
      for(j in 2:length(postlist[[i]])) post_df[[i]] <- rbind(post_df[[i]],postlist[[i]][[j]])
    }
    post_df[[i]] <- as.data.frame(post_df[[i]])
  }
  if(is.null(pnames)) {
    thenames <- NULL
    for(i in 1:len) thenames <- c(thenames,names(post_df[[i]]))
    pname <- sapply(strsplit(thenames,split="[",fixed=T),FUN="[",1)
    pnames <- sort(unique(pname))
  }
  
  for(name in pnames) {
    dfsub <- q05 <- q25 <- q50 <- q75 <- q95 <- list()
    for(i in 1:len) {
      pnamei <- sapply(strsplit(names(post_df[[i]]),split="[",fixed=T),FUN="[",1)
      dfsub[[i]] <- as.matrix(post_df[[i]][,pnamei==name])
      q05[[i]] <- unname(apply(dfsub[[i]], 2, quantile, p=0.05))
      q25[[i]] <- unname(apply(dfsub[[i]], 2, quantile, p=0.25))
      q50[[i]] <- unname(apply(dfsub[[i]], 2, quantile, p=0.50))
      q75[[i]] <- unname(apply(dfsub[[i]], 2, quantile, p=0.75))
      q95[[i]] <- unname(apply(dfsub[[i]], 2, quantile, p=0.95))
    }
    lengths <- sapply(q05,length)
    maxl <- max(lengths)
    if(maxl==1) plot(NA,ylim=c(-.6,.2), xlim=c(min(unlist(q05)),max(unlist(q95))),main=name,ylab="",xlab="",yaxt="n")
    if(maxl>1) {
      if(all(lengths %in% c(0,length(years1)))) xvals <- years1
      if(all(lengths %in% c(0,length(years2)))) xvals <- years2
      if(!all(lengths %in% c(0,length(years1))) & !all(lengths %in% c(0,length(years2)))) xvals <- 1:maxl
      plot(NA,xlim=c(min(xvals),max(xvals)+1), ylim=c(min(unlist(q05)),max(unlist(q95))),main=name,ylab="",xlab="")
    }
    for(i in 1:len) {
      d <- (i-1)*0.6/len
      if(maxl==1 & lengths[i]>0) {
        grid(ny=0,nx=NULL)
        segments(q05[[i]], -d, q95[[i]], -d, col=cols[i],lwd=2, lend=1)
        segments(q25[[i]], -d, q75[[i]], -d, lwd=5, col=cols[i], lend=1)
        points(q50[[i]], -d, pch=16, col=cols[i])
        del <- 0.2/len/2
        lines(rep(q50[[i]],2),-d+c(-1,1)*del,col=cols[i],lwd=2)
        text(q50[[i]], -d-del, labels=signif(q50[[i]],digits=5), pos=1)
      }
      if(maxl>1 & lengths[i]>0) {
        grid(nx=0,ny=NULL)
        segments(xvals+d, q05[[i]], xvals+d, q95[[i]], col=cols[i])
        segments(xvals+d, q25[[i]], xvals+d, q75[[i]], lwd=3, col=cols[i])
        points(xvals+d, q50[[i]], pch=16, col=cols[i])
      }
    }
    legend("topright",legend=names(postlist),col=cols,lwd=3)
  }
}
