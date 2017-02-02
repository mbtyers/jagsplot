# jagsplot

Diagnostic and inferential plots for R2jags output.  Possibly works with rjags output, but I don't know yet.

Highlights are `jags.trace()`, `jags.dens()`, and `jags.hist()`.  All use the same basic syntax, with argument `which.param` selecting the indices of the parameters to plot.  The order of parameters can be found by calling `jags.names()`.