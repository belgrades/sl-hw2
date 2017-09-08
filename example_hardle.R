# Libraries ---------------------------------------------------------------
require(ggplot2)
library(mgcv)

kernel.smooth = function(x, y, lambda, FUN){
  # Implementation of the Nadaraya–Watson estimator
  # Generalized Additive Models - Page 19
  # Hastie, Trevor; Tibshirani, Robert
  yo = numeric()
  
  for(xi in x){
    yo = c(yo, sum(FUN((xi-x)/lambda)*y)/sum(FUN((xi-x)/lambda)))  
  }
  
  return(yo)
  
}

# Quartic Kernel
quartic.kernel = function(u) (abs(u)<=1)*(15/16)*(1-u^2)^2
epanechnikov.kernel = function(u) (abs(u)<=1)*0.75*(1-u^2)
gaussian.kernel = function(u) (1/sqrt(2*pi))*exp(-(0.5*u^2))

backfitting = function(X, y, lambda, kernel, tol = 1e-6, max_iter = 200){
  n = nrow(X)
  d = ncol(X)
  
  alpha = mean(y)
  f = f = matrix(data = rep(0, n*d), nrow = n, ncol = d)
  
  rss0 = sum((y - alpha - apply(f, 1, sum))^2)
  rss = 0
  
  # Day by day there's a man in a suit that's gonna make you pay
  # For the thoughts that you think and the words they won't let you say
  
  rsss = numeric()
  iter = 0
  
  while(abs(rss0-rss) > rss0*tol && iter <= max_iter){
    rss = rss0
    iter = iter + 1
    print(iter)
    rsss = c(rsss, rss)
    
    for(j in 1:d){
      r_j = y - alpha - apply(f, 1, sum) + f[, j]
      f[, j] = kernel.smooth(X[,j], r_j, lambda, FUN = kernel)
      f[, j] = f[, j] - mean(f[, j])
    }
    rss0 = sum((y - alpha - apply(f, 1, sum))^2)
  }
  
  return(f)  
}

# Example 8.1 Page 216
# Non Parametric and Semiparametric Models
# Wolfgan Hardle, Stefan Sperlich 
# Referenced at SpAM (Hardle et al. 2004) 

X = runif(n = 750000, -2.5, 2.5)
eg2 = mean(X^2)
eg4 = mean(exp(-X))

# Experiment

n = 750
d = 4

X = matrix(runif(n = n*d, -2.5, 2.5), nrow = n, ncol = d)
y = -sin(2*X[, 1]) + X[, 2]^2 - eg2 + X[, 3] + exp(-X[, 4]) - eg4 + rnorm(n) 

lambda = 1

max(y)
f = backfitting(X, y, lambda, kernel = quartic.kernel)

par(mfrow = c(2,2))
for(i in 1:d){
  kernel.output = kernel.smooth(X[, i], y, 1, FUN = quartic.kernel)
  plot(x = X[,i], 
       kernel.output,
       xlim = c(min(X[, i])-0.1, max(X[, i])+0.1),
       ylim = c(min(kernel.output)-1, max(kernel.output)+1),
       xlab = paste(c("x", i), collapse = ""),
       ylab = paste(c("f", i), collapse = ""),
       col = "purple")
  points(x = X[,i], y = y) 
  points(X[order(X[,i]), i],f[order(X[,i]),i],
         type = "l",
         lwd  = 2)
}
