# Libraries ---------------------------------------------------------------
require(ggplot2)
library(mgcv)
library(sfsmisc)

trace = function(S) sum(diag(S))
norma.2 = function(x) sqrt(mean(x^2))

SpAM.backfitting = function(X, y, bandwidth, lambda, kernel, tol = 1e-6, max_iter = 200, gcv = FALSE){
  n = nrow(X)
  d = ncol(X)
  
  alpha = mean(y)
  f = matrix(data = rep(0, n*d), nrow = n, ncol = d)
  
  rss0 = sum((y - alpha - apply(f, 1, sum))^2)
  rss = 0
  
  # Vector of traces
  vj = numeric()
  
  # Day by day there's a man in a suit that's gonna make you pay
  # For the thoughts that you think and the words they won't let you say
  
  rsss = numeric()
  iter = 0
  
  while(abs(rss0-rss) > rss0*tol && iter <= max_iter){
    rss = rss0
    iter = iter + 1
    print(c(iter = iter, rss = rss, lambda = lambda))
    rsss = c(rsss, rss)
    
    for(j in 1:d){
      r_j = y - alpha - apply(f, 1, sum) + f[, j]
      
      f[,j] = ksmooth(X[,j], r_j, bandwidth = 2)$y
      
      lj2 = sqrt(mean(f[, j]^2))
      
      # print(c(lj = 1-lambda/lj2, lj2 = lj2))
      
      f[, j] =ifelse(test = (1-lambda/lj2) < 0, yes = 0, no = (1-lambda/lj2) )*f[, j]
      
      f[, j] = f[, j] - mean(f[, j])
    }
    rss0 = sum((y - alpha - apply(f, 1, sum))^2)
  }
  # Method converged
  
  if(gcv){
    df = 0
    for(i in 1:d){
      if(norma.2(f[,i])>tol){
        S = hatMat(f[, i])
        vi = trace(S)
        df = df + norma.2(f[, i])*vi  
      }
      
    }
    return(mean((y - alpha - apply(f, 1, sum))^2)/(1-df/n)^2)
  }else{
    count = 0
    df = 0
    for(i in 1:d){
      if(norma.2(f[,i])>tol){
        S = hatMat(f[, i])
        vi = trace(S)
        df = df + norma.2(f[, i])*vi
        count = count + 1
      }
      
    }
    print(c(count = count, df = df))
    return(f)
  }
}

# Example 8.1 Page 216
# Non Parametric and Semiparametric Models
# Wolfgan Hardle, Stefan Sperlich 
# Referenced at SpAM (Hardle et al. 2004) 

X = runif(n = 750000, -2.5, 2.5)
eg2 = mean(X^2)
eg4 = mean(exp(-X))

# Experiment

n = 150
d = 200

X = matrix(runif(n = n*d, -2.5, 2.5), nrow = n, ncol = d)
y = -sin(2*X[, 1]) + X[, 2]^2 - eg2 + X[, 3] + exp(-X[, 4]) - eg4 + rnorm(n) 

bandwidth = 2

my.kernel = quartic.kernel

gcv = F

if(gcv){
  gcv.vect = numeric()
  for(i in 1:length(lambdas)){
    gcv.vect[i] = SpAM.backfitting(X, y, bandwidth, lambdas[i], kernel = my.kernel, gcv = gcv)
    print(c(gcv.lambda = lambdas[i], gcv = gcv.vect[i]))
    plot(lambdas[1:i], gcv.vect, main = "Cv per lambda")
  }  
  plot(lambdas, gcv.vect, main = "Cv per lambda")
}else{
  par(mfrow = c(2,2))
  lambda = 0.015
  f = SpAM.backfitting(X, y, bandwidth, lambda, kernel = my.kernel, gcv = gcv)
  for(i in 1:4){
    kernel.output = predict(smooth.spline(X[,i], y), X[,i])$y
    plot(x = X[,i], 
         kernel.output,
         xlim = c(min(X[, i])-0.1, max(X[, i])+0.1),
         ylim = c(min(kernel.output)-0.5, max(kernel.output)+0.5),
         xlab = paste(c("x", i), collapse = ""),
         ylab = paste(c("f", i), collapse = ""),
         col = "purple")
    points(X[order(X[,i]), i],f[order(X[,i]),i],
           type = "l",
           lwd  = 2)
  }
}