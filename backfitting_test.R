
# Load data ---------------------------------------------------------------

load(file = "data/ore.RData")

# Rename variable because Giulia is so fucking picky ----------------------

X = data.frame(ore$t1, ore$t2)
names(X) = c("x1", "x2")
y = ore$width

# set n and p -------------------------------------------------------------

n = nrow(X)
p = ncol(X)

# Set alpha and F ---------------------------------------------------------

tol = 1e-6
alpha = mean(y)
f = matrix(data = rep(0, n*p), nrow = n, ncol = p)

# Set and rss0 rss -----------------------------------------------------------------

rss0 = sum((y - alpha - apply(f, 1, sum))^2)
rss = 0

# Day by day there's a man in a suit that's gonna make you pay
# For the thoughts that you think and the words they won't let you say

iter = 1
max_iter = 200
rsss = numeric()

# But without practical application, theory soon loses its charm
# J. Verne - From Earth to the Mooooon.

smooth.matrix = function(x, df){
  n = length(x);
  A = matrix(0, n, n);
  for(i in 1:n){
    y = rep(0, n); y[i]=1;
    yi = predict(smooth.spline(x, y, df=df), x)$y;
    #yi = smooth.spline(x, y, df=df)$y;
    A[,i]= yi;
  }
  (A+t(A))/2;
}

while(abs(rss0-rss) > tol && iter <= max_iter){
  rss = rss0
  iter = iter + 1

  for(j in 1:p){
    # Compute ri = yi - alpha - sum rk(xi) for all i, k != j
    r_j = y - alpha - apply(f, 1, sum) + f[, j]
    
    # Apply a smoother to rj on xj to obtain fj
    f[, j] = predict(smooth.spline(x = X[, j], y = r_j), X[, j])$y
    
    # Set fj(x) = fj(x) - mean(fj(xi)) for all i
    f[, j] = f[, j] - mean(f[, j])
  }
  rss0 = sum((y - alpha - apply(f, 1, sum))^2)
  rsss = c(rsss, rss0)
}

# Proving Smoothing Matrix ------------------------------------------------

# We need the vector 
L = smooth.matrix(alpha + apply(f, 1, sum), df = 38)
prueba = L %*% y

L2 = smooth.matrix(y, df = 38)
prueba2 = L2 %*% y

# Proving package for Smoothing Matrix ------------------------------------
library(sfsmisc)
L3 = hatMat(y, df = 38)
prueba3 = L3 %*% apply(X, 1, sum)

rss0 = sum((y - alpha - apply(f, 1, sum))^2)

alpha + apply(f, FUN = sum, MARGIN = 1)

plot(1:length(rsss), rsss, type = "l", lwd = 3, ylim = c(0, rsss[1]+10))
