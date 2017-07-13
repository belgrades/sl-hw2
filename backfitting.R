
# Load data ---------------------------------------------------------------

# Generating training data
n = 150; p = 200
X = 0.5*matrix(runif(n*p),n,p) + matrix(rep(0.5*runif(n),p),n,p)
# Generating response
y = -2*sin(X[,1]) + X[,2]^2-1/3 + X[,3]-1/2 + exp(-X[,4]) + exp(-1)-1

# Test data ---------------------------------------------------------------

n = 500; d = 200
X.te = 0.5*matrix(runif(n*d),n,d) + matrix(rep(0.5*runif(n),d),n,d)
# Generating response
y.te = -2*sin(X.te[,1]) + X.te[,2]^2-1/3 + X.te[,3]-1/2 + exp(-X.te[,4]) + exp(-1)-1

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

while(abs(rss0-rss) > tol && iter <= max_iter){
  rss = rss0
  iter = iter + 1
  
  for(j in 1:p){
    r_j = y - alpha - apply(f, 1, sum) + f[, j]
    f[, j] = predict(smooth.spline(x = X[, j], y = r_j), X[, j])$y
    f[, j] = f[, j] - mean(f[, j])
  }
  rss0 = sum((y - alpha - apply(f, 1, sum))^2)
  rsss = c(rsss, rss0)
}

rss0 = sum((y - alpha - apply(f, 1, sum))^2)

alpha + apply(f, FUN = sum, MARGIN = 1)

plot(1:length(rsss), rsss, type = "l", lwd = 3, ylim = c(0, rsss[1]+10))
