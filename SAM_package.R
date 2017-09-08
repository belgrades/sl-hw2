library(SAM)
??SAM

# Generate training data 
n = 150 
d = 200
X = 0.5*matrix(runif(n*d),n,d) + matrix(rep(0.5*runif(n),d),n,d)

X = matrix(runif(n*d), n, d)

# generating response 
y = -2*sin(X[,1]) + X[,2]^2-1/3 + X[,3]-1/2 + exp(-X[,4])+exp(-1)-1

plot(X[,1], y)

# Training 
out.trn = samQL(X,y) 
print.samQL(out.trn)
plot.samQL(out.trn)

lambdas = out.trn$lambda
lambdas
fn = out.trn$func_norm
fn
df = data.frame(lambdas = out.trn$lambda, sse = out.trn$sse)

## plotting solution path 
plot(out.trn)
## generating testing data 
nt = 1000 

Xt = 0.5*matrix(runif(nt*d),nt,d) + matrix(rep(0.5*runif(nt),d),nt,d)

yt = -2*sin(Xt[,1]) + Xt[,2]^2-1/3 + Xt[,3]-1/2 + exp(-Xt[,4])+exp(-1)-1

## predicting response 
out.tst = predict(out.trn,Xt)
plot(out.tst, yt)
length(out.tst$values)

out.tst$values
