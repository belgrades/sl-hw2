load(file = "data/ore.RData")
View(ore)

str(ore)
library(mgcv)
par(mfrow = c(2, 2))
ore.gam <- gam(width ~ s(t1) + s(t2), data = ore)
plot(ore$t1, ore$width)
plot(ore$t2, ore$width)
plot(ore.gam, pages = 1)

