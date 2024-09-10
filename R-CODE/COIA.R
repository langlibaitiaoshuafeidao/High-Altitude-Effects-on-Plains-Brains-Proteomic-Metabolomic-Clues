library(ade4)
Ross <- read.delim('metaV2.txt', row.names = 1, check.names = FALSE)
Affy <- read.delim('proteinV2.txt', row.names = 1, check.names = FALSE)
classes <-  read.delim('group.txt', row.names = 1, check.names = FALSE)
dudi1 <- dudi.pca(Ross, scale = FALSE, scan = FALSE, nf = 4)
dudi2 <- dudi.pca(Affy, scale = FALSE, scan = FALSE, nf = 4)
summary(dudi1)
summary(dudi2)
all.equal(dudi1$lw, dudi2$lw)
coin1 <- coinertia(dudi1, dudi2, scan = FALSE, nf = 2)
coin1
summary(coin1)
rand_test <- randtest(coin1, nrepet = 999)
rand_test
plot(rand_test)
summary_coin1 <- summary(coin1)
names(coin1)
names(summary_coin1)
coin1$eig
coin1$eig / sum(coin1$eig)
coin1$co
coin1$li
summary_coin1$EigDec
plot(coin1)