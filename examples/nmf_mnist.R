setwd("/Users/alexyoung/Documents/Teaching/Stat 185 - Unsupervised Learning/IntroUL/examples/")

library(NMF)
library(archetypes)

set.seed(185)
load("../../Datasets/digits.Rdata")
eights <- digits$pixels[digits$labels == 8,][1:100,] 
zero_col <- which(colSums(eights) == 0)
eights_min_zero <- eights[,-zero_col]

mnist_nmf <- nmf(t(eights_min_zero), rank = 1:25, 
                 method = "brunet", nrun = 5)
# pdf("mnist_nmf_summaries.pdf")
par(mfrow = c(1,2))
plot(mnist_nmf, c("residuals","cophentic"))
plot(mnist_nmf, "cophenetic")
# dev.off()


k <- 2
H <- matrix(0,nrow = k, ncol = ncol(eights))
H[,-zero_col] <- t(mnist_nmf$fit$'2'@fit@W)
pdf("../images/mnist_nmf_features.pdf")
par(mfrow = c(k/2,2), mar=c(1,1,1,1))
for (j in 1:k){
  image(matrix(H[j,],nrow = 28),
        xaxt = 'n', yaxt = 'n')
}
dev.off()

k <- 2
mnist_svd <- svd(eights_min_zero, nv = k)
H <- matrix(0,nrow = k, ncol = ncol(eights))
H[,-zero_col] <- t(mnist_svd$v)
# pdf("mnist_svd_features.pdf")
par(mfrow = c(k/2,2),mar=c(1,1,1,1))
for (j in 1:k){
  image(matrix(H[j,],nrow = 28),
        xaxt = 'n', yaxt = 'n')
}
# dev.off()


k = 2
eights_aa <- archetypes(eights_min_zero,k=2)
H <- matrix(0,nrow = k, ncol = ncol(eights))
H[,-zero_col] <- eights_aa$archetypes
pdf("mnist_archetypes_features.pdf")
par(mfrow = c(k/2,2),par(mar=c(1,1,1,1)))
for (j in 1:k){
  image(matrix(H[j,],nrow = 28),
        xaxt = 'n', yaxt = 'n')
}
dev.off()

df <- data.frame(x= mnist_nmf$fit$'2'@fit@H[1,],
                 y= mnist_nmf$fit$'2'@fit@H[2,])
hull <- df %>%
  slice(chull(x, y))
# Define the scatterplot
p <- ggplot(df, aes(x, y)) + geom_point(shape = 21, color = "red") + 
  xlab("") + ylab("") + 
  geom_polygon(data = hull, alpha = 0.5)
p


library(vrnmf)
k<-2
volp <- vol_preprocess(eights_min_zero)
test <- volnmf_main(volp, n.comp = k,n.iter = 200,verbose = F, wvol = 0.5)
H <- matrix(0,nrow = k, ncol = ncol(eights))
H[,-zero_col] <- t(test$C)
par(mar=c(1,1,1,1))
par(mfrow = c(k/2,2))
for (j in 1:k){
  image(matrix(H[j,],nrow = 28),
        xaxt = 'n', yaxt = 'n')
}
