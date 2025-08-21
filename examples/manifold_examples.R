library(scatterplot3d)
library(Rdimtools)
library(dimRed)

setwd("/Users/alexyoung/Documents/Teaching/Stat 185 - Unsupervised Learning/IntroUL/examples/")
set.seed(1)
# generate helix data
t <- sort(runif(2000, min = 0, max = 15))
helix <- matrix(NA,nrow = length(t), ncol = 3)
for (n in 1:length(t)){
  helix[n,] <- c( cos(t[n]), sin(t[n]) , t[n])/sqrt(2)
}

# generate Swiss roll-shaped data
S <- rep(0,2000)
Swiss <- matrix(NA, nrow = 2000, ncol = 3)
for( n in 1:2000){
  s <- runif(1, min = 3*pi/2, max = 9*pi/2)
  t <- runif(1, min = 0,      max = 15)
  S[n] <- s
  Swiss[n, ] <- c( s*cos(s), t, s*sin(s) )
}

# generate washer in 2d 
N <- 2000
washer <- matrix(NA, nrow =N, ncol = 2)
for (n in 1:N){
  r <- runif(1,min=5,max = 10); 
  a = runif(1,min = 0, max = 2*pi); 
  washer[n,] <- r*c(cos(a),sin(a)) + c(20,0)
}

# generate folded washer in 3D
washer3 <- cbind(washer, washer[,2]^2)

# generate rolled washer in 3D
washer.swiss <- washer3
for (n in 1:dim(washer3)[1] ){
  washer.swiss[n,] <- c( washer[n,1]*cos(washer[n,1]), washer[n,2], washer[n,1]*sin(washer[n,1] )) 
}

write.csv(helix, file = "../data/helix.csv")
write.csv(Swiss, file = "../data/swiss_roll.csv")
write.csv(S, file = "../data/swiss_color.csv")
write.csv(washer3, file = "../data/folded_washer.csv")
write.csv(washer.swiss, file = "../data/rolled_washer.csv")


################### apply manifold learning methods and save results ##########
###############################################################################

helix.iso <- embed(helix, "Isomap", knn = 15, ndim = 1)
helix.lle <- do.lle(helix, ndim = 1, type = c("knn",15))
helix.LA <- do.lapeig(helix, ndim = 1, type = c("knn",15))
# helix.HLLE <- embed(helix, "HLLE", knn = 15) # running in sklearn
write.csv(helix.iso, "../data/helix_isomap.csv")
write.csv(helix.lle, "../data/helix_lle.csv")
write.csv(helix.LA, "../data/helix_lapleig.csv")

swiss.iso <- embed(Swiss, "Isomap", knn = 15, ndim = 2)
swiss.lle <- do.lle(Swiss, ndim = 2, type = c("knn",15))
swiss.LA <- do.lapeig(Swiss, ndim = 2, type = c("knn",15))
swiss.HLLE <- embed(Swiss, "HLLE", knn = 15, ndim = 2)
write.csv(swiss.iso, "../data/swiss_isomap.csv")
write.csv(swiss.lle, "../data/swiss_lle.csv")
write.csv(swiss.LA, "../data/swiss_lapleig.csv")
write.csv(swiss.HLLE, "../data/swiss_hlle.csv")

fold_washer.iso <- embed(washer3, "Isomap", knn = 15, ndim = 2)
fold_washer.lle <- do.lle(washer3, ndim = 2, type = c("knn",15))
fold_washer.LA <- do.lapeig(washer3, ndim = 2, type = c("knn",15))
fold_washer.HLLE <- embed(washer3, "HLLE", knn = 11, ndim = 2)
write.csv(fold_washer.iso, "../data/fold_washer_isomap.csv")
write.csv(fold_washer.lle, "../data/fold_washer_lle.csv")
write.csv(fold_washer.LA, "../data/fold_washer_lapleig.csv")
write.csv(fold_washer.HLLE, "../data/fold_washer_hlle.csv")

roll_washer.iso <- embed(washer.swiss, "Isomap", knn = 15, ndim = 2)
roll_washer.lle <- do.lle(washer.swiss, ndim = 2, type = c("knn",15))
roll_washer.LA <- do.lapeig(washer.swiss, ndim = 2, type = c("knn",15))
roll_washer.HLLE <- embed(washer.swiss, "HLLE", knn = 11, ndim = 2)
write.csv(roll_washer.iso, "../data/roll_washer_isomap.csv")
write.csv(roll_washer.lle, "../data/roll_washer_lle.csv")
write.csv(roll_washer.LA, "../data/roll_washer_lapleig.csv")
write.csv(roll_washer.HLLE, "../data/roll_washer_hlle.csv")

