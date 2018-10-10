setwd("C:/Users/plougue/Desktop/UVs/SY09/Projets/Projet 2")
data <- read.csv("donnees/breastcancer.csv")
N <- dim(data)[1]
n <- N
p <- dim(data)[2] - 2
X <- data[,1:p+1]
z <- data[,p+2]



(norm(as.matrix(X),type="F")-norm(diag(diag(as.matrix(X))),type="F"))/norm(as.matrix(X),type="F")

X.pca <- prcomp(X)
X.proj <- X.pca$x[,1:2]
barplot(X.pca$sdev)



par(pty="s")
par(mfrow=c(1,2))


plot(X.proj, col=z)
plot(X.proj, col=z)

mu_1 <- colMeans(X.proj[z==1,])
var_1 <- var(X.proj[z==1,])
mu_2 <- colMeans(X.proj[z==2,])
var_2 <- var(X.proj[z==2,])

###########################################
########   INDEPENDANCY TEST    ###########
###########################################

var_1 <- var(X[z==1,])
var_2 <- var(X[z==2,])
X_1.pr <- princomp(X[z==1,])
X_2.pr <- princomp(X[z==2,])

cat("Log relative volume 1/2 : ", log(det(var_1)/det(var_2)),
    "\nDimension : ", p,
    "\nRelative average variance 1/2 : ", sum(diag(var_1))/sum(diag(var_2)),
    "\nRelative average precision 1/2 : ", sum(diag(solve(var_2)))/sum(diag(solve(var_1))),
    "\nRelative maximum variance 1/2 : ", max(X_1.pr$sdev)/max(X_2.pr$sdev))

###########################################
########  DRAW CONFIDENCE ELLIPSES ########
###########################################
source("fonctions/draw_ellipse.R")


# 
# 
# norm(var_1 - diag(diag(var_1))) / norm(var_1)
# norm(var_2 - diag(diag(var_2))) / norm(var_2)
# 
# plot(X.proj, col=z)
# draw_ellipse(var_1, mu_1, add=T, confidence=0.95)
# draw_ellipse(var_1, mu_1, add=T, confidence=0.8, col="blue")
# draw_ellipse(var_1, mu_1, add=T, confidence=0.6, col="orange")
# draw_ellipse(var_2, mu_2, add=T, confidence=0.95)
# draw_ellipse(var_2, mu_2, add=T, confidence=0.8, col="blue")
# draw_ellipse(var_2, mu_2, add=T, confidence=0.6, col="orange")
# 
# ##########################################
# ##########################################
# ##########################################
# require(MASS)
# library(e1071)
# PC1 <- X.pca$x[,1]
# PC2 <- X.pca$x[,2]
# data.plan <- data.frame(PC1,PC2,z)
# logit.fit <- glm(z ~ ., data = data.plan)
# w <- logit.fit$coefficients
# intercept <-  w[1]/w[3]
# slope <- w[2]/w[3]
# plot(X.proj,col=z,pch=22+predict(logit.fit,data=data))
# abline(a=intercept,b=slope,col="purple")
# 
# 
# 
# 
# ############################
# ##    AFFICHER LDA       ###
# ############################
# 
# par(mfrow=c(1,1))
# 
# lda.fit <- lda(Z ~ .-X, data = data)
# 
# plot(X.proj,col=z, pch=22+as.integer(predict(lda.fit,data=X)$class))
# w <- t(lda.fit$scaling) %*% X.pca$rotation[,1:2]
# intercept <- 0
# slope <- w[1]/w[2]
# abline(a=intercept,b=slope,col="purple")
# 
# 
# 
# 
# 
# ############################
# ## ANALYSE DES VARIANCES ###
# ############################
# 
# 
# pca.1 <- prcomp(X[z==1,])
# pca.2 <- prcomp(X[z==2,])
# 
# attach(mtcars)
# par(mfrow=c(1,2))
# 
# inertia.1 <- pca.1$sdev/max(pca.1$sdev)
# inertia.2 <- pca.2$sdev/max(pca.2$sdev)
# barplot(inertia.1, col="green")
# barplot(inertia.2, col="red")
# 
# sum(pca.1$sdev)
# sum(pca.2$sdev)
# 
# par(mfrow=c(1,1))
# barplot(inertia.1/inertia.2, col="purple")
# 
# 
