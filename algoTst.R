require(MASS)
require(ggplot2)
library(caret)
library(e1071)
library(rpart)


repPredLDA <- function(N_essais,X, nz){
min <- 1
max <- 0
err <- 0
res <- list()
for(i in 1:N_essais)
{
  tst <- sample(2, nrow(X), replace = T, prob = c(0.8, 0.2))
  X.app <- X[tst == 1,]
  X.tst <- X[tst == 2,]
  
  
  fit <- lda(Z~.-X, X.app)
  pred <- predict(fit, newdata=X.tst[,-nz])
  val <- mean(pred$class!=X.tst[,nz])
  if (val<min) {min <- val}
  if(val>max) {max <- val}
  err <- err + val/N_essais
}
res$err <- err
res$min <- min
res$max <- max
return(res)
}


repPredQDA <- function(N_essais,X, nz){
  min <- 1
  max <- 0
  err <- 0
  res <- list()
  for(i in 1:N_essais)
  {
    tst <- sample(2, nrow(X), replace = T, prob = c(0.8, 0.2))
    X.app <- X[tst == 1,]
    X.tst <- X[tst == 2,]
    
    
    fit <- qda(Z~.-X, X.app)
    pred <- predict(fit, newdata=X.tst[,-nz])
    val <- mean(pred$class!=X.tst[,nz])
    if (val<min) {
      min <- val
    }
    if(val>max) {max <- val}
    err <- err + val/N_essais
  }
  res$err <- err
  res$min <- min
  res$max <- max
  return(res)
}


#faire as.factor() avant
repPredNBC <- function(N_essais,X, nz){
  min <- 1
  max <- 0
  err <- 0
  res <- list()
  for(i in 1:N_essais)
  {
    tst <- sample(2, nrow(X), replace = T, prob = c(0.8, 0.2))
    X.app <- X[tst == 1,]
    X.tst <- X[tst == 2,]
    
    
    fit <- naiveBayes(Z ~ . -X, data = X.app)
    pred <- predict(object=fit, newdata=X.tst)
    val <- mean(pred!=X.tst[,nz])
    if (val<min) {min <- val}
    if(val>max) {max <- val}
    err <- err + val/N_essais
  }
  res$err <- err
  res$min <- min
  res$max <- max
  return(res)
}

repPredRL <- function(N_essais,X, nz){
  min <- 1
  max <- 0
  err <- 0
  res <- list()
  for(i in 1:N_essais)
  {
    tst <- sample(2, nrow(X), replace = T, prob = c(0.8, 0.2))
    X.app <- X[tst == 1,]
    X.tst <- X[tst == 2,]
    
    
    fit <- train(Z ~ .-X, data = X.app,method="glm")
    pred <- predict(object=fit, newdata=X.tst)
    val <- mean(pred!=X.tst[,nz])
    if (val<min) {min <- val}
    if(val>max) {max <- val}
    err <- err + val/N_essais
  }
  res$err <- err
  res$min <- min
  res$max <- max
  return(res)
}

repPredBT <- function(N_essais,X, nz){
  min <- 1
  max <- 0
  err <- 0
  res <- list()
  for(i in 1:N_essais)
  {
    tst <- sample(2, nrow(X), replace = T, prob = c(0.8, 0.2))
    X.app <- X[tst == 1,]
    X.tst <- X[tst == 2,]
    
    
    fit <- rpart(Z ~. -X, method="class", data=X.app)
    #prune <- prune(prune, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
    pred <- predict(fit,newdata=X.tst, type="class")
    val <- mean(pred!=X.tst[,nz])
    if (val<min) {min <- val}
    if(val>max) {max <- val}
    err <- err + val/N_essais
  }
  res$err <- err
  res$min <- min
  res$max <- max
  return(res)
}

#test with breastcancer data
#If p> 0.05, normality can be assumed.
bc.Z1 <- bc[bc$Z==1,]
bc.Z2 <- bc[bc$Z==2,]
lshap.bc1 <- lapply(bc.Z1[,-c(1,32)], shapiro.test)
lres.bc1 <- sapply(lshap.bc1, `[`, c("p.value"))
group1 <- mean(lres.bc1[[1]])

tot <- 0
for(i in 1:length(lres.bc1)){
  tot <- tot + lres.bc1[[i]]
}
group1 <- tot / length(lres.bc1)

lshap.bc2 <- lapply(bc.Z2[,-c(1,32)], shapiro.test)
lres.bc2 <- sapply(lshap.bc2, `[`, c("p.value"))
tot <- 0
for(i in 1:length(lres.bc2)){
  tot <- tot + lres.bc2[[i]]
}
group2 <- tot / length(lres.bc2)

pvaluemoyenne <- (group1 + group2 )/2


#test with ionosphere data
iono.Z1 <- iono[iono$Z==1,]
iono.Z2 <- iono[iono$Z==2,]
lsharp.iono1 <- lapply(iono.Z1[,-c(1,3,36)], shapiro.test)
lres.iono1 <- sapply(lsharp.iono1, `[`, c("p.value"))
tot <- 0
for(i in 1:length(lres.iono1)){
  tot <- tot + lres.iono1[[i]]
}
group1 <- tot / length(lres.iono1)

lsharp.iono2 <- lapply(iono.Z2[,-c(1,3,36)], shapiro.test)
lres.iono2 <- sapply(lsharp.iono2, `[`, c("p.value"))
tot <- 0
for(i in 1:length(lres.iono2)){
  tot <- tot + lres.iono2[[i]]
}
group2 <- tot / length(lres.iono2)

pvaluemoyenne <- (group1 + group2 )/2

#test with spam data
lsharp.spam <- lapply(spam[,-c(1,62)], shapiro.test)
lres.spam <- sapply(lsharp.spam, `[`, c("p.value"))
mean(lres.spam[[1]])

lsharp.spam<- lapply(spam[,-c(1,62)], shapiro.test)
lres.spam <- sapply(lsharp.spam, `[`, c("p.value"))
mean(lres.spam[[1]])


tot <- 0
min <- 1
max <- 0
for(i in 1:length(lres.bc)){
  tot <- tot + lres.bc[[i]]
  if(lres.bc[[i]]>max) max <- lres.bc[[i]]
  if(lres.bc[[i]]<min) min <- lres.bc[[i]]
}
tot <- tot / length(lres.bc)

tot <- 0
min <- 1
max <- 0
for(i in 1:length(lres.spam)){
  tot <- tot + lres.spam[[i]]
  if(lres.spam[[i]]>max) max <- lres.spam[[i]]
  if(lres.spam[[i]]<min) min <- lres.spam[[i]]
}
tot <- tot / length(lres.spam)

spam.Z1 <- spam[spam$Z==1,]
spam.Z2 <- spam[spam$Z==2,]
lshap.spam1 <- lapply(spam.Z1[,-c(1,62)], shapiro.test)
lres.spam1 <- sapply(lshap.spam1, `[`, c("p.value"))
group1 <- mean(lres.spam1[[1]])

tot <- 0
for(i in 1:length(lres.spam1)){
  tot <- tot + lres.spam1[[i]]
}
group1 <- tot / length(lres.spam1)

lshap.spam2 <- lapply(sonar.Z2[,-c(1,62)], shapiro.test)
lres.sonar2 <- sapply(lshap.sonar2, `[`, c("p.value"))
tot <- 0
for(i in 1:length(lres.sonar2)){
  tot <- tot + lres.sonar2[[i]]
}
group2 <- tot / length(lres.sonar2)

pvaluemoyenne <- (group1 + group2 )/2

spam.Z1 <- spam[spam$Z==1,]
spam.Z2 <- spam[spam$Z==2,]
lshap.spam1 <- lapply(spam.Z1[,-c(1,59)], shapiro.test)
lres.spam1 <- sapply(lshap.spam1, `[`, c("p.value"))
group1 <- mean(lres.spam1[[1]])

tot <- 0
for(i in 1:length(lres.spam1)){
  tot <- tot + lres.spam1[[i]]
}
group1 <- tot / length(lres.spam1)

lshap.spam2 <- lapply(spam.Z2[,-c(1,59)], shapiro.test)
lres.spam2 <- sapply(lshap.spam2, `[`, c("p.value"))
tot <- 0
for(i in 1:length(lres.spam2)){
  tot <- tot + lres.spam2[[i]]
}
group2 <- tot / length(lres.spam2)

pvaluemoyenne <- (group1 + group2 )/2


#test with spam2 data
spam2.Z1 <- spam2[spam2$Z==1,]
spam2.Z2 <- spam2[spam2$Z==2,]
lshap.spam21 <- lapply(spam2.Z1[,-c(1,59)], shapiro.test)
lres.spam21 <- sapply(lshap.spam21, `[`, c("p.value"))
tot <- 0
for(i in 1:length(lres.spam21)){
  tot <- tot + lres.spam21[[i]]
}
group1 <- tot / length(lres.spam21)

lshap.spam22 <- lapply(spam2.Z2[,-c(1,59)], shapiro.test)
lres.spam22 <- sapply(lshap.spam22, `[`, c("p.value"))
tot <- 0
for(i in 1:length(lres.spam22)){
  tot <- tot + lres.spam22[[i]]
}
group2 <- tot / length(lres.spam22)

pvaluemoyenne <- (group1 + group2 )/2

abs((dim(spam2.Z1)[1]-dim(spam2.Z2)[1])/(dim(spam2.Z1)[1] +dim(spam2.Z2)[1] ))