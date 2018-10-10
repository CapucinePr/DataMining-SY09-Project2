#Des fonctions pour étudier les données
#calcule le mean par variable
getMean <- function(b, e, x){
  res <- c(rep(0,e));
  for(i in b:e){
    res[i]<- mean(x[,i]);
  }
  return(res[b:e]);
}

#Variance
getVar <- function(b, e, x){
  res <- c(rep(0,e));
  for(i in b:e){
    res[i]<- var(x[,i]);
  }
  return(res[b:e]);
}

#Ecart type
getSd <- function(b, e, x){
  res <- c(rep(0,e));
  for(i in b:e){
    res[i]<- sd(x[,i]);
  }
  return(res[b:e]);
}
