p   <- 1000
n   <- 50

set.seed(03875)

X       <- matrix(rnorm(n*p, mean = 10, sd = 3), n, p)
group   <- rep(1:2, each = n/2)

computeT_for <- function(X, group){
  
  Tstats <- rep(0, p)
  
  for(j in 1:p) Tstats[j] <- t.test(X[, j] ~ group)$stat
  
  return(Tstats)
}

computeT_for2 <- function(X, group){
  
  Tstats <- rep(0, p)
  
  for(j in 1:p) Tstats[j] <- t.test(X[group == 1, j], X[group == 2, j])$stat
  
  return(Tstats)
}

library(microbenchmark)
microbenchmark(computeT_for(X, group), computeT_for2(X, group), times = 10) # formula interface is much slower!!!

computeT_for3 <- function(X, group){
  
  Tstats  <- rep(0, p)
  n1      <- sum(group ==1)
  n2      <- sum(group ==2)
  
  for(j in 1:p){
    m1 <- mean(X[group ==1, j])
    m2 <- mean(X[group ==2, j])
    var1 <- var(X[group ==1, j])
    var2 <- var(X[group ==2, j])
    Tstats[j] <- (m1 - m2) / sqrt(var1/n1 + var2/n2)
  }
  
  return(Tstats)
}

library(microbenchmark)
microbenchmark(computeT_for2(X, group), computeT_for3(X, group), times = 10)

computeT_vec <- function(X, group){
  
  Tstats  <- rep(0, p)
  n1      <- sum(group ==1)
  n2      <- sum(group ==2)
  
  m1 <- colMeans(X[group == 1, ])
  m2 <- colMeans(X[group == 2, ])
  var1 <- colSums((X[group == 1, ] - matrix(m1, n1, p, byrow = T))^2) / (n1 - 1)
  var2 <- colSums((X[group == 2, ] - matrix(m2, n2, p, byrow = T))^2) / (n2 - 1)
  
  Tstats <- (m1 - m2) / sqrt(var1/n1 + var2/n2) 
  
  return(Tstats)
}

computeT_vec_mine <- function(X, group){
  
  Tstats  <- rep(0, p)
  n1      <- sum(group ==1)
  n2      <- sum(group ==2)
  
  m1 <- colMeans(X[group == 1, ])
  m2 <- colMeans(X[group == 2, ])
  var1 <- rowSums((t(X[group == 1, ]) - m1)^2) / (n1-1)
  var2 <- rowSums((t(X[group == 2, ]) - m2)^2) / (n2-1)
  
  Tstats <- (m1 - m2) / sqrt(var1/n1 + var2/n2)
   
  return(Tstats)
}

head(computeT_for3(X, group))
head(computeT_vec(X, group))
head(computeT_vec_mine(X, group))

microbenchmark(computeT_for3(X, group), computeT_vec(X, group), computeT_vec_mine(X, group), times = 10)

M <- matrix(c(1,2,3,4), 2, 2)
M - colMeans(M)
t(M) - colMeans(M)


