# Classification rule in discriminant analysis
require(mnormt) # for multivariate normal data generation

# Functions for classification
##############################################
# INPUT
# beta - supplied discriminant vector
# xtrain, ytrain - training data
# xtest, ytest - testing data
#
# OUTPUT
# ypred - predicted class membership
# error - percent of misclassified observations

classify_for <- function(beta, xtrain, ytrain, xtest, ytest){
  # [ToDo] Code discriminant analysis classifier using for loop
  # Calculate sample means based on training data
  xbar1 <- colMeans(xtrain[ytrain == 1, ])
  xbar2 <- colMeans(xtrain[ytrain == 2, ])
  # Calculate class assignments for xtest in a for loop
  ntest <- nrow(xtest)
  ypred <- rep(1, ntest)
  for (i in 1:ntest){
    # Apply h(x) rule to the ith row of xtest
    h1 = as.numeric(crossprod(beta, xtest[i, ] - xbar1)^2)
    h2 = as.numeric(crossprod(beta, xtest[i, ] - xbar2)^2)
    if (h1 > h2){
      ypred[i] <- 2
    }
  }
  # Calculate % error using ytest
  error <- (sum(ytest != ypred) / ntest) * 100
  
  return(list(ypred = ypred, error = error))
}

classify_vec <- function(beta, xtrain, ytrain, xtest, ytest){
  # [ToDo] Try to create vectorized version of classifyV_for
  
  return(list(ypred = ypred, error = error))
}

# Example 
##############################################

# Create model parameters
p <- 10 # dimension
mu1 <- rep(0, p) # mean vector for class 1
mu2 <- rep(1, p) # mean vector for class 2
# equicorrelation covariance matrix with correlation rho
rho <- 0.4
Sigma <- matrix(rho, p, p) + diag(1-rho, p)

# Create training data
n1 <- 100 # number of samples in class 1
n2 <- 100 # number of samples in class 2
ytrain <- c(rep(1, n1), rep(2, n2)) # class assignment
xtrain <- matrix(0, n1 + n2, p)
xtrain[ytrain == 1, ] <- rmnorm(n1, mean = mu1, varcov = Sigma)
xtrain[ytrain == 2, ] <- rmnorm(n2, mean = mu2, varcov = Sigma)

# Create testing data of the same size for simplicity
ytest<- c(rep(1, n1), rep(2, n2)) # class assignment
xtest <- matrix(0, n1 + n2, p)

xtest[ytest == 1, ] <- rmnorm(n1, mean = mu1, varcov = Sigma)
xtest[ytest == 2, ] <- rmnorm(n2, mean = mu2, varcov = Sigma)

# Calculate sample means and within class sample covariance on training data
xbar1 <- colMeans(xtrain[ytrain == 1, ])
xbar2 <- colMeans(xtrain[ytrain == 2, ])
W <- ((n1 - 1) * cov(xtrain[ytrain == 1, ]) + (n2 - 1) * cov(xtrain[ytrain == 2, ]))/(n1 + n2 - 2)

# Calculate the discriminant vector
beta <- solve(W, xbar1 - xbar2)

# [ToDo] Calculate test assignments based on each function

classify_for(beta, xtrain, ytrain, xtest, ytest)

# [ToDo] Verify the assignments agree with each other

# [ToDo] Use microbenchmark package to compare the timing