# Squaring elements of a given vector

square_for <- function(x){
  # [ToDo] Use the for loop
  for(i in 1:length(x)) x[i] <- x[i]^2
  return(x)
}

square_sapply <- function(x){
  # [ToDo] Use the sapply function
  sapply(x, function(y) y^2)
}

square_vec <- function(x){
  # [ToDo] Use power(^) function in vector form
  x^2
}

square_vec2 <- function(x){
  # [ToDo] Use multiplication(*) function in vector form
  x*x
}

# [ToDo] Create a vector x of size 100,000 of normal variables
x <- rnorm(100000)

# [ToDo] Verify that all functions return the same output
as.numeric(crossprod(square_for(x) - square_sapply(x)))
as.numeric(crossprod(square_vec(x) - square_vec2(x)))

z1 <- square_for(x)
z2 <- square_sapply(x)
z3 <- square_vec(x)
z4 <- square_vec2(x)

identical(z1, z2)
identical(z1, z3)
identical(z1, z4)

# [ToDo] Use microbenchmark package to compare three functions in terms of speed
library(microbenchmark)

microbenchmark(square_for(x), square_sapply(x), square_vec(x), square_vec2(x))
