# Squaring elements of a given vector

square_for <- function(x){
  # [ToDo] Use the for loop
  y = x
  for (i in 1:length(x)){
    y[i] = x[i]^2
  }
  return(y)
}

square_sapply <- function(x){
  # [ToDo] Use the sapply function
  
  y <- sapply(x, function(t) t^2)
  return(y)
}

square_vec <- function(x){
  # [ToDo] Use power function in vector form
  
  y = x^2
  
  return(y)
}

square_vec2 <- function(x){
  # [ToDo] Use power function in vector form
  
  y = x * x
  
  return(y)
}

# [ToDo] Create a vector x of size 100,000 of normal variables

# [ToDo] Verify that all functions return the same output

# [ToDo] Use microbenchmark package to compare three functions in terms of speed