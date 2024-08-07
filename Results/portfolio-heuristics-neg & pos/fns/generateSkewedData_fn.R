generateSkewedData <- function(n, shape, rate, inverse){
  #value <- rgamma(n, shape, rate)
  #if(inverse) value = max(value) + 0.1 - value
  #costPerHect <- runif(n, 80, 120)
  #cost <- value * costPerHect
  #return(data.frame(cbind(value, cost)))
  
  #modified so it has the same mean:
  x <- rgamma(n,shape,rate)
  if(!inverse){value <- 10 + (x-mean(x))}
  if(inverse) {value <- 10 - (x-mean(x))}
  costPerHect <- runif(n, 80, 120)
  cost <- value * costPerHect
  return(data.frame(cbind(value, cost)))
}

generateUniformData <- function(n, min, max){
  value <- runif(n, min, max)
  costPerHect <- runif(n, 80, 120)
  cost <- value * costPerHect
  return(data.frame(cbind(value, cost)))
}



