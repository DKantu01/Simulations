generateSkewedData <- function(n, shape, rate, inverse){
  #value <- rgamma(n, shape, rate)
  #if(inverse) value = max(value) + 0.1 - value
  #costPerHect <- runif(n, 80, 120)
  #cost <- value * costPerHect
  #return(data.frame(cbind(value, cost)))
  na <- 6
  na1 <- na+1
  shape1 <- shape
  shape2 <- shape + 1
  shape3 <- shape + 2
  shape4 <- shape + 3
  shape5 <- shape + 4
  shape6 <- shape + 5
  rate1 <- rate
  rate2 <- rate + 1
  rate3 <- rate + 2
  rate4 <- rate + 3
  rate5 <- rate + 4
  rate6 <- rate + 5
  
  #modified so it has the same mean:
  x1 <- rgamma(n,shape1,rate1)
  x2 <- rgamma(n,shape2,rate2)
  x3 <- rgamma(n,shape3,rate3)
  x4 <- rgamma(n,shape4,rate4)
  x5 <- rgamma(n,shape5,rate5)
  x6 <- rgamma(n,shape6,rate6)
  # x1 <- rgamma(n,shape,rate)
  # x2 <- rgamma(n,shape,rate)
  # x3 <- rgamma(n,shape,rate)
  # x4 <- rgamma(n,shape,rate)
  # x5 <- rgamma(n,shape,rate)
  # x6 <- rgamma(n,shape,rate)
  if(!inverse){
    value1 <- runif(n,5,20) + (x1-mean(x1))
    value2 <- runif(n,5,20) + (x2-mean(x2))
    value3 <- runif(n,5,20) + (x3-mean(x3))
    value4 <- runif(n,5,20) + (x4-mean(x4))
    value5 <- runif(n,5,20) + (x5-mean(x5))
    value6 <- runif(n,5,20) + (x6-mean(x6))
    }
  if(inverse) {
    value1 <- runif(n,5,20) - (x1-mean(x1))
    value2 <- runif(n,5,20) - (x2-mean(x2))
    value3 <- runif(n,5,20) - (x3-mean(x3))
    value4 <- runif(n,5,20) - (x4-mean(x4))
    value5 <- runif(n,5,20) - (x5-mean(x5))
    value6 <- runif(n,5,20) - (x6-mean(x6))
  }
  
  value<-cbind(value1,value2,value3,value4,value5,value6)
  value0<-apply(value,1,mean)
  costPerHect <- runif(n, 80, 120)
  cost <- value0 * costPerHect
  valuec<-cbind(value1,value2,value3,value4,value5,value6,cost)
  results<-array(valuec,dim = c(n,na1))
  return(results)
}

generateUniformData <- function(n, min, max){
  na <- 6
  na1 <- na+1
  # min1 <- min
  # min2 <- min + 1
  # min3 <- min + 2
  # min4 <- min + 3
  # min5 <- min + 4
  # min6 <- min + 5
  # max1 <- max
  # max2 <- max + 1
  # max3 <- max + 2
  # max4 <- max + 3
  # max5 <- max + 4
  # max6 <- max + 5
  value1 <- runif(n, min, max)
  value2 <- runif(n, min, max)
  value3 <- runif(n, min, max)
  value4 <- runif(n, min, max)
  value5 <- runif(n, min, max)
  value6 <- runif(n, min, max)
  value<-cbind(value1,value2,value3,value4,value5,value6)
  value0<-apply(value,1,mean)
  costPerHect <- runif(n, 80, 120)
  cost <- value0 * costPerHect
  valuec<-cbind(value1,value2,value3,value4,value5,value6,cost)
  results<-array(valuec,dim = c(n,na1))
  return(results)
}



