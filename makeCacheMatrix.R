#Week 3 Peers Assignment
library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL    #intialize inverse value as NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x #Here we will get matrix X from this functiion
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function(){
          inver <- ginv(x)
          inver%*%x
      }
    list(set = set, get= get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
   if(is.null(inv)){
          message("getting cache data")
          return(inv)
            }
dat <- x$get()
inv <- solve(dat,...)
x$setinverse(inv)
inv
}