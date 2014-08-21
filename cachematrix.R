## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)

makeCacheMatrix <- function(x = matrix()) {

i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- ginv(data, ...)
  x$setinverse(i)
  i
}

#test cases are shown:

#>a = matrix(1:10,2,5)
#> a
#[,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    5    7    9
#[2,]    2    4    6    8   10
#> cacheSolve(makeCacheMatrix(a))
#[,1]  [,2]
#[1,] -0.8  0.70
#[2,] -0.5  0.45
#[3,] -0.2  0.20
#[4,]  0.1 -0.05
#[5,]  0.4 -0.30

#>b = matrix(1:4,2,2)
#> b
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(makeCacheMatrix(b))
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
