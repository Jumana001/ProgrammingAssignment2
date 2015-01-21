## Matrix inversion is often used by datascientists and can be a costly computation
## particularly if required repeatedly
## Here we will write a pair of functions that will cache the inverse of a matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix is a special vector that is really a list containing several functions:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) Inv <<- inverse
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function calculates the inverse of the matrix 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setInv function.
## For this assignment, we will assume that the matrix supplied 
## is always invertible.


cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}

## Example
## q = cbind (c(1,-2,3), c(-2,1,3), c(3,-1,2))
## s = makeCacheMatrix(q)
## s$get()
##    [,1] [,2] [,3]
## [1,]    1   -2    3
## [2,]   -2    1   -1
## [3,]    3    3    2

## cacheSolve(s)
## No cache value of the inverse of this matrix as yet:
##          [,1]       [,2]       [,3]
## [1,] -0.20833333 -0.5416667 0.04166667
## [2,] -0.04166667  0.2916667 0.20833333
## [3,]  0.37500000  0.3750000 0.12500000

## Running cacheSolve(s) once again:
## cacheSolve(s)
## getting cached data
##             [,1]       [,2]       [,3]
## [1,] -0.20833333 -0.5416667 0.04166667
## [2,] -0.04166667  0.2916667 0.20833333
## [3,]  0.37500000  0.3750000 0.12500000