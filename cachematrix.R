
## Function 1: this function creates a "matrix" object ("m") that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  
  # set up empty matrix for the imverse (set to NuLL)
    matrixinv <- NULL
  
  # get and set functions for the matrix 
  set <- function(matrixfunction) {
    m <<- matrixfunction
    matrixinv <<- NULL
  }
  get <- function() m
  
  # set and get for the  matrix inverse
  setInverse <- function(inverse) matrixinv <<- inverse
  getInverse <- function() matrixinv
  
    # return a list of get and set functions
    list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function 2: this function now computes the inverse of "m" (the matrix we made with the makeCacheMatrix function above). 
## If the matrix inverse has already been calculated and the matrix hasn't changed, then the matrix inverse will be retrieved from the cache.

## Return a matrix that is the inverse of "m" called "matrixinv"
cacheSolve <- function(m, ...) {
  
  # get the saved inverse 
    matrixinv <- m$getInverse()
  
  # if it exists, then return:
    if (!is.null(matrixinv)) {
    message("getting cached data")
    return(matrixinv)
  }
  
  # if it doesn't exist, then calculate, store and return: 
  mat <- m$get()
  matrixinv <- solve(mat, ...)
  m$setInverse(matrixinv)
  matrixinv
}


## Testing the function:

# generate a matrix with 9 random numbers between whose values range between 1 and 10
matrixA <- makeCacheMatrix(matrix(runif(9, 1, 10), ncol=3, nrow=3))
matrixA$get()

# create the cache
cacheSolve(matrixA)

# retrieve the inverse 
matrixA$getInverse()
