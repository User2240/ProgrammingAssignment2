
## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
         x <<- y
         im <<- NULL
  }
  get <- function() x
  setim <- function(solve) im <<- solve
  getim <- function() im
  list(set = set, get = get,
          setim = setim,
          getim = getim)
}


## The cacheSolve function  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {       
  im <- x$getim()
  if(!is.null(im)) {
        message("getting cached data")
        return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setim(im)
  im
}

## Example to run the above functions:
> testmatrix <- matrix(c(1,4,8,7), nrow=2, ncol=2)
> testmatrix
     [,1] [,2]
[1,]    1    8
[2,]    4    7
> testmatrix2 <- makeCacheMatrix(testmatrix)
> cacheSolve(testmatrix2)
      [,1]  [,2]
[1,] -0.28  0.32
[2,]  0.16 -0.04
