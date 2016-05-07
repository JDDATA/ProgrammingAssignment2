## The following two functions will calculate the inverse matrix or retrieve the inverse matrix from the cache
## The "makeCacheMatrix" function allows caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
  IV <- NULL
  set <- function(y) {
    x <<- y
    IV <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) IV <<- inverse
  getInverse <- function() IV
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Write a short comment describing this function: computes the inverse of the special “matrix” (which is the input of cachemean) returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       IV <- x$getInverse()
       if(!is.null(IV)){
         message("getting cached data")
         return(IV)
       }
       mat <- x$get()
       IV <- solve(mat, ...)
       x$setInverse(IV)
       IV
}
