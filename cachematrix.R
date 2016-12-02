
## Create and returns four functions namely set,get,setinverse and getinverse.
makeCacheMatrix <- function(x = matrix()) {
  inv_cache <- NULL
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv_cache <<- solve
  getinverse <- function() inv_cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Computes inverse of the matrix if it does not exist in the cache else return the inverse in cache
cacheSolve <- function(res, ...) {
  inv <- res$getinverse()
## Check if inverse is already cached or not
  if(!is.null(inv)) {
    message("Getting cached data !!")
    return(inv)
  } else {
    message("Cached data not available. computing inverse matrix !!")
  }
  mat <- res$get()
  ## Compute Inverse matrix using Solve function
  inv <- solve(mat, ...)
  res$setinverse(inv)
  inv
}
