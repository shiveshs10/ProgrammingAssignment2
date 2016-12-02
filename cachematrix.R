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

cacheSolve <- function(res, ...) {
  inv <- res$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data !!")
    return(inv)
  } else {
    message("Cached data not available. computing inverse matrix !!")
  }
  mat <- res$get()
  inv <- solve(mat, ...)
  res$setinverse(inv)
  inv
}
