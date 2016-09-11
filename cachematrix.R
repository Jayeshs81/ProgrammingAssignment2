## makeCacheMatrix returns a matric with get and set functions and the ability 
## to cache its inverse. cacheSolve uses the cacheInverse if available else 
## caches caculated result

## makeCacheMatrix returns a list if set and get functions for the matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 setinv <- function(inverse) <<- inverse
 getinv <- function() inv
 
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function retrieves the inverse if it has been cache else 
## calculates the inverse and caches the results
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  else {
    mt <- x$get()
    inv <- solve(mt)
    x$setinv(inv)
    inv
  }
}
