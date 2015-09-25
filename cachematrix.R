## The two functions calculates the inverse of the matrix given as a parameter.
## After being computed first time, the inverse matrix will be cached.
## If the inverse of the same martix will be computed at a later time,
## the inverse will be retrieved from cache. 
## The function makeCacheMatrix creates "the cache matrix".

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL 
       ## set function will store the matrix to the Cache
       set <- function(y) {
	  x <<- y
	  m <<- NULL 
      }
       ## get function retrieve the  matrix from Cache
      get <- function() x 

      ## setInverse function stores the inverse of the matrix
      setInverse <- function(inverse) m <<- inverse 
      
      ## getInverse function retrieves the inverse of the matrix

      getInverse <- function() m
      
      list(set = set, get = get,
	       setInverse = setInverse,
	       getInverse = getInverse)
}

## The function cacheSolve will create the inverse of the matrix -provided as parameter x.
## First, the cache is checked, by using function above.
## If the results is there, the inverse is retrieved from cache.
## If not, the inverse is calculated. The result will be stores in the cache.

cacheSolve <- function(x, ...) {
        ## m function retrieves the inverse from cache if it is found there.
m <- x$getInverse() 
      if(!is.null(m)) { 
	  message("getting cached inverse")
	  return(m)
      }
      ## if the inverse was not find in the cache is computed, then it is stored in the cache.
      message("getting the inverse first time i.e. uncached")
      data <- x$get() 
      m <- solve(data) 
      x$setInverse(m) 
      m 
}

