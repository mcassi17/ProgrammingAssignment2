## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the makeCacheMatric function creates a matrix that can cache it's inverse. It
      ## contains a set function, a get function, a setinverse function, and a
      ## getinverse function.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(solve) m <<- solve
      
      getinverse <- function() m
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
            
}


## Write a short comment describing this function
## The cacheSolve function that computes the inverse of the matrix that was returned in
      ## in the makeCacheMatrix function.  The function will return the cached inverse
      ## of the matrix if it has already been calculated.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()
      
      m <- solve(data, ...)
      
      x$setinverse(m)
      
      m  
}
