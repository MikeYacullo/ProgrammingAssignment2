## create a custom matrix object that can cache its own inverse
#   including support functions for storing/fetching cached value

## custom matrix object
makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse to NULL
  i <- NULL
  # setter
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #get matrix object
  get <- function() x
  # getter and setter for inverse
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## check the cache for the inverse
#   return it if we have it
#   if not, calculate it and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # see if we already have it in the cache
        i <- x$getinverse()
        if(!is.null(i)){
          message("getting cached inverse")
          return(i)
        }
        #it's not cached so calculate it
        data <- x$get()
        i <- solve(data,...)
        #now cache it
        x$setinverse(i)
        i
} 
