## This pair of functions allows the user to define a matrix
##    and either calculate its inverse
##    or retrieve the inverse if it has been previously calculated


## makeCacheMatrix is a list of functions to manipulate a cached matrix and its inverse
## a <- makeCacheMatrix(yourmatrixhere) allows you to call the functions:
## a$get() retrieves the matrix
## a$set() changes the cached matrix to the matrix in the argument
## a$getinv() retrieves the inverse matrix
## a$setinv() sets the inverse of the matrix
## Note that setinv() doesn't actually calculate the inverse
##    it just sets the inverse to the given value

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
          setinv = setinv, getinv = getinv)
}


## cacheSolve will return the inverse of the matrix
## If the inverse is already in the cache it will just return the value
##    with the message saying so
## If it hasn't yet been calculated it will calculate the inverse and return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
            message("Getting cached inverse")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
