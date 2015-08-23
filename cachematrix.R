##Below are two functions that are used to create a special object 
##that stores a matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(ginv) m <<- ginv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}

##The following function calculates the inverse of the special "martix" created 
##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets 
##the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getinv()
    if(!is.null(invs)) {
      message("getting cached data")
      return(invs)
    }
    data <- x$get()
    invs <- ginv(data, ...)
    x$setinv(invs)
    invs
  
}
