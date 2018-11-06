## These functions work to take a square matrix input and caculate the inverse. 
## in doing this, they work to cache the pervious result and compare this to 
## the most recent result.  If the results match, the cached result is returned,
## bypassing the need to perform the same calculation agian.

## Create a list of functions to set the matrix value, get the matrix value, set the inverted matrix value
## and get the previously calculated matrix value

makeCacheMatrix <- function(x = matrix()){
     if(nrow(x)!=ncol(x))
     {
          return("Not a valid matrix.")
     }
     else{
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setfun <- function(solve) m <<- solve
     getfun <- function() m
     list(set = set, get = get,
          setfun = setfun,
          getfun = getfun)
}
}

## Calculate the inverse of the matrix list created in the makeCacheMatrix function,
## first checking to see if the inverse has already been calculated.  If it has,
## return the cached value.

cacheSolve <- function(x, ...) {
     m <- x$getfun()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setfun(m)
     m
}

