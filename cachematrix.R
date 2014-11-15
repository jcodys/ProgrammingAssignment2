## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix, which is really a list
## containing a function to set the value of the matrix,get the value
## of the matrix, set the value of the matrix inverse, and get the
## value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function()x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## This function calculates the inverse of a matrix created with the function 
## above. It first, though, checks to see if the matrix inverse has already
## been calculated. If it has, it retrieves the cached value and skips the
## computation. Otherwise it calculates the matrix inverse and caches it 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)){
          message('getting cached matrix inverse data')
          return(x)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
}
