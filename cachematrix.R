## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- null
    set <- function(y){
        x <<- y
        m <<- null
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getmean <- function() i
    list(set = set, get = get,
         setinverse = setinverse
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    message("calculating inverse")
    data <- x$get()
    i <- inverse(data,...)
    x$setinverse(i)
    i
}
