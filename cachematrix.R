# this function creates a matrix object that can cache its inverse, 
# in reality, this is a list containing four elements; set the value of the vector, get the value of the vector
# set the value of the inverse, get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

# this function computes the inverse of the matrix returned by the previous function, makeCacheMatrix. If the inverse has already been 
# calculated (and the matric has not changed), then the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
