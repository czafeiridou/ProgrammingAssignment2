## the function computes the inverse of a matrix  


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}



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
