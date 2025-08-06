makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() m
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(m, ...) {
    inv <- m$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- m$get()
    inv <- solve(matrix, ...)
    m$setinv(inv)
    inv
}
