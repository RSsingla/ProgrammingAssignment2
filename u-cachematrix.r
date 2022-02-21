
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        settingInv <- function(inverse) inv <<- inverse
        getttingInv <- function() inv
        list(set = set,
             get = get,
             settingInv = settingInv,
             getttingInv = getttingInv)
}

cacheSolve <- function(x, ...) {
        inv <- x$gettingInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matric <- x$get()
        inv <- solve(matric, ...)
        x$settingInv(inv)
        inv
}