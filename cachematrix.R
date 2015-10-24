##Below a pair of functions is to cache the inverse of a matrix
## Use solve function to complete the matrix inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinv <- function(solve) m <<- solve
        getinv <- function()m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes and return the inverse of the special "matrix" returned by makeCacheMatrix above

## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache

# If the inverse matrix is null, pass matrix "X" to "data" and cache "m" 

cacheSolve <- function(x, ...) {
        
        m <-x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
