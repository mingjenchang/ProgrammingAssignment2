
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## function: makeCacheMatrix
## argument: a matrix
## check if the argument is a square, if yes, do inverse and cache

makeCacheMatrix <- function(x = matrix()) {
        ## this step check if x is a square matrix. If yes, do inverse, else give message and set x to NA
        if (length(x[1,]) != length(x[,1])) {
                message("Not a sequre matrix. Try again.")
                x <- matrix(NA)
                return (x)
        }
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## function: cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ## check if x is caculated. If yes, show meesage and return inverse. Else calulate and return inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
