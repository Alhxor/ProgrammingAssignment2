# makeCacheMatrix() creates a special matrix object
# that can cache its inverse
# cacheSolve() either calculates and returns that inverse,
# or gets it from cache and returns it

# creates an list with 4 methods: get, set, getinv, setinv
# containing matrix and its inverse that can be cached
# by using cacheSolve()
makeCacheMatrix <- function(x = matrix(runif(25), 5, 5)) {
    inv <- NULL # initializing inverse
    set <- function(y) { # function to assign a new matrix
        x <<- y
        inv <<- NULL # removing calculated inverse
    }
    get <- function() x # getting stored matrix
    setinv <- function(inverse) inv <<- inverse # setting new inverse
    getinv <- function() inv # getting stored inverse
    list(set = set, get = get, # returning a list with 4 functions
         setinv = setinv,
         getinv = getinv)
}

# receives a list, created with makeCacheMatrix
# produces an inverse of that matrix and caches it for quick access
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) { # inverse was cached
        message("Getting inverse from cache")
        return(inv)
    }
    # calculating new inverse
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}
