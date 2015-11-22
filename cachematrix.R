# The two functions below are trying to cache-and-retrieve the result of a 
# usually costly operation-calculating the inverse of a matrix. For a very large
# matrix, it will take a long time to compute its inverse, especially if it has 
# to be computed more than once, for example in a loop. If the matrix does not 
# change, it will save time if we cache the inverse of the matrix so that when 
# we need it again, it can be looked up in the cache rather than recomputed. 
# This is taking advantage of the scoping rules of the R language to preserve 
# state inside of an R object.


# This function creates a special "matrix" object that can cache its inverse. It 
# actually is a list containing 4 function to: 1. set the matrix, 2. get the 
# matrix, 3. set the inverse of matrix and 4. get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # The "<<-" operator below is used to assign a value to an object in an
        # environment that is different from the current environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function makeCacheMatrix. However, it first checks to 
# see if the inverse has already been calculated and the matrix has not changed.
# If so, it will use the "getinverse" function to retrieve the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of the
# matrix and sets it in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse of matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}
