## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(m) {
                mat <<- m
                inv <<- NULL
        }
        get <- function() mat
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inv <- mat$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrice <- mat$get()
        inv <- solve(matrice, ...)
        mat$setInverse(inv)
        inv
}
