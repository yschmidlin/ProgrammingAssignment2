

# Matrix inversion can be costly in terms of computing time. So, it may be beneficial to caching the 
# inverse of a matrix rather than computing it repeatedly. 
# The two functions presented below are used to cache the inverse of a matrix 
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(z) {
        x <<- z
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of a matrix. The function first checks if
# the inverse has already been computed. If it has been computed, it gets the result and skips 
# the computation. If it has not been computed, then it computes the inverse, sets the value in the 
# cache via setinverse function. This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

#Output script

# x = rbind(c(1,-1/2),c(-1/2,1))
# m = makeCacheMatrix(x)
# m$get()
# cacheSolve(m)
# cacheSolve(m)

