## The next fuctions allow to compute the Inverse of a Matrix
## and to store the result in Cache Memory, in such a way that
## if its required to obtain again the Inverse of the same Matrix, 
## this can be done just extracting the stored result, without 
## using machine resourses to compute the Inverse of the Matrix again.
## These functions are specially handy when working with iterative
## processes that compute the Inverse of Large Matrices.

## makeCacheMatrix is a function that creates a list containing
##four different functions: 
## 1. set: Change the stored matrix
## 2. get: Obtains the stored matrix
## 3. setInvMatrix: Sets the Inverse Matrix.
## 4. getInvMatrix: Obtains the Inverse Matrix
## These functions are used to store/obtain the Matrix and 
## Inverse in/from Cache.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(InvMatrix) m <<- InvMatrix
        getInvMatrix <- function() m
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## CacheSolve computes the Inverse of the Matrix Stored in the previous 
## function. If the Inverse was already calculated before, CachesSolve
## extracts The stored Inverse Matrix. If not, the Inverse is calculated
## and stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMatrix(m)
        m
}
