## Caching the Inverse of a Matrix

## Matrix inversion is a costly computation, therefore, when the matrix
## doesn't change, it makes sense to cache the inverse of that matrix and retrieve 
## it when need it from the environment rather than computing it repeatedly.

## The following two functions are used to cache the operation of inversing a matrix.
## NOTE: We assume that the square matrix supplied is always invertible.

## makeCacheMatrix 
## This function creates a special "matrix" object that can cache its inverse.
## This special object is a list of functions required to set the value of the 
## matrix, get the value of the matrix, set the value of the inverse matrix and
## get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- solve(y)
        m <<- NULL
    }
    get <- function() x
    setinvmatrix<- function(mx) m <<- mx
    getinvmatrix <- function() m
    
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)

}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    
    m <- x$getinvmatrix()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setinvmatrix(m)
    ## Return a matrix that is the inverse of 'x'
    m
}

## Example:
## > a <- makeCacheMatrix(matrix(c(1,4,7,34,23,10,0,45,2), 3, 3))
## > cacheSolve(a)
## [,1]          [,2]         [,3]
## [1,] -0.04026311 -0.0067769583  0.152481563
## [2,]  0.03059597  0.0001993223 -0.004484752
## [3,] -0.01205900  0.0227227427 -0.011261710
## > cacheSolve(a)
## getting cached data
## [,1]          [,2]         [,3]
## [1,] -0.04026311 -0.0067769583  0.152481563
## [2,]  0.03059597  0.0001993223 -0.004484752
## [3,] -0.01205900  0.0227227427 -0.011261710
