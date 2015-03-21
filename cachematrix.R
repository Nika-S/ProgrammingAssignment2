## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes   the inverse of this matrix (or retrieves, if it had been calculated
## previosly


## This function creates a special "matrix" object that can cache its inverse
## the argument x should be an invertable square matrix 
## initialization example:#  n<-makeCacheMatrix( matrix(c(1, 3, 2, 4),nrow=2,ncol=2))

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL #Inverse of the matrix x is not known until it is set by setsolve method 
                    # or  calculated by the function cacheSolve()

        # 'set' method below is used to set new data list instead of the one which was set by creating object
        set <- function(y) {
                x <<- y       # reset original matrix x by y, which is provided as an argument of the method 'set'
                inv <<- NULL    # as we reset the data, so we need to set inverse to NULL for it, preventing 
                                # keeping old inverse for new data!
        }
        get <- function() x     ## this "method" returns unchanged x matrix
        setsolve <- function(solve) inv <<- solve

        getsolve <- function() inv  # this "method" returns inverse matrix cached previously 
                                    # (or set by setsolve() or NULL if it wasn't set before  

        # return list of "methods" with their values
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
