## This is the second R programming assignment
## 

## Create the matrix and define the functions list in makeCacheMatrix
## This function does not actually set any data. It simply defines the functions
## That are called in the CacheSolve function later.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y){
        x <<- y
        minv <<- NULL
    }
    # Define the remaining list values to be set later in 'cacheSolve'
    get <- function() x
    setInverse <- function(solvem) minv <<- solvem
    getInverse <- function() minv
    # Now create the list with the functions set:
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This is the function that actually returns the inverse:
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Return the inversed value from x$getInverse() and pass it to minv:
    minv <- x$getInverse()
    ## Check to see if there is anything in x$getInverse(), if its empty, null will be returned
    if(!is.null(minv)){ # Value is not (!) null - double negative here 'not', 'is null' - i.e. it has something...
        message("getting cached data")
        return(inv) # Return the stored (cached) value
    }
    # No value is stored so 'get' the matrix from x
    data <- x$get()
    # Solve the matrix (calculate the inverse):
    minv <- solve(data,...)
    # Now return (set) that the calculated inverse matrix to the list:
    x$setInverse(minv)   # Sets the inversed matrix value (minv) to the list
    return(minv)         # Return the inversed matrix values to the console
}

# Examples
mat<-makeCacheMatrix(matrix(20:23,2,2))
cacheSolve(mat)
cacheSolve(makeCacheMatrix(matrix(20:23,2,2)))
