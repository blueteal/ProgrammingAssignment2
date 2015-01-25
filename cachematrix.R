## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

# Function object to create a stored Matrix and its Inverse
makeCacheMatrix <- function(x = matrix()) {

    # Initialise empty object to store Inverse Matrix
    y <- NULL

    # Set the Matrix and its Inverse in one call
    setall <- function(a) {
        x <<- a
        y <<- solve(x) %*% x
    }

    # Set the Matrix
    setmatrix <- function(a) {
        x <<- a
        y <<- NULL
    }

    # Set the Matrix Inverse
    setmatrixinverse <- function() {
        y <<- solve(x) %*% x
    }

    # Get the List of the Matrix and its Inverse
    getall <- function() c(x, y)

    # Get the original Matrix
    getmatrix <- function() x

    # Get the Matrix Inverse
    getmatrixinverse<- function() y

    # Return list of member functions
    list(setall = setall, getall = getall,
        setmatrix = setmatrix,
        getmatrix = getmatrix,
        setmatrixinverse = setmatrixinverse,
        getmatrixinverse = getmatrixinverse)

}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

    # Test if 'x' is a Cached Matrix Object else return message
    # If 'x' is a Cached Matrix Object assign MI the current value
    if(typeof(x) == "list" && typeof(x$getmatrixinverse) == "closure") {
        mi <- x$getmatrixinverse()
    } else {
        message("Not a Cached Matrix Object")
        return(0)
    }


    # If MI is not NULL, return MI
    if(!is.null(mi)) {
            message("getting cached Matrix Inverse")
            return(mi)
    }

    # If MI is NULL, continue to this line and get original Matrix (MA)
    ma <- x$getmatrix()

    # Find Inverse for MA
    mi <- solve(ma) %*% ma

    # Set MI into special matrix object X for future use
    x$setmatrixinverse(mi)

    # Return Matrix Inverse (MI)
    mi

}
