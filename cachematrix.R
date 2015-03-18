## The goal of this program is to take the costly computation of matrix
## inversion and cache it- allowing for better performance.

## makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m   <- NULL
    # Create set and get methods
    set <-function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x

    # Create methods to set and get the inverse of 'x'
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m

    # return list of methods
    list(set = set, get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

        # Check if inverse of x has been cached
        m <- x$get_inverse()
        if ( !is.null(m) ) {
            message("Getting cached data..")
            return(m)
        }

        my_matrix <- x$get()

        # Solve for the retrieved matrix
        m <- solve(my_matrix)

        # Set the inverse of the result
        x$set_inverse(m)
        # return
        m
}

