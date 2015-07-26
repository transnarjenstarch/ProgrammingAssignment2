## Put comments here that give an overall description of what your
## functions do

## takes input matrix and stores it in cache 

makeCacheMatrix <- function(x = matrix()) {
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list( set = set, get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse)
}


## the output object of makeCacheMatrix is input of cacheSolve
## This function first checks to see if inverse of matrix is in cache
## If so, it returns the value of the inverse matrix
## If the inverse is not in the cache, it calculates it, stores it with set_inverse
## and returns the inverse value

cacheSolve <- function(x, ...) {
        i <- x$get_inverse()
            if (!is.null(i)) {
                message('getting cached data')
                return(i)
            }
        data <- x$get()
        i <- solve(data)
        x$set_inverse(i)
        i
}
