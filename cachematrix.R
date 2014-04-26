
## These two functions that are used to create a special object that stores a matrix  and caches its inverse.
## It is assumed that the matrix is inversible

##makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set up  the value of the  matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix







makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)             
}


## cacheSolve calculates the inverse of the special "matrix" created with the  makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setXXXXXX function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i  <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve (data, ...)
        x$setinverse(i)
        i
}

