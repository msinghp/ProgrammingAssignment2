## This programming exercise illustrates the concepts of memoization.
## In memoization the results of expensive computations are cached.
## When the same inputs are encountered, the results are returned
## from the cache. In th is exercise, the results of calculating the
## inverse matrix are cached to be used when the same input matrix
## is encountered again
##
## Function Description


## The makeCacheMatrix`function creates a special "matrix" object that ## caches its inverse
##
## The cacheSolve function computes the inverse of thematrix returned ## by `makeCacheMatrix` function. 
## If the inverse is already calculated  then `cacheSolve` retrieves ## the inverse from the cache.

## Returns a special matrix capable of caching it's inverse
##
makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Returns cached matrix inverse using previously computed matrix inverse
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		 m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}

