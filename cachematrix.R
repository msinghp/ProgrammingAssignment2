
## Function Description


## The makeCacheMatrix`function creates a special "matrix" object that caches its inverse
## The cacheSolve function computes the inverse of thematrix returned by `makeCacheMatrix` function. 
## If the inverse is already calculated  then `cacheSolve` retrieves the inverse from the cache.

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

