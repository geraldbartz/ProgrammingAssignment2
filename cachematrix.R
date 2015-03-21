## Create a class of enhanced matrices which are able to cache its inverses.

## Creates environment which represents an enhanced matrix class
## Is able to hold its inverse in cache so it has not to be recomputed

makeCacheMatrix <- function(m = matrix()) {
        print("inv")
        if (exists("inv")){print(inv)}
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'm'
## Writes inverses into environment of input CacheMatrix

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        inv <- m$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv
}
