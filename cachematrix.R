## These two functions caculate the inverse of a square, invertible matrix and caches the result

## Create a special matrix object, which contains the functions necessary to cache its inverse

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(y) {     
                m <<- y          
                inv <<- NULL       
        }
        get <- function() m                 
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv             
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Checks if a cached inverted matrix exists and, if not, it calculates it.

cacheSolve <- function(m, ...) {
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
