## These two functions caculate the inverse of a square, invertible matrix and cache the result. If the same
## matrix is inverted again, R returns the cached result instead of computing the result again.

## MakeCacheMatirx creates a special "matrix", which  is really a list of the functions to 
## set and get the matrix contents (set, get) and its inverse (setinverse, getinverse)

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


## cacheSolve calculates the inverse of the special "matrix" created qith makeCacheMatrix.
## If a cached inverted matrix already exists, then it gets its value from the cache instead than computing it again

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
