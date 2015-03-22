## The following code contains two functions that are used to get the inverse
## of square invertible matrix. However, in order to make things more efficient,
## the inverse is calculated once and stored in the Cache. If the inverse is 
## needed again in the future, it'll be retrieved from the Cache instead
## of calculating it again (this assumes that the original matrix hasn't changed).


## The function makeCacheMatrix creates a list of functions that will be used
## by the cacheSolve function in order to retrieve the inverse from the Cache
## (in case it's been calculated previously) or store the inverse in the Cache
## (in case it's the first time that it's calculated).

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix by retrieving it from
## the cache (if it's already there) and if not, it calculates the inverse, stores
## it in the cache and finally returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
