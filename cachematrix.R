## This pair of functions create a matrix for which the inverse can be cached,
## returning the cached value if it has already been calculated and cached, or
## calculating and storing the inverse if it has not.

## makeCacheMatrix assigns matrix values to an object with a cache value for
## inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function (y) {
                x <<-y
                i <<-NULL
        }
        get <- function () x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list (set = set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve checks the inverse value of the object made by makeCacheSolve.
## If null, it calculates inverse, stores it in the object, and returns it. 
## If not null, it returns the value stored in the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
