## Two functions defined here. Combined, the functions can be used to
## compute the inverse of a matrix.  Once the inverse is found it is
## "cached" and if asked for again later, this cached result is returned
## rather than re-calculating.

## 1) makeCacheMatrix is a function that creates a special "matrix" that 
##    can cache its inverse; actually creates a list of functions to:
##      a - set the value of the matrix
##      b - get the value of the matrix
##      c - set the value of the inverse
##      d - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


## 2) cacheSolve is a function that computes the inverse of a matrix.  But,
##    it checks first to see if the inverse was previously calculated,
##    and if so, it retrieves the value from the cache rather than
##    re-calculating it.  If not, it calculates and caches the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
